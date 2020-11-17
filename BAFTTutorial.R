library(tidyverse)
library(forecast)
library(timetk)
library(Metrics)
library(lubridate)
library(sweep)
options(dplyr.print_min = 50)
theme_set(theme_minimal())

#SLIDE 5: Check long format
########################################
#TOP 10 ITEMS FROM WALMART

#load in training set 1-29-2015 to 2015-04-25
train <- read.csv('walmartToyTrain.csv', stringsAsFactors = F)


#### PREPARE TEST SET 2015-04-26 to 2016-04-24
test <- read.csv('walmartToyTest.csv', stringsAsFactors = F)
test <- test %>%
  mutate(date = as.Date(date))

#SLIDE 6: EXAMINE TRAIN/TEST SETS VISUALLY
######################################
#plot original train series
train %>%
  group_by(item_id)%>%
ggplot(aes(as.Date(date), total_daily, color=item_id))+
  geom_line(size=.4)+
  guides(color=F)+
  facet_wrap(~item_id, nrow=5, scales='free_y')+
  labs(x='', title='Walmart Train top 10 items daily unit sales')

#plot original test series
test %>%
  group_by(item_id)%>%
ggplot(aes(as.Date(date), total_daily, color=item_id))+
  geom_line(size=.4)+
  guides(color=F)+
  facet_wrap(~item_id, nrow=5, scales='free_y')+
  labs(x='', title='Walmart Test top 10 items daily unit sales')

#SLIDES 7-10
############## ABC XYZ ANALYSIS 
#3 functions to use: abc(), xyz() and abcxyz()
library(tsutils)

#need wide format: date rows, material columns
wide_df <- train %>%
  tidyr::spread(item_id, total_daily)

#Get % total sales
abc(wide_df %>% select(-date))
abc_plot <- abc(wide_df %>% select(-date))
plot(abc_plot)
class <- t(data.frame(abc_plot$class))
abc_plot$value
results <- cbind(data.frame(mean_value = abc_plot$value), importance = abc_plot$class[1,])
results <- results%>%
  rownames_to_column(var='item_id')


# ABC analysis
#          Importance %
# A (20%): 38.19
# B (30%): 28.561
# C (50%): 33.248

#plot mean values by item
results%>%
  ggplot(aes(reorder(item_id, mean_value), mean_value, fill=importance))+
  geom_col()+
  scale_fill_brewer(type='qual', direction = -1)+
  coord_flip()+
  labs(x='Items')

#XYZ: estimate forecastibility by coefficient of variation:
# sd(series)/mean(series)
xyz(wide_df %>% select(-date), type='cv')
xyz_plot <- xyz(wide_df %>% select(-date), type='cv')
plot(xyz_plot)
xyz_plot$value
xyz_plot$class
# XYZ analysis
#          Errors %
# Z (20%): 36.158
# Y (30%): 32.528
# X (50%): 31.313
xyzresults <- cbind(data.frame(item_id = results$item_id, 
                               cv_value = xyz_plot$value[1,]), 
                    forecastability = xyz_plot$class[1,])

#Plot results
xyzresults%>%
  ggplot(aes(reorder(item_id, cv_value), 
             cv_value, fill=forecastability))+
  geom_col()+
  scale_fill_brewer(type='qual', direction = 1)+
  guides(fill = guide_legend(reverse=T))+
  coord_flip()+
  labs(x='Items')

#Use this to guide your forecasting strategy
x <- abcxyz(abc(wide_df %>% select(-date)),
       xyz(wide_df %>% select(-date), type='cv'))

#PRE-MODELING
#################################################
#set up naive for comparison. final date training period sales: 2015-04-25
naive_pred <- train%>%
  mutate(date = as.Date(date))%>%
  group_by(item_id)%>%
  filter(date == last(date))%>%
  pull(total_daily)

naive_items <- train%>%
  mutate(date = as.Date(date))%>%
  group_by(item_id)%>%
  filter(date == last(date))%>%
  pull(item_id)

#create a 'naive df' of final value repeated 365 days forward
naive_df <- data.frame(forecast = rep(naive_pred, 365),
                       item_id = rep(naive_items, 365))
naive <- naive_df%>%
  group_by(item_id)%>%
  mutate(date = seq.Date(as.Date('2015-04-26'),
                                       by='day', length.out = 365))

#Join test set
naive <- naive %>%
  inner_join(test, by = c('date', 'item_id'))

#calculate forecast error i
naive <- naive %>%
  mutate(error = forecast-total_daily)

#plot error series. You can see jan/dec holiday times bad predictions
naive%>%
  ggplot(aes(date, error, color=item_id))+
  geom_line()+
  guides(color=F)+
  facet_wrap(~item_id, nrow=5)

#CHECK ACCURACY ON TEST SET: 162 RMSE
forecast::accuracy(naive$forecast, naive$total_daily)


#SLIDES 11-12
################### BUILD TS OBJECTS 
#1. CONVERT TO DATE GROUP AND THEN NEST EACH MATERIAL INTO LIST COLUMNS
nest <- train%>%
  mutate(date = ymd(date))%>%
  group_by(item_id)%>%
  dplyr::select(-item_id, total_daily)%>%
  nest(.key= 'dem_df')

#2. FOR EACH LIST COLUMN, CONVERT IT TO TIME SERIES OBJECT
nest_ts <- nest %>%
  mutate(dem_df = 
           map(.x = dem_df,
               .f = tk_ts,
               select = total_daily, #select the outcome col
               start= c(2011,29), #Jan 29th 2011
               #end = c(2020,210),
               deltat= 1/365)) #daily data

#SLIDE 13
#NOW YOU CAN BUILD FORECAST MODELS 
########################################
#auto arima takes some time to run (~2mins) 
#AUTOARIMA. 
ar_models <- nest_ts %>%
  mutate(ar_fit = map(.x=dem_df,
                       .f = auto.arima))


#SLIDE 14: TIDYING UP
#FORECAST 12 months in testing
ar_forecast <- ar_models %>%
  mutate(fcast = map(ar_fit,
                     forecast,
                     h=365))%>%
  mutate(swp = map(fcast, sw_sweep, fitted=FALSE))%>%
  unnest(swp)%>%
  filter(key == 'forecast')%>%
  mutate(dt = seq(from = as.Date('2015-04-26'), by='day', length.out = 365))%>%
  select(item_id, dt, total_daily)
  
#join with actual values in validation
ar_forecast_date <- ar_forecast %>%
  left_join(test, by = c('dt'='date', 'item_id'))


#label your model forecasts for later visualization
ar_forecast_date <- ar_forecast_date %>%
  mutate(model = 'arima')

#SLIDE 15
#CHECK ACCURACY ON TEST SET. x is pred, y is actual. RMSE 122.2
forecast::accuracy(ar_forecast_date$total_daily.y, ar_forecast_date$total_daily.x)
                   
#plot forecasts to verify nothing insane happened
ar_forecast %>%
  group_by(item_id)%>%
ggplot(aes(dt, total_daily, color=item_id))+
  geom_line(size=1)

#####################
#LINEAR REGRESSION FORECAST
##FORECAST 12 months in testing
lm_models <- nest_ts %>%
  mutate(lm_fit = map(.x=dem_df,
                       .f = function(x) tslm(x ~ trend + season)))

lm_forecast <- lm_models %>%
  mutate(fcast = map(lm_fit,
                     forecast,
                     h=365))%>% 
   mutate(swp = map(fcast, sw_sweep, fitted=FALSE))%>%
   unnest(swp)%>%
   filter(key == 'forecast')%>%
   mutate(dt = seq(from = as.Date('2015-04-26'), by='day', length.out = 365))%>%
   select(item_id, dt, value)


#join with actual values in validation
lm_forecast_date <- lm_forecast %>%
  left_join(test, by = c('dt'='date', 'item_id'))

#label your model forecasts for later visualization
lm_forecast_date <- lm_forecast_date %>%
  mutate(model = 'lm')

#CHECK ACCURACY ON TEST SET: RMSE 140.97
forecast::accuracy(lm_forecast_date$total_daily, lm_forecast_date$value) #total_daily = actual, value = forecast value

################# ETS FORECAST
ets_models <- nest_ts %>%
  mutate(ets_fit = map(.x=dem_df,
                       .f = ets))

ets_forecast <- ets_models %>%
  mutate(fcast = map(ets_fit,
                     forecast,
                     h=365))%>% 
  mutate(swp = map(fcast, sw_sweep, fitted=FALSE))%>%
  unnest(swp)%>%
  filter(key == 'forecast')%>%
  mutate(dt = seq(from = as.Date('2015-04-26'), by='day', length.out = 365))%>%
  select(item_id, dt, total_daily)


#join with actual values in validation
ets_forecast_date <- ets_forecast %>%
  left_join(test, by = c('dt'='date', 'item_id'))

#label your model forecasts for later visualization
ets_forecast_date <- ets_forecast_date %>%
  mutate(model = 'ets')

#CHECK ACCURACY ON TEST SET. x is pred, y is actual. RMSE 154.48
forecast::accuracy(ets_forecast_date$total_daily.y, ets_forecast_date$total_daily.x)

################# Seasonal NAIVE FORECAST
naive_models <- nest_ts %>%
  mutate(naive_fit = map(.x=dem_df,
                       .f = snaive))

naive_forecast <- naive_models %>%
  mutate(fcast = map(naive_fit,
                     forecast,
                     h=365))%>% 
  mutate(swp = map(fcast, sw_sweep, fitted=FALSE))%>%
  unnest(swp)%>%
  filter(key == 'forecast')%>%
  mutate(dt = seq(from = as.Date('2015-04-26'), by='day', length.out = 365))%>%
  select(item_id, dt, total_daily)


#join with actual values in validation
naive_forecast_date <- naive_forecast %>%
  left_join(test, by = c('dt'='date', 'item_id'))


#CHECK ACCURACY ON TEST SET. x is pred, y is actual. RMSE 102
forecast::accuracy(naive_forecast_date$total_daily.y, naive_forecast_date$total_daily.x)


#label your model forecasts for later visualization
naive_forecast_date <- naive_forecast_date %>%
  mutate(model = 'snaive')

#SLIDE 16
###############################################################
#LOOK AT PREDICTION ERROR FOR ALL MODELS
#Combine all into one long DF

#change column names from lm model to match others. Value is pred, total_daily is actual
lm_forecast_date <- lm_forecast_date %>%
  mutate(total_daily.x = value,
         total_daily.y = total_daily,
         value = NULL,
         total_daily = NULL)

#Combine all models into one long DF
full_df <- rbind(ets_forecast_date, 
                 lm_forecast_date, 
                 ar_forecast_date, 
                 naive_forecast_date)

full_df <- full_df%>%
  mutate(error = total_daily.x - total_daily.y)


#SLIDE 17
#What can you conclude from the visual here? 
full_df%>%
  ggplot(aes(dt, error, color=model))+
  geom_line()+
  facet_wrap(~item_id, ncol =2, scale='free_y')

#ideally use train/valid/test. ideally refit. just showing you basics!
#now it's your turn 
