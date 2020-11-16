library(tidyverse)

#read in data
#100 unique time series. 1 for each item. Top 100 items over all stores
act <- read.csv('100GroupActivity.csv', stringsAsFactors = F)

#IN YOUR GROUP
#Person 1: slice(1:25), person 2: slice(26:50), 
#person 3: slice(51-75), person 4: slice(76-100)
my_items <- act%>%
  group_by(item_id)%>%
  summarize(total_sales = sum(total_daily))%>%
  arrange(desc(total_sales))%>%
  slice(1:25)%>% #change this to match your numbers
  pull(item_id)

#Save your items in a new data frame
my_df <- act%>%
  filter(item_id %in% my_items)

#create train/validation sets
training <- my_df%>%
  filter(date < as.Date('2015-04-26'))

testing <- my_df%>%
  filter(date >= as.Date('2015-04-26'))

#Recreate the analysis from before. Which method(s) performs best? why?
#Good luck! PS: you may need to load other libraries
# PPS: to save RAM you can delete unneeded DFs with rm(act) and gc()
###################################################################################