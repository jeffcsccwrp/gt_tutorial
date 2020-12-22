# GT tutorial by Heili L
# 20201201 JC

library(tidyverse)
library(lubridate)
library(gt)
library(paletteer)

#look at dataset description
?pizzaplace

pizza <- gt::pizzaplace #double ":" means that its pulling from that package
View(pizza)


#make a new column for month
pizza <- pizza %>% #take OG dataset and mutate date format
  mutate(date_format = as_date(date)) %>% 
  mutate(month_num = month(date_format)) %>% 
  mutate(year_num = year(date_format)) %>% 
  mutate(day_num = day(date_format))

pizza_money <- pizza %>%
  group_by(month_num, type) %>% 
  mutate(type = factor(type, levels = c("classic", "veggie", "chicken", "supreme"))) %>%  #this step relevels pizza types
  summarize(sold = n(), profit = sum(price)) %>% 
  ungroup() #always ungroup data or this will carry over to following code and mess things up

