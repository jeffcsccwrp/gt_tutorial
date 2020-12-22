# GT tutorial by Heili L
# 20201201 JC

library(tidyverse)
library(lubridate)
library(gt)
library(paletteer)
library(scales)

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


pizza_table <- pizza_money %>% 
  filter(month_num > 9) %>% 
  mutate(month_name = case_when(month_num == 10 ~ "October", 
                                month_num == 11 ~ "November",
                                month_num == 12 ~ "December")) %>% 
  gt(groupname_col = "month_name", rowname_col = "type") %>% 
  cols_hide(columns = vars(month_num)) %>% 
  fmt_currency(columns = vars(profit), currency = "USD") %>% 
  tab_header(title = "Q4 Monthly Pizza Sales",
             subtitle = "by JC") %>% 
  tab_source_note(md("More info can be found at `?pizzaplace`")) %>% 
  cols_label(sold = "Pizzas Sold",
             profit = "Profits") %>% 
  tab_stubhead(label = "Month") %>% 
  tab_spanner(label = "Pizzas Sold + Revenue",
              columns = vars(sold, profit)) %>%  #need to refer to the OG name, not renames
  summary_rows(groups = T,
               columns = vars(profit),
               fns = list("TOTAL" = "sum"),
               formatter = fmt_currency, currency = "USD", 
               use_seps = T) %>% 
  grand_summary_rows(
    columns = vars(profit),
    fns = list("GRAND TOTAL" = "sum"), 
    formatter = fmt_currency, currency = "USD",
    use_seps = T) %>% 
  data_color(
    columns = vars(profit),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::teal_material") %>% 
        as.character(),
      domain = NULL),
    alpha = 0.75) 

pizza_table

gtsave(pizza_table, "pizza_table.png")











