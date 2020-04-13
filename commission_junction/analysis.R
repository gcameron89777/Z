
# libraries
library(tidyverse)
library(DBI)
library(odbc)
library(dbplyr)

# postgres con
con <- dbConnect(odbc(), "PostgreSQL ANSI")
con_flagship <- dbConnect(odbc(), "PostgreSQL Flagship")


# read in data from alex
cj_data <- read_csv("cj_2020.csv")
names(cj_data)<-str_replace_all(names(cj_data), c(" " = "_"))


# get transactions data
trans <- con %>% tbl(in_schema("ga_flagship_marketing", "transactions")) %>% 
  select(dimension1, dimension4, transaction_id, transaction_revenue, item_quantity) %>% 
  filter(dimension4 %in% local(cj_data$Order_ID))
sessions <- con %>% tbl(in_schema("ga_flagship_marketing", "sessions")) %>% 
  select(dimension1, date, medium, source, campaign)
ga_data <- trans %>% inner_join(sessions, by = "dimension1") %>% collect()


# Pravardhans flagship data for account status
zf_users <- con_flagship %>% tbl("zfusers") %>% 
  filter(CreatedOn >= '2019-01-01') %>% collect() %>% 
  mutate(UserID = as.character(UserID)) %>% 
  filter(UserID %in% local(cj_data$Order_ID))

# bring together
combined <- cj_data %>% 
  select(Order_ID, Posting_Date, Publisher_Name, Action_Name, Status) %>% 
  left_join(ga_data, by = c("Order_ID" = "dimension4")) %>% 
  left_join(zf_users, by = c("Order_ID" = "UserID"))



# write to csv
write_csv(combined, "cj_combined.csv")


