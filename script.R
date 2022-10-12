library(pacman)
p_load(tidyverse,lubridate,janitor,report)
library(formattable)


## importing and cleaning the columns of the data

customer_churn <- read_csv("~/customer churn/telecom_customer_churn.csv") %>% 
  clean_names()

## shape of the data set
dim(customer_churn)

## data contains 7043 rows and 38 columns

## Understand gender of customers

gender <- customer_churn %>% pull(2)

report(gender)

##2 entries, such as Male (50.48%); Female (49.52%); NA (0 missing)

## understand the age of customers
str(customer_churn)

age<- customer_churn %>% pull(3)

report(age)

##n = 7043, Mean = 46.51, SD = 16.75, Median = 46.00, range: [19, 80], 0% missing

ggplot(data_frame(age),aes(age))+geom_freqpoly()

##undersstand city

city<- customer_churn %>% pull(6)
city <- as.factor(city)
tibble(city) %>% 
  group_by(city) %>% 
  summarise(n=n()) %>% 
  mutate(p =percent(c( n/sum(n,na.rm = T)))) %>% 
  arrange(-n)

## understand number_of_refferals

number_of_refferals <- customer_churn %>% pull(10)

summary(number_of_refferals)

report(number_of_refferals)

## understand tenure_months

tenure_months<- customer_churn %>% pull(11)

report_table(tenure_months)

## understand offer

offers <- customer_churn %>% pull(12)
offers <- factor(offers)
summary(offers)

## understand phone_service
phone_service <- customer_churn %>% pull(13)

phone_service <- as.factor(phone_service)
summary(phone_service)


## understand avg_monthly_long_distance_charges
avg_monthly_long_distance_charges <- customer_churn %>% pull(14)
summary(avg_monthly_long_distance_charges)

ggplot(data_frame(avg_monthly_long_distance_charges),aes(avg_monthly_long_distance_charges))+geom_boxplot()


## understand multiple_lines

multiple_lines <- customer_churn %>% pull(15)
multiple_lines <- factor(multiple_lines)
summary(multiple_lines)

## understand device_protection_plan
device_protection_plan <-  customer_churn %>% pull(16)

device_protection_plan <- factor(device_protection_plan)
summary(device_protection_plan)

## understand internet_type

internet_type <- customer_churn %>% pull(17)
internet_type <- factor(internet_type)
summary(internet_type)

## understand avg_monthly_gb_download

avg_monthly_gb_download <- customer_churn %>% pull(18)

report(avg_monthly_gb_download)

## online_security

online_security <- customer_churn %>% pull(19)
online_security <-  factor(online_security)
report(online_security)
as.report_table(online_security)

## online_backup
online_backup <- customer_churn %>% pull(20)
online_backup <- factor(online_backup)
report(online_backup)

## device_protection_plan

device_protection_plan <- customer_churn %>% pull(21)
device_protection_plan <-  factor(device_protection_plan)
report_table(device_protection_plan)

## #premium_tech_support

premium_tech_support <- customer_churn %>% pull(22)
premium_tech_support <-  factor(premium_tech_support)
report_table(premium_tech_support)

library(report)
## streaming_tv

streaming_tv <- customer_churn %>% pull(23)
streaming_tv <- factor(streaming_tv)
report_table(streaming_tv)

## streaming_movies

streaming_movies <- customer_churn %>% pull(24)

streaming_movies  <- factor(streaming_movies)

report_table(streaming_movies)

## streaming_music
streaming_music <- customer_churn %>% pull(25)
streaming_music <- factor(streaming_music)
report_table(streaming_music)

## unlimited_data
unlimited_data  <- customer_churn %>% pull(26)
unlimited_data <- factor(unlimited_data)
report_table(unlimited_data)

## contract

contract  <- customer_churn %>% pull(27)
contract <-  factor(contract)
report_table(contract)


## paperless_billing

paperless_billing <- customer_churn %>% pull(28)

paperless_billing <- factor(paperless_billing)
report_table(paperless_billing)

##  payment_method

payment_method <- customer_churn %>% pull(29)
payment_method <- factor(payment_method)
report_table(payment_method)


## monthly_charge

monthly_charge <- customer_churn %>% pull(30)
report_table(monthly_charge)


## total_charges
total_charges <- customer_churn %>% pull(31)
report_table(total_charges)

## total_refunds

total_refunds <- customer_churn %>% pull(32)
report_table(total_refunds)

## total_extra_data_charges

total_extra_data_charges <- customer_churn %>% pull(33)
report_table(total_extra_data_charges)

## total_long_distance_charges

total_long_distance_charges <- customer_churn %>% pull(34)
report_table(total_long_distance_charges)


## total_revenue

total_revenue <- customer_churn %>% pull(35)
report_table(total_revenue)

install.packages("gtExtras")
## customer_status

customer_status <- customer_churn %>% pull(36)
customer_status <- factor(customer_status)
report_table(customer_status)

library(report)


library(gt)
##churn_category
churn_category <- customer_churn %>% pull(37)
churn_category <- factor(churn_category)
report_table(churn_category)

##churn_reason
churn_reason <-  customer_churn %>% pull(38)
churn_reason <- factor(churn_reason)
report_table(churn_reason)

observation

internet_service and phone services led to churn.
customer who are do not use any offer are capable of churning
customers using fibre optic have a high chance of churning

