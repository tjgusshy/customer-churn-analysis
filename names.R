# 1] "customer_id"                       "gender"                            "age"                               "married"                           "number_of_dependents"             
# [6] "city"                              "zip_code"                          "latitude"                          "longitude"                         "number_of_referrals"              
# [11] "tenure_in_months"                  "offer"                             "phone_service"                     "avg_monthly_long_distance_charges" "multiple_lines"                   
# [16] "internet_service"                  "internet_type"                     "avg_monthly_gb_download"           "online_security"                   "online_backup"                    
# [21] "device_protection_plan"            "premium_tech_support"              "streaming_tv"                      "streaming_movies"                  "streaming_music"                  
[26] "unlimited_data"                    "contract"                          "paperless_billing"                 "payment_method"                    "monthly_charge"
# [31] "total_charges"                     "total_refunds"                     "total_extra_data_charges"          "total_long_distance_charges"       "total_revenue"                    
# [36] "customer_status"                   "churn_category"                    "churn_reason"                     
# > 
  
"unlimited_data"
avg_monthly_gb_download
number_of_referrals

tenure_in_months
total_charges
contract
total_refunds
paperless_billing
monthly_charge
total_revenue

  customer_churn_2  <-  select(customer_churn, customer_status,tenure_in_months,offer,phone_service,
                                   tenure_in_months,,multiple_lines,internet_service,internet_type,unlimited_data,contract,churn_category,avg_monthly_gb_download,
                                   streaming_tv,paperless_billing,total_extra_data_charges,married,streaming_movies,total_long_distance_charges,
                                   payment_method,multiple_lines,streaming_music,monthly_charge
            
            
    customer_churn %>% 
      
      
      
      
12000*800
    

    select(is_numeric())
    
    `r text_spec("churned",bold= T, color = "red")`
            
  
  corrgram(customer_churn_2)
  ggpairs(customer_churn_2, ggplot(churn_category))
  library(psych)
  pairs.panel(customer_churn_2)
  
  view(customer_churn_2)
  ggplot(aes(reorder(churn_reason,n),n, fill=gender))+geom_col()+coord_flip()+
  scale_fill_manual("Gender",values = c("#b2df8a","#bebada"))+theme(plot.title = element_text(face = "bold",size = 10 ),axis.ticks = element_blank(),
                                                                    legend.key=element_blank(),
                                                                    panel.background = element_blank(),
                                                                    axis.title =    element_text(face = "bold",size = 8 )                                                    )+
  labs(subtitle = "Both gender left for the same reasons", title = "Most of the customers left for our compertitors" )+xlab("Churn Reason")+ylab(("No. of Churn"))


library(ggcharts)
install.packages("ggcharts")
?lolipop_chart
?ggcharts

bar

biomedicalrevenue %>%
  filter(year %in% c(2012, 2015, 2018)) %>%
  bar_chart(x = company, y = revenue, facet = year)

?highlight_spec

spec <- highlight_spec( c("darkgreen", "darkorange"))
bar_chart(revenue2018, company, revenue, hi)




d <- data.frame(
  idx = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
  value = c(1, 2, 3, 10, 11, 12, 9, 10, 11),
  category = rep(c("a", "b", "c"), 3),
  stringsAsFactors = FALSE
)

# Highlight the lines whose max values are larger than 10
ggplot(d, aes(idx, value, colour = category)) +
  geom_line() + gghighlight(max(value) > 10)

# Highlight the points whose values are larger than 10
ggplot(d, aes(idx, value)) +
  geom_point() +
  gghighlight(value > 10, label_key = category)

# Specify the styles for unhighlighted layer
ggplot(d, aes(idx, value, colour = category)) +
  geom_line(size = 5) +
  gghighlight(max(value) > 10,
              unhighlighted_params = list(size = 1)
  )



p <- ggplot(mtcars, aes(hp)) +
  geom_histogram() +
  ggtitle("Mixing **bold** and *italics* is easy")

# Text is not rendered
p + my_theme

# Text is rendered properly
p + as_md_theme(my_theme)
my_theme <- theme_gray() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  )

ggplot(mtcars, aes(hp)) +
  geom_histogram() +
  ggtitle("Mixing **bold** and *italics* is easy")+as_md_theme(my_theme)

theme(legend.position = "None",plot.title = element_text(face = "bold",size = 20, hjust = 0.5),strip.text.x = element_text(size = 20),axis.ticks = element_blank(),
      legend.key=element_blank(),
      panel.background = element_blank(),legend.title= element_text(size = 15),legend.text = element_text(size = 15),
      axis.title =    element_text(face = "bold",size = 15 ), axis.text =element_text(size = 15))



findinds

library(GGally)


^^ people who do not receive tech support churned.
cor(customer_churn)
corrplot::corrplot(customer_churn)
library(corrplot)
corrplot(corr = cor(mtcars))

library(GGally)

ggpairs(customer_churn)
ggcorr(customer_churn)


library(corrgram)

corrgram(customer_churn)




customer_churn_2  <-  data.frame()


p3 <- ggplot(mtcars, aes(wt, mpg)) +
 eme(strip.background = element_rect(colour = "black", fill = "white"))
p3 + theme(strip.text.x = element_text(colour = "white", face = "bold"))
p3 + theme(panel.spacing = unit(2, "lines"))

