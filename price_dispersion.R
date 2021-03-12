#install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
library(data.table) 

price_data <- read.csv("../home_assignment_data_pricing.csv")

#This function filters data based in the product type
filter_data<- function(data,product_type){
  data %>%
    filter(category == product_type) %>% 
    group_by(store_id,year,month_numerical, product_id) %>% 
    summarise_at(vars("log_of_cpi_adjusted_price"), mean)
}

#Filtering the Ps3 data,
ps3dt<-filter_data(price_data,"Ps3")

ps3dt<-ps3dt[order(ps3dt$store_id,ps3dt$year,ps3dt$month_numerical),]


#Filtering the Ps3 data and consider store ids between 10000 and 17000
ps3dt <- ps3dt %>%
  filter(store_id > 9000, store_id < 17000)

ps3dt$store_id <- factor(ps3dt$store_id)

ps3dt$product_id <- factor(ps3dt$product_id)

#setDT(ps3dt)
#ps3mean_price<-ps3dt[ ,list(price=mean(price), store_id), by=product_id]

ggplot(ps3dt, aes(x = month_numerical, y = log_of_cpi_adjusted_price, color = store_id)) +
  xlab("Time")+
  ylab("Ps3 product mean price")+
  geom_line()+
  facet_wrap(~year)


ggplot(ps3dt, aes(x = time(log_of_cpi_adjusted_price), y = log_of_cpi_adjusted_price, color = store_id)) +
  xlab("Time")+
  ylab("Ps3 product mean price")+
  geom_point()+
  geom_line()+
  facet_wrap(~year)
