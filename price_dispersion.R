install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
library(data.table) 

price_data <- read.csv("../home_assignment_data_pricing.csv")

#This function filters data based in the product type
filter_data<- function(data,product_type){
  data %>%
    group_by(col_to_group_by) %>%
    filter(category == product_type) %>%
    na.omit()
}

#Filtering the Ps3 data
ps3dt<-filter_data(price_data,"Ps3")

#Filtering the Cellphones data
cellphonesdt<-filter_data(price_data,"Cellphones")

#head(ps3dt)
#summary(ps3dt)
#str(ps3dt)

setDT(ps3dt)
ps3mean_price<-ps3dt[ ,list(price=mean(price)), by=month_numerical]


ggplot(ps3mean_price, aes(x = month_numerical, y = price)) +
  geom_line()+
  xlab("Moenth")+
  ylab("Ps3 product mean price")

ggplot(cellphonesdt, aes(x = date, y = cpi_adjusted_price, color= store_id)) +
  geom_line()+
  xlab("Dates")+
  ylab("Cellphones product price")



