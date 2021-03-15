library(tidyverse)
library(ggplot2)
library(data.table) 
library(scales)

price_data <- read.csv("../home_assignment_data_pricing.csv")

#This function filters data based in the product type
filter_data<- function(data,product_type){
  data %>%
    filter(category == product_type) %>% 
    group_by(year,month_numerical, week, weekday, store_id, product_id) #  %>% 
    #summarise_at(vars("log_of_cpi_adjusted_price"), mean)
}

#Filtering the Ps3 data,
ps3dt<-filter_data(price_data,"Ps3")

ps3dt_date<-ps3dt[order(ps3dt$store_id,ps3dt$year,ps3dt$week,ps3dt$weekday),]

#Filtering the Ps3 data and consider store ids between 10000 and 17000
ps3dt <- ps3dt %>%
  filter(store_id > 10000, store_id < 17000)

ps3dt$store_id <- factor(ps3dt$store_id)

ps3dt$product_id <- factor(ps3dt$product_id)

setDT(ps3dt)

ps3dt[,"date"] <- as.Date(ps3dt$date)

#from these set of plots we can understand the product type of ps3
#that are being sold by most of the vendors
ggplot(ps3dt, aes(alpha(0.01),x = date, y = log_of_cpi_adjusted_price, color = store_id, group = store_id)) +
  xlab("Time")+
  ylab("Ps3 product mean price")+
  geom_step(size = 1)+
  facet_wrap(~product_id)


#levels(ps3dt$product_id)
#[1] "118314"  "118368"  "253583"  "281484"  "377284"  "446376"  "492459"  "523907"  "560989" 
#[10] "576085"  "632007"  "641971"  "643176"  "677937"  "782157"  "792609"  "806787"  "917342" 
#[19] "917355"  "970331"  "1046073" "1093458" "1112931" "1125245" "1275172" "1278280" "1279226"
#[28] "1284584" "1341803" "1372514" "1414898" "1619812" "1657179" "1724504" "1788309" "1857076"
#[37] "1863129" "2073927" "2283039" "2348308" "2371959" "2524653" "2608908" "2622725" "2687750"
#[46] "2719933" "2782670" "2811073" "2992621" "2992622" "3096664" "3096685" "3186032" "3195060"
#[55] "3195120" "3520955" "3704473"

ps3dt_product <- ps3dt[product_id=="3186032"]

ggplot(ps3dt_product, aes(x = date, y = log_of_cpi_adjusted_price, color = store_id, group = store_id)) +
  xlab("Date")+
  ylab("Ps3 price")+
  geom_step(size = 1)+
  scale_x_date(breaks = "1 month")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#---------------------------------------------------------------------------



