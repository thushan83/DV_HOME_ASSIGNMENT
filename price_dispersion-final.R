library(tidyverse)
library(ggplot2)
library(data.table) 
library(scales)
library(ggridges)

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

#ps3dt_date<-ps3dt[order(ps3dt$store_id,ps3dt$year,ps3dt$week,ps3dt$weekday),]

#Filtering the Ps3 data and consider store ids between 5000 and 17000
ps3dt <- ps3dt %>%
  filter(store_id > 5000, store_id < 17000)

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


products<-c("1619812", 
             "2992621",
             "2992622",
             "3096664",
             "3186032",
             "2719933",
             "446376"
)

ps3dt_filtered_products<- ps3dt[product_id %in% products]


ggplot(ps3dt_filtered_products, aes(x = date, y = log_of_cpi_adjusted_price, color = store_id, group = store_id)) +
  xlab("Date")+
  ylab("Ps3 price")+
  geom_step(size = 5, alpha = 0.5)+
  scale_x_date(breaks = "2 weeks")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(ps3dt_filtered_products, aes(x = date, y = log_of_cpi_adjusted_price, color = store_id, group = store_id)) +
  xlab("Date")+
  ylab("Ps3 price")+
  geom_step(size = 1, alpha = 0.5)+
  scale_x_date(breaks = "4 month")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_wrap(~product_id)

ggplot(ps3dt_filtered_products, aes(x = price, y = product_id, fill = stat(x))) + 
  geom_density_ridges_gradient(quantile_lines = TRUE, quantiles = 4,alpha = 0.8)+
  scale_fill_viridis_c(name = "Price", option = "C") +
  labs(title = 'PS3 prices from 2016-05-01 to 2017-01-01')+
  xlab("Item price")+
  ylab("PS3 product id")+
  theme_ridges()


#---------------------------------------------------------------------------
#consider data from 2016-05-01 to 2017-01-01


get_mean_prices_sd_by_vendor<-function(input){
  input %>%                                        # Specify data frame
    group_by(product_id, "store"= store_id) %>%                         # Specify group indicator
    summarise_at(vars(price),              # Specify column
                 list(mean_price = mean, sd = sd))            # Specify function
  
}

get_sd_and_product_id<- function(input){
  c(sd(input$mean_price),input$product_id)
}

ps3dt_product_gt20160501_1619812<-ps3dt_filtered_products[product_id=="1619812"]

ggplot(ps3dt_product_gt20160501_1619812, aes(x = price, y = store_id, fill = stat(y))) + 
  geom_density_ridges(alpha = 0.8)+
  scale_fill_viridis_c(name = "Store Id", option = "C") +
  labs(title = 'PS3 item 1619812 prices from 2016-05-01 to 2017-01-01')+
  xlab("Item price")+
  ylab("Store id")

ps3dt_product_gt20160501_2992621<-ps3dt_filtered_products[product_id=="2992621"]

ggplot(ps3dt_product_gt20160501_2992621, aes(x = price, y = store_id, fill = stat(y))) + 
  geom_density_ridges(alpha = 0.8)+
  scale_fill_viridis_c(name = "Store Id", option = "B") +
  labs(title = 'PS3 item 1619812 prices from 2016-05-01 to 2017-01-01')+
  xlab("Item price")+
  ylab("Store id")

ps3dt_product_gt20160501_2992622<-ps3dt_filtered_products[product_id=="2992622"]

ggplot(ps3dt_product_gt20160501_2992622, aes(x = price, y = store_id, fill = stat(y))) + 
  geom_density_ridges(alpha = 0.8)+
  scale_fill_viridis_c(name = "Store Id", option = "B") +
  labs(title = 'PS3 item 1619812 prices from 2016-05-01 to 2017-01-01')+
  xlab("Item price")+
  ylab("Store id")

ps3dt_product_gt20160501_3096664<-ps3dt_filtered_products[product_id=="3096664"]

ggplot(ps3dt_product_gt20160501_3096664, aes(x = price, y = store_id, fill = stat(y))) + 
  geom_density_ridges(alpha = 0.8)+
  scale_fill_viridis_c(name = "Store Id", option = "B") +
  labs(title = 'PS3 item 1619812 prices from 2016-05-01 to 2017-01-01')+
  xlab("Item price")+
  ylab("Store id")

ps3dt_product_gt20160501_3186032<-ps3dt_filtered_products[product_id=="3186032"]

ggplot(ps3dt_product_gt20160501_3186032, aes(x = price, y = store_id, fill = stat(y))) + 
  geom_density_ridges(alpha = 0.8)+
  scale_fill_viridis_c(name = "Store Id", option = "B") +
  labs(title = 'PS3 item 1619812 prices from 2016-05-01 to 2017-01-01')+
  xlab("Item price")+
  ylab("Store id")

ps3dt_product_gt20160501_2719933<-ps3dt_filtered_products[product_id=="2719933"]

ggplot(ps3dt_product_gt20160501_2719933, aes(x = price, y = store_id, fill = stat(y))) + 
  geom_density_ridges(alpha = 0.8)+
  scale_fill_viridis_c(name = "Store Id", option = "B") +
  labs(title = 'PS3 item 1619812 prices from 2016-05-01 to 2017-01-01')+
  xlab("Item price")+
  ylab("Store id")

ps3dt_product_gt20160501_446376<-ps3dt_filtered_products[product_id=="446376"]

ggplot(ps3dt_product_gt20160501_446376, aes(x = price, y = store_id, fill = stat(y))) + 
  geom_density_ridges(alpha = 0.8)+
  scale_fill_viridis_c(name = "Store Id", option = "B") +
  labs(title = 'PS3 item 1619812 prices from 2016-05-01 to 2017-01-01')+
  xlab("Item price")+
  ylab("Store id")

p1<-get_mean_prices_sd_by_vendor(ps3dt_product_gt20160501_1619812)

p2<-get_mean_prices_sd_by_vendor(ps3dt_product_gt20160501_2992621)

p3<-get_mean_prices_sd_by_vendor(ps3dt_product_gt20160501_2992622)

p4<-get_mean_prices_sd_by_vendor(ps3dt_product_gt20160501_3096664)

p5<-get_mean_prices_sd_by_vendor(ps3dt_product_gt20160501_3186032)

p6<-get_mean_prices_sd_by_vendor(ps3dt_product_gt20160501_2719933)

p7<-get_mean_prices_sd_by_vendor(ps3dt_product_gt20160501_446376)


#Task 2------------------------------------------------------------

ps3dt_3186032_new<-ps3dt_product_gt20160501_3186032
ps3dt_3186032_new$product_id <- as.character(ps3dt_product_gt20160501_3186032_new$product_id)
ps3dt_3186032_new$store_id <- as.character(ps3dt_product_gt20160501_3186032_new$store_id)

filter_cols<-function(input,filter_year){
  input%>%filter(year == filter_year)%>%select(store_id,cpi_adjusted_price)
}

ps3dt_3186032_2015<-filter_cols(ps3dt_3186032_new,"2015")
ps3dt_3186032_2016<-filter_cols(ps3dt_3186032_new,"2016")
ps3dt_3186032_2017<-filter_cols(ps3dt_3186032_new,"2017")

ps3dt_3186032_2015<-na.omit(ps3dt_3186032_2015)
ps3dt_3186032_2015.scaled<-scale(ps3dt_3186032_2015)


