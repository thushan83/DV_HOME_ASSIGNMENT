library(tidyverse)
library(ggplot2)
library(data.table) 
library(scales)
library(ggridges)
price_data <- read.csv("home_assignment_data_pricing.csv")
#data_pricing <-fread("home_assignment_data_pricing.csv")
#This function filters data based in the product type
filter_data<- function(data,product_type){
  data %>% 
    filter(category == product_type) %>% 
    group_by(year,month_numerical, week, store_id, product_id)
}

#Filtering the Ps3 data,
ps3dt<-filter_data(price_data,"Ps3")
#ps3dt<-ps3dt %>%                                        
#  group_by(year,month_numerical,week,product_id, "store"= store_id) %>%                         
#  summarise_at(vars(price),             
#               list(mean_price = mean)) 
#view(ps3dt)
ps3dt <- ps3dt%>%filter(store_id > 5000, store_id < 17000)

setDT(ps3dt)

ps3dt[,"time"] <- as.character(paste(ps3dt$year,ps3dt$month_numerical,ps3dt$week,ps3dt$store, sep = "-"))

products<-c("1619812", 
            "2992621",
            "2992622",
            "3096664",
            "3186032",
            "2719933",
            "446376"
)

ps3dt_filtered_products<- ps3dt[ps3dt$product_id %in% products]


ps3dt_product_gt20160501_1619812<-ps3dt_filtered_products[product_id=="1619812"]

ps3dt_product_gt20160501_2992621<-ps3dt_filtered_products[product_id=="2992621"]

ps3dt_product_gt20160501_2992622<-ps3dt_filtered_products[product_id=="2992622"]

ps3dt_product_gt20160501_3096664<-ps3dt_filtered_products[product_id=="3096664"]

ps3dt_product_gt20160501_3186032<-ps3dt_filtered_products[product_id=="3186032"]

ps3dt_product_gt20160501_2719933<-ps3dt_filtered_products[product_id=="2719933"]

ps3dt_product_gt20160501_446376<-ps3dt_filtered_products[product_id=="446376"]

export<-function(input){
  prod_id<-input$product_id[1]
  tmp<-input%>%select(store_id,date,log_of_cpi_adjusted_price)
  tmp$date<-str_replace_all(tmp$date, c('-'), "")
  file_path=paste(".\\mydataclean_prod_",prod_id,".xlsx")
  write.csv(tmp,file_path, row.names = TRUE)
}


transform_to_csv<-function(input){
  store_ids = unique(input$store_id)
  prod_id<-input$product_is[1]
  i<-0
  for (str_id in store_ids) {
    
    
    input1<-input%>%filter(store_id == str_id)%>%select(cpi_adjusted_price,date,store_id)
    input1[order(input1$date)]
    
    column.names<-input1$date
    
    row.names<-as.character(str_id)
    rows<-input1$cpi_adjusted_price
    
    result <- matrix(
      rows,
      nrow = length(row.names),
      byrow = TRUE,
      dimnames = list(
        row.names,
        column.names
      )
    )
    
    file_path=paste(".\\mydataclean_prod_",prod_id,"_stor_",str_id,".xlsx")
    write.csv(result,file_path, row.names = TRUE)
    
    i<-i+1
  }
  
  return(result)
}

export(ps3dt_product_gt20160501_1619812)

#transform_to_csv(ps3dt_product_gt20160501_1619812)