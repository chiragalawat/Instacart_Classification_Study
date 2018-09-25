path<- "C:\\Users\\CHIRAG\\Downloads\\Assignment-Nira"
setwd(path)

##exploratory analysis

library(data.table)
library(dplyr)
library(ggplot2)
library(knitr)
library(stringr)
library(DT)
library(magrittr)
library(grid)
library(gridExtra)
library(ggthemes)
library(ggrepel)
library(tcltk)
library(gsubfn)
library(proto)
library(RSQLite)
library(sqldf)
library(Matrix)
library(arules)
library(tidyr)
library(arulesViz)
library(methods)
library(ff)

products<- read.csv("products.csv")
orders<-read.csv("orders.csv")
departments<- read.csv("departments.csv")
aisles<- read.csv("aisles.csv")

order_prior <- read.table.ffdf(file = "order_products__prior.csv", FUN = "read.csv", na.strings="")
prior<- read.csv("output001.csv")
##other way to process this file is to break it in chunks like below:
input <- file("order_products__prior.csv", "r") 
fileNo <- 1 
repeat{ 
  myLines <- readLines(input, n=100000) # 100K lines / file 
  if (length(myLines) == 0) break 
  writeLines(myLines, sprintf("output%03d.csv", fileNo)) 
  fileNo <- fileNo + 1 
} 
close(input)



kable(head(orders,10))
glimpse(orders)
dim(orders)


kable(head(products,10))
glimpse(products)
dim(products)

kable(head(departments,10))
glimpse(departments)
dim(departments)

kable(head(aisles,10))
glimpse(aisles)
dim(aisles)

kable(head(order_prior,10))
glimpse(order_prior)
dim(order_prior)
View(order_prior)

#Recoding the Character Variables to Factor
orders <- orders %>% mutate(order_hour_of_day = as.numeric(order_hour_of_day), eval_set = as.factor(eval_set))
products <- products %>% mutate(product_name = as.factor(product_name))
aisles <- aisles %>% mutate(aisle = as.factor(aisle))
departments <- departments %>% mutate(department = as.factor(department))

#Product Aisle Department merging these tables together

Products_Aisles<-merge(products,aisles,by="aisle_id")
Products_Aisles_Departments<-merge(Products_Aisles,departments,"department_id")
kable(head(Products_Aisles_Departments,6))
glimpse(Products_Aisles_Departments)
dim(Products_Aisles_Departments)

#Number of product in each aisle

Number_of_Product_each_Aisle<-Products_Aisles_Departments%>%group_by(aisle)%>%summarise(Number_of_Products=n())%>%arrange(desc(Number_of_Products))

#Top 20 Aisle by number of product offerings
Top_20<-head(Number_of_Product_each_Aisle,n=20)
Bottom_20<-tail(Number_of_Product_each_Aisle,n=20)

#Plotting Number of Products in each aisle in decreasing order(Top 20)
ggplot(Top_20, aes(x = reorder(aisle,Number_of_Products), y = Number_of_Products,label=paste0(round(Number_of_Products,0)))) +
  geom_bar(stat = "identity")+coord_flip()+
  labs(title="Top 20 Aisle by Variety of Product Offering",y="Number of Products",x="Aisle")+
  geom_text(nudge_y = 35)

#Plotting Number of Products in each aisle in decreasing order(Bottom 20)
ggplot(Bottom_20, aes(x = reorder(aisle,Number_of_Products), y = Number_of_Products,label=paste0(round(Number_of_Products,0)))) +
  geom_bar(stat = "identity")+coord_flip()+labs(title="Bottom 20 Aisle by Variety of Product offering",y="Number of Products",x="Aisle")+
  geom_text(nudge_y = 3.5)

#Number of Products in each department

Number_of_Product_each_department<-Products_Aisles_Departments%>%group_by(department)%>%summarise(Number_of_Products=n())%>%arrange(desc(Number_of_Products))

kable(head(Number_of_Product_each_department))

ggplot(Number_of_Product_each_department, aes(x = reorder(department,Number_of_Products), y = Number_of_Products,label=paste0(round(Number_of_Products,0)))) +
  geom_bar(stat = "identity")+coord_flip()+labs(title="Department by Variety of Product offering",y="Number of Products",x="Department")+
  geom_text(nudge_y = 250)

#Percentage of order evey hour

Orders_everyhour<-orders%>%group_by(order_hour_of_day)%>%summarise(Number_of_Orders=n())%>%mutate(Percentage_of_orders=(Number_of_Orders*100/nrow(orders)))
kable(head(Orders_everyhour,24))

# refrence from anoother site

x<-Orders_everyhour$Percentage_of_orders
clock.plot <- function (x, col = rainbow(n,s=1,v=1,start=0,end=max(1,n-1)/n,alpha=0.5), ...) {
  if( min(x)<0 ) x <- x - min(x)
  if( max(x)>1 ) x <- x/max(x)
  n <- length(x)
  if(is.null(names(x))) names(x) <- 0:(n-1)
  m <- 1.05
  plot(0, 
       type = 'n', # do not plot anything
       xlim = c(-m,m), ylim = c(-m,m), 
       axes = F, xlab = '', ylab = '', ...)
  a <- pi/2 - 2*pi/200*0:200
  polygon( cos(a), sin(a) )
  v <- .02
  a <- pi/2 - 2*pi/n*0:n
  segments( (1+v)*cos(a), (1+v)*sin(a), 
            (1-v)*cos(a), (1-v)*sin(a) )
  segments( cos(a), sin(a), 
            0, 0, 
            col = 'light grey', lty = 3) 
  ca <- -2*pi/n*(0:50)/50
  for (i in 1:n) {
    a <- pi/2 - 2*pi/n*(i-1)
    b <- pi/2 - 2*pi/n*i
    polygon( c(0, x[i]*cos(a+ca), 0),
             c(0, x[i]*sin(a+ca), 0),
             col=col[i] )
    v <- .1
    text((1+v)*cos(a), (1+v)*sin(a), names(x)[i])
  }
}
clock.plot(x, 
           main = "Peak Ordering Hours")

#Number of Orders every day of the week

Orders_everyday<-orders%>%group_by(order_dow)%>%summarise(Number_of_Orders=n())%>%mutate(Percentage_of_orders=(Number_of_Orders*100/nrow(orders)))

#Visualizing Number of Orders by day of the week

ggplot(Orders_everyday,aes(x=order_dow,y=Percentage_of_orders,label=paste0(round(Percentage_of_orders,1))))+
  geom_bar(stat = "identity")+labs(title="% of Orders by day of the Week",y="Percentage of Total Orders",x="Day of the Week : 0 denotes Sunday ")+
  geom_text(nudge_y = .5)

#orders Every hour of the Day 

Dow_hod_orders<-orders%>%group_by(order_dow,order_hour_of_day)%>%
  summarise(Number_of_Orders=n())

Dow_hod_orders_combined<-merge(Dow_hod_orders,Orders_everyday,by="order_dow",all.x = TRUE)%>%
  mutate(Percentage_by_doy=Number_of_Orders.x*100/Number_of_Orders.y)


#Visualizing orders by dow-->hod (Every day_ Each Hour)

ggplot(Dow_hod_orders_combined, aes(x = Dow_hod_orders_combined$order_hour_of_day, y = Dow_hod_orders_combined$Percentage_by_doy)) +
  geom_bar(stat="identity") +
  labs(title="Visualizing orders by hour of day for each day of week with 0 representing Sunday",x="0-24 represents hours of the day",y="Percentage of orders for the day")+
  facet_grid(~ Dow_hod_orders_combined$order_dow)

library(plyr)
Reordering_Gap<-count(orders,'days_since_prior_order')%>%arrange(desc(freq))%>%mutate(Percent_orders=round(freq*100/nrow(orders)))

#infrence 11% refill there groceries in 30 days and 9% in one week gap and NA represents number of new users everyday

#visualizing order gap
Reordering_Gap_plot<-ggplot(orders,aes(x=days_since_prior_order))+
  geom_histogram(aes(fill=..count..),binwidth=1)+
  scale_x_continuous(name = "Days Since Prior Order",breaks = seq(0, 30, 1))+
  scale_y_continuous(name = "Frequency of Orders",breaks=seq(0,1000000,100000))+
  ggtitle("Gap between two orders?")+
  labs(x="Days Since Prior Order")+
  theme_update()
Reordering_Gap_plot

top25_products<-count(prior$product_id)%>%arrange(desc(freq))%>%head(25)

colnames(top25_products)[1]<- "product_id"

Top25Products<-merge(top25_products,Products_Aisles_Departments,by='product_id')%>%arrange(desc(freq))

View(head(Top25Products,25))

#Visualization of top 25 products

ggplot(Top25Products, aes(x = reorder(product_name,freq), y = freq,label=paste0(round(freq,0)))) +
  geom_bar(stat = "identity")+coord_flip()+labs(title="Most ordered Products: Top 25 ",y="Number of orders",x="product_name")+
  geom_text(nudge_y = 20000)

#bottom 25

bottom25_products<-count(prior$product_id)%>%arrange(desc(freq))%>%tail(25)

colnames(bottom25_products)[1]<-'product_id'

bottom25Products<-merge(bottom25_products,Products_Aisles_Departments,by='product_id')%>%arrange(freq)

View(head(bottom25Products,25))

ggplot(bottom25Products, aes(x = reorder(product_name,freq), y = freq,label=paste0(round(freq,0)))) +
  geom_bar(stat = "identity")+coord_flip()+labs(title="least ordered Products: Bottom 25 ",y="Number of orders",x="product_name")+
  geom_text(nudge_y = 1)


# Market basket analysis calculating support confidence and lift ratio
# took refrence through online MBA techniques

prior4mba<-split(prior$product_id,prior$order_id)

transaction_prior<-as(prior4mba,"transactions")

dim(transaction_prior)

#frequent product ids in the transactions

itemFrequencyPlot(transaction_prior,support=0.05,cex.names=0.8)

#Apriori algorithm

basket_rules<-apriori(transaction_prior,parameter = list(sup=0.00001,conf=0.6,maxlen=3,target="rules"))

#Visualizing rules

#Number of Products per basket

hist(size(transaction_prior), breaks = 0:150, xaxt="n", ylim=c(0,250000), col = "grey",
     main = "Number of Products per Order", xlab = "Order Size:Number of Products")
axis(1, at=seq(0,160,by=10), cex.axis=0.8)
mtext(paste("Total:", length(transaction_prior), "Orders,", sum(size(transaction_prior)), "Products"))

#Frequently ordered products

#We find 15 products to occur when the support is set at 0.03. This means these products are found in 3% of the total transactions which is approximately about 90,000

item_frequencies <- itemFrequency(transaction_prior, type="a")
support <- 0.03
freq_items <- sort(item_frequencies, decreasing = F)
freq_items <- freq_items[freq_items>support*length(transaction_prior)]

par(mar=c(2,10,2,2)); options(scipen=5)

barplot(freq_items, horiz=T, las=1, main="Frequent Items", cex.names=.8, xlim=c(0,500000))
mtext(paste("support:",support), padj = .8)
abline(v=support*length(transaction_prior), col="red")

#Frequent items bought together

#We desire to make 2 products and 3 product combinations and hence we choose a lower support = 0.003 which means the product is in around 0.3 % of 3 million transactions that is about 10,000 times the product is sold

basket_rules<-apriori(transaction_prior,parameter = list(sup=0.0003, conf=0.5, target="rules"))

plot(basket_rules)

plot(head(sort(basket_rules,by="lift"),10),method="graph")

plot(basket_rules,method="grouped")

#Above figure visualizes all the three parameters: support, confidence, and lift. Confidence level is set at 50%. We get a set of 60 rules. We sort them by the value of lift which gives the efficiency of the rule and thereby make our product combinations