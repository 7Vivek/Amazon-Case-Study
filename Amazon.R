---
  title: "Amazon Case Study"
author: "Vivek limbad sdbi"
---

#----------------------Amazon Mobile Phone Reviews EDA--------------------------
#Load libraries 
library(tidyverse)
library(viridis)
library(viridisLite)
library(data.table)
library(lubridate)
library(patchwork)
library(fmsb)
library(stopwords)


#load data 
#fread() from the data.table package as a faster alternative to read.table() and read.csv().

items <- fread("C:\\Users\\Lenovo\\Downloads\\items.csv")
reviews <- fread("C:\\Users\\Lenovo\\Downloads\\20191226-reviews.csv\\reviews.csv")

# Column Name
names(items)                      #str(items)
names(reviews)                    #str(reviews)



#
sapply(items, function(x) sum(is.na(x)))
sapply(reviews, function(x) sum(is.na(x)))

#----Dropping only NAs in items because 4 have not brand names
items <- na.omit(items)




total <- left_join(items,reviews,by="asin")                 # apply left join 17 variable
str(total)
names(total)
rm(items,reviews)                                           # remove the initial two tables
gc()

#remove the unnecessary column
total = subset(total, select = -c(image) )


# 1.----Checking the Total Reviews

total %>% select(brand,totalReviews,verified) %>% 
  group_by(brand,verified) %>% 
  summarize(n=sum(totalReviews)) %>%
  ggplot(aes(x=brand,y=log(n),fill=verified))+geom_col(col="black")+
  scale_fill_viridis(discrete=T)+
  theme_minimal()+
  coord_polar()+
  theme(panel.grid.major = element_line(colour = "#808080"))+
  theme(axis.text.x = element_text(face = "bold"))+
  ylab("LOG of total reviews")+ggtitle("Total reviews of mobile companies(Amazon)")+
  labs(fill="Verified")

#2.---- How months and reviews matter

total %>% select(date) %>%
  mutate(date=mdy(date)) %>%
  mutate(month=month(date))%>% 
  count(month) %>%
  ggplot(aes(x=month,y=as.numeric(n)))+geom_smooth(method = 'loess',formula = 'y~x')+xlim(c(1,12))+
  ylab("No. of reviews")+ggtitle("Months and review counts")


#3.---- How total reviews increased with time

total %>% select(date,totalReviews,brand) %>% 
  mutate(date=mdy(date))%>%
  mutate(year=year(date)) %>% 
  select(brand,year,totalReviews) %>% 
  group_by(brand,year) %>% 
  summarize(n=(sum(totalReviews))) %>%
  ggplot(aes(x=year,y=log(n),fill=brand))+
  geom_area(alpha=0.6,size=0.5,colour="white")+
  scale_fill_viridis(discrete=T)+xlab("Year")+ylab("Log of total reviews")+
  ggtitle("Brandwise contribution to total reviews")

#4.---- Checking helpful votes by brand

total %>% select(brand,helpfulVotes) %>%
  na.omit() %>% 
  group_by(brand) %>% 
  summarize(n=sum(helpfulVotes))  ->p        # defining p here and storing as variable in model

p$fraction <- p$n/sum(p$n)
p$ymax <- cumsum(p$fraction)
p$ymin <- c(0,head(p$ymax,n=-1))
p$label_pos <- (p$ymax+p$ymin)/2
p$label <- paste0(p$brand,"\n votes: ",p$n)
ggplot(p,aes(ymax=ymax,ymin=ymin,xmax=4,xmin=3,fill=brand))+
  geom_rect(show.legend=F)+
  geom_label(x=4,aes(y=label_pos,label=label),size=3,show.legend=F,hjust=1,alpha=0.6)+
  theme(legend.position="none")+
  coord_polar(theta="y")+xlim(c(2,4)) +
  theme_void()+ggtitle("Helpful votes")

#str(total)

#5.---- Maximun words used in reviews companywise
to_remove <- c(stopwords("english"),"phone","Samsung","Nokia","Apple","ASUS","OnePlus","Motorola","HUAWEI","Sony","Google","Xiaomi","great","like","good",
               "samsung","nokia","iphone","apple","asus","oneplus","motorola","huawei","sony","google","xiaomi")

p <- total %>% 
  select(brand,body) %>% 
  unnest_tokens(input=body,output=word) %>%
  count(brand,word,sort=T) %>% 
  filter(nchar(word)>3) %>%
  filter(!word %in% to_remove) %>%
  group_by(brand)

p %>% top_n(n=6,n) %>%
  ggplot(aes(x=reorder(word,n),y=n,fill=brand)) + 
  geom_col(show.legend=F,col="black")+
  coord_flip()+ 
  facet_wrap(~brand,ncol=3,scales="free")+
  xlab("") + ylab("Count")+
  ggtitle("Feedback")

#6.---- Sentiments of reviews by company

p %>%
  inner_join(get_sentiments("bing")) %>%
  mutate(linenumber=row_number()) %>%
  count(index=linenumber,sentiment)%>%
  spread(sentiment,n,fill=0) %>%
  group_by(brand) %>% 
  summarize(neg=sum(negative)*-1,
  pos=sum(positive)) %>%
  gather(type,value,c(neg,pos)) %>% 
  arrange(brand) %>%
  ggplot(aes(x=brand,y=value,fill=brand))+
  geom_col(show.legend=F,aes(fill=ifelse(value>0,"green","blue")),col="white",width=0.5)+geom_hline(yintercept=0)+
  coord_flip()+geom_text(aes(label=brand),size=4)+ylab("<-Negative --Sentiments-- Positive->")+theme_classic()+
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line=element_blank(),
    axis.title.x=element_text(hjust=0.69))+xlab("")+ggtitle("Sentiments of reviews -/+")









