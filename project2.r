#Libraries
library(tidyverse)
library(ggplot)
library(readr)
library(ggthemes)

#For loading csv file
data=read.csv("C:\\Users\\MADDINENI VIJAY\\Desktop\\googleplaystore.csv")
print(data)
print(nrow(data))
str(data)

#Average rating across categories
data %>%  group_by(Category) %>%  filter(!is.na(Rating), Category!='1.9') %>% summarise(meanRating = mean(Rating)) %>% 
  ggplot(mapping = aes(x = Category, y = meanRating)) + geom_col(aes(fill = Category)) + geom_line(group = 1) +  coord_flip() + ggtitle("Average rating across categories") + ylab("Average rating") + xlab("category")+ guides(fill=FALSE)

#Data Cleaning
data<- data%>%
  filter(Installs != "0")
options(scipen = 999)
data$Installs <- gsub(",", "", gsub("\\.", "", data$Installs))
data$Installs <- as.character(data$Installs)
data$Installs = substr(data$Installs,1,nchar(data$Installs)-1)
data$Installs <- as.numeric(data$Installs)

#Removoing Null Values
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
data[is.nan(data)] <- 0

#visualization
#paid Vs free
data <- na.omit(data)
ggplot(data, aes(Type)) +
  geom_bar() + labs(title = "Free vs. Paid Apps",x= "", y= "Count") +
  ylim(0, 15000) + theme_wsj()

#Top 100 Apps
data %>% 
  group_by(Category) %>%
  summarize(totalInstalls = sum(Installs)) %>%
  arrange(desc(totalInstalls)) %>%
  head(100) %>%
  ggplot(aes(x = Category, y = totalInstalls, fill = Category)) +
  geom_bar(stat="identity") +
  labs(title= "Top 100 Installed Apps" ) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#How apps are prized
ggplot(data, aes(x=data$Price))+geom_density(linetype="dashed", color="Black", fill="blue")
games_data<- subset(data, Category=="GAME", select = c(App, Price, Rating,Installs))
games_data<-top_n(games_data,10,wt=Price)
View(games_data)
ggplot(games_data, aes(x=games_data$Price))+
  geom_density(color="black",fill="blue")

paid_data<- subset(data, Type=="Paid", select = c(App, Price, Rating,Installs))
ggplot(paid_data, aes(x=paid_data$Installs))+
  geom_density(color="black",fill="red")

