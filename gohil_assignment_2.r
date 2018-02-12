install.packages('nycflights13')
install.packages('dplyr')
install.packages("jupyter")
library(nycflights13)
library(ggplot2)
library(dplyr)
View(flights)
data(flights)
data <- flights
rm(list=ls())
class(data)
avg_delay <-  summary(mean(data$arr_delay, na.rm = TRUE) ,  mean(data$dep_delay, na.rm = TRUE))


#Q1
#a)How many flights were there to and from NYC in 2013?
sum(nrow(flights[flights[,"dest"] == 'JFK' | flights[,"dest"]=='LGA' | flights[,"dest"]=='EWR',"flight"]),nrow(flights))
#There were a total of 336777 flights (To and From) NYC airports in 2013

#b)How many flights were there from NYC airports to Seattle (SEA) in 2013?
sum(nrow(flights[flights[,"dest"]=='SEA',"flight"]))
#There were 3923 flights from NYC to SEA in 2013

#c)How many airlines fly from NYC to Seattle? Hint: look at the function unique()
nrow(unique(flights[flights[,"dest"]=='SEA',"carrier"]))
NROW(unique(subset(flights$carrier,flights$dest=='SEA')))
# 5 airlines fly from NYC to Seattle (note: tried two diffrent ways)


#d)What is the average arrival delay for flights from NYC to Seattle?
mean(subset(flights$arr_delay,flights$dest=='SEA'),na.rm = T)
# Average arrival delay is -1.099 for flights to Seattle


#Q2.
#a)What is the mean arrival delay time? What is the median arrival delay time?
mean(flights$arr_delay,na.rm = T) #Mean Arrival Delay is 6.895
median(flights$arr_delay,na.rm = T) #Median is -5

#b)What does a negative arrival delay mean?
#Negative Arrival delay means that the flight actually arrived before Scheduled Arrival time.
#In other words, the flight was not delayed.


#c)Plot a histogram of arrival delay times.

qplot(x = arr_delay, data = flights, geom = "histogram")
p <- ggplot(flights, mapping = aes(arr_delay)) +
  labs(x = "Arrival Delay", y = "Count") +
  ggtitle(label = "Arrival Delays 2013", subtitle = "3 Major NY City Airport") +
  xlim(-100, 150)
p + geom_histogram(bins = 40)  

p + geom_histogram(aes(y = ..density..),
                   bins = 150,
                   colour = "black",
                   fill = "white") +
geom_density(alpha = .2, fill = "#FF6666") 

#d)Is there seasonality in departure delays?
plot(flights$month, flights$dep_delay)
by(flights$dep_delay, flights$month, function(x) mean(x, na.rm=T))

ggplot(flights, aes(x = month, y = dep_delay)) +
  geom_bar(stat="identity")

ggplot(flights, aes(x=month, y=dep_delay,color=origin)) + 
  geom_point() + scale_color_manual(values = c("red", "blue", "green")) +
  geom_hline(yintercept=0, color="black")
#There is a significant increase in delay during the months of June, July and December
#Hence, flights are more likely to be delayed
#Best month to leave NYC would be in November
#Worst Month to leave NYC is July
#This overlaps with Sumemr and Christmas break. 


#Q3
#a)Plot a histogram of the total air flight time with 100 breaks. . 
#How many peaks do you see in this distribution? What is an explanation for this?

hist(flights$air_time, 
     main="Histogram for total air time", 
     xlab=" Total Air time", 
     border="black", 
     col="gold",
     las=1, 
     breaks=100)


#b)What time of day do flights most commonly depart? 
#Why might there be two most popular times of day to depart?
hist(flights$dep_time, 
     main="Histogram for total air time", 
     xlab=" Total Air time", 
     border="blue", 
     col="green",
     las=1, breaks=100)

#6am to 8am and  2:30pm to 
#c)Plot a box plot of departure delays and hour of departure. What pattern do you
#see? What is an explanation for this?
ggplot(data = flights, mapping = aes(x = dep_delay, y = hour), binwidth=100, na.rm=T) +
  geom_boxplot()

#This boxplot suggests that the minimum hour of departure is 5 and the maximum hour of departure is
#approximately 23. Q1 is close to 16. Q3 is close to 9. And the median is 13.
#Also, boxplots give a quick summary of the data. 
#Hence, we see that while the departure delays lie between 0 to 12.50, 
#the hour of departure lies in the 9 to 13 quartile range.

boxplot(flights$dep_delay~flights$hour, range  = 1.5,xlab="Hour of departure", ylab="Departure Delays", ylim=c(-20,80))


#Q4
#Which of the three airports of new york has the highest delay on average in departure?
#In this airport, which fligh carrier is contributing to the most delay?

plot(flights$distance,flights$arr_delay)
x1 <- by(flights$dep_delay, flights$origin, function(x) mean(x, na.rm=T))
barplot(x1)
#This boxplot shows that EWR has highest departure delay on average.

jfk <- filter(flights, flights$origin=='JFK')
ewr <- filter(flights, flights$origin=='EWR')
lga <- filter(flights, flights$origin=='LGA')
par(mfrow=c(1,1))
x2 <- by(jfk$dep_delay, jfk$carrier,function(x) mean(x, na.rm=T))
x3 <- by(ewr$dep_delay, ewr$carrier,function(x) mean(x, na.rm=T))
x4 <- by(lga$dep_delay, lga$carrier,function(x) mean(x, na.rm=T))
barplot(x2)
#Endevor Airlines(9E) is causing the most delay in JFK airport
barplot(x3)
#Endevor Airlines(OO) is causing the most delay in EWR airport
barplot(x4)
#Frontier Airlines(F9) is causing the most delay in LGA airport