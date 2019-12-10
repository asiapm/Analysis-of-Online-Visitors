library(dplyr)  
library(ggplot2) 
library(scales)
library(forcats)

data<-read.csv("online_shoppers_intention.csv")
print(data)
is.data.frame(data)

print(data$Region)

print(data$Revenue)

#most shoppers did not purchase items while visiting this site, as shown here

countTrueRev<-length(which(data$Revenue=="TRUE"))
countTrueRev

countFalseRev<-length(which(data$Revenue=="FALSE"))
countFalseRev

#most shoppers visited the site on the weekdays, not the weekends, as shown here

countWeekend<-length(which(data$Weekend=="TRUE"))
countWeekend

countNoWeekend<-length(which(data$Weekend=="FALSE"))
countNoWeekend

#most shoppers did not look at an administrative page, as shown here

countAdmin0<-length(which(data$Administrative=="0"))
countAdmin0

countAdmin1<-length(which(data$Administrative=="1"))
countAdmin1

countAdmin2<-length(which(data$Administrative=="2"))
countAdmin2

countAdmin3<-length(which(data$Administrative=="3"))
countAdmin3

#most shoppers (10,869) checked administrative pages for less than 60 seconds
#less than 1500 shoppers checked administrative pages for more than 60 seconds
#almost 6000 shoppers did not check administrative pages 

countAdDur<-length(which(data$Administrative_Duration<"60"))
countAdDur

countAdDur0<-length(which(data$Administrative_Duration=="0"))
countAdDur0

countAdDur1<-length(which(data$Administrative_Duration>"60"))
countAdDur1

#the max amount of pages relating to products looked at is 705, the min is 0

max(data$ProductRelated, na.rm = TRUE)

min(data$ProductRelated, na.rm = TRUE)

#counting the amount of pages viewed 

countProductPages0<-length(which(data$ProductRelated<="5"))
countProductPages0

countProductPages1<-length(which(data$ProductRelated>="5", data$ProductRelated<"20"))
countProductPages1

countProductPages2<-length(which(data$ProductRelated>="20", data$ProductRelated<"100"))
countProductPages2

countProductPages3<-length(which(data$ProductRelated>="100", data$ProductRelated<"300"))
countProductPages3

countProductPages4<-length(which(data$ProductRelated>="300", data$ProductRelated<"1000"))
countProductPages4

sum(data$ProductRelated, na.rm = TRUE)

#sum of total amount of visitors is 12,330, as shown here

v<-sum(countVisNew+countVisOther+countVisRet)
v

#4,780 visitors were from Region 1

countRegion1<-length(which(data$Region=="1"))
countRegion1

#VISITOR TYPE
#There are 10,551 returning visitors
#There are 1,694 new visitors
countVisNew<-length(which(data$VisitorType=="New_Visitor"))
countVisNew

countVisRet<-length(which(data$VisitorType=="Returning_Visitor"))
countVisRet

countVisOther<-length(which(data$VisitorType=="Other"))
countVisOther

#Types of TRAFFIC: Direct (shopper typed URL into browser), 
#referral (shopper clicked on a link from another website), 
#search (shopper came from search engine), campaign (shopper came from campaign), and there are more different branches of said traffic

#The less pages customers looked at, the more likely they were to buy a product. Most who viewed product related pages purchased a product 
#this graph shows relationship with revenue
ggplot(data = data, aes(x=data$ProductRelated, y=data$VisitorType, 
                        colour=data$VisitorType, 
                        size=data$Revenue))+
      geom_point()


#this graph shows relationship between visitors and product related pages with no revenue factored in
qplot(data$VisitorType,data$ProductRelated,xlab = 'Visitor Type', 
      ylab = 'Number of Product Related Pages Viewed',
      main='Visitor Type VS Product Pages')

#Quick plot: Administrative Pages vs Visitors
#Returning visitors visited a greater amount of administrative pages to view/edit their account

qplot(data$VisitorType,data$Administrative,xlab = 'Visitor Type', 
      ylab = 'Admin Pages',
      main='Visitor Type VS Admin Pages')

#Graph shows how long shoppers took viewing/editing their account information
#Most visitors took 15 minutes or less to view/edit information

qplot(data$VisitorType,data$Administrative_Duration,xlab = 'Visitor Type', 
      ylab = 'Seconds (time)',
      main='Administrative Pages Duration')


#Bounce rate of visitors/traffic type (where the visitors came from)
qplot(data$BounceRates,data$TrafficType,
      xlab = 'Bounce Rate', 
      ylab = 'Traffic Type',
      main='Bounce Rates of Visitors & Traffic Type', colour=data$VisitorType)



