Boats <- read.csv("F:/PythonFiles/FinalProject/TestCsv.csv",stringsAsFactors = F)
attach(Boats)
Boats= Boats[,1:12]

#Data Cleaning
Zip_clean=Boats[-which(ZipCode==0 | ZipCode=="" | nchar(ZipCode)>6 | tolower(ZipCode)=="other"),]
Fuel_type=Zip_clean[-which(Zip_clean$Fuel.Type=="" | tolower(Zip_clean$Fuel.Type)=="other"),]
Price_clean=Fuel_type[-which(tolower(trimws(Fuel_type$Price))== "requestaprice"),]
Contact_clean=Price_clean[-which(nchar(Price_clean$Seller_Contact)>11),]
Contact_clean$Length=gsub("[[:punct:]]", "", Contact_clean$Length)
Final_clean=unique.data.frame(Contact_clean)
attach(Final_clean)

#Categorize Year as Old, Medieval,New
Final_clean["Year_Category"] <- NA
Final_clean$Year_Category[Final_clean$Year>=2005]= "New"
Final_clean$Year_Category[Final_clean$Year>=1980 & Final_clean$Year<2005]= "Medieval"
Final_clean$Year_Category[Final_clean$Year<1980]= "Old"
Final_clean$Price=as.numeric(Final_clean$Price)
Final_clean$Length=as.numeric(Final_clean$Length)
Final_clean$Year=as.numeric(Final_clean$Year)
attach(Final_clean)

#Write the cleaned dataset to an Excel File
library(xlsx)
write.xlsx(Final_clean, "F:/PythonFiles/FinalProject/FinalData_Cleaned.xlsx") 

# Common statistics
# Data Visualization

hist(Price,breaks=6000,col = "blue",xlim = c(0,mean(Price)))
meanPrice=mean(Price)
medianPrice=median(Price)
barplot(c(meanPrice,medianPrice), main="Mean  Vs Median Price Distribution",names.arg=c("Mean Price","Median Price"),col=c("darkblue","red"))
barplot(table(Year_Category),main="Boat Distribution by Year Category",col = c("green","red","yellow"))

library(ggplot2)
# Price Variation along months
qplot(Year_Category,Price,data = Final_clean)
qplot(Year,Price,data = Final_clean)
qplot(Class,Price,data = Final_clean)
qplot(Fuel.Type,Price,data = Final_clean)
qplot(Length,Price,data = Final_clean)
qplot(Hull.Material,Price,data = Final_clean)

#Regression Model

Scraplm=lm(log(Price)~log(Length)+log(Year)+as.factor(Fuel.Type))
summary(Scraplm);
library(mgcv)
mod=gam(Price ~ s(Year) + s(Length))
summary(mod)

#clustering
names(Final_clean)
mod2 <- kmeans(dist(Final_clean[,5]),4) #For Zipcode
clus <- as.numeric(mod2$cluster)
by(Final_clean[,5],as.factor(clus),summary)

library(fpc)
plotcluster(Final_clean$ZipCode,mod2$cluster)
