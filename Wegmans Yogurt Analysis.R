setwd("~/Wegmans")
install.packages("ggplot2", dependencies=T)
install.packages("tm",dependencies=T)
install.packages("wordcloud", dependencies=T)
wegmans<-read.spss("Wegmans Survey 1.sav",to.data.frame = TRUE,
                   use.value.labels = TRUE,use.missings = TRUE,trim.factor.names = TRUE)

library(ggplot2)
library(wordcloud)
library(tm)
library(foreign)

# 1. Depict the portion of the sample that is female vs. male.    
wegmansFemale <- wegmans$Question33Areyou[wegmans$Question33Areyou == "Female           "]
wegmansMale <- wegmans$Question33Areyou[wegmans$Question33Areyou == "Male             "]
length(wegmansFemale)  #Find the number of female is 889 
length(wegmansMale) #Find the number of male is 105
populationSEX = c(.13,.87) 
sampleSex =c(105, 889) 
chisq.test(sampleSex,p=populationSEX) 
#P-value= 0.02302.Reject null hypothesis since p-value is smaller than confidence level 5%. 
#Therefore,the sample collected cannot represent the population.


# 2. Describe the average importance ratings  
impVars <- c("Question6Allnatural","Question6Blended","Question6Calorielevel","Question6Consistency","Question6Fatlevel","Question6Fruitonthebottom","Question6Organic","Question6Price","Question6Proteinlevel","Question6rbSTfree","Question6Sidebysidecup","Question6Taste","Question6Texture")
temp=sapply(wegmans[impVars],as.numeric)
impMeans <- colMeans(temp, na.rm = TRUE)  #column means
impSD <-  apply(temp,2,sd,na.rm=TRUE)/sqrt(colSums(!is.na(wegmans[,impVars]))) #standard error
ratingsforAttrubutes=data.frame(name=names(impMeans),impMeans,impSD)
write.csv(ratingsforAttrubutes,file = "ratings for important attributes.csv")

#Plot the Error Bar plot
impVars2 = c("Allnatural","Blended","Calorielevel","Consistency","Fatlevel","Fruitonthebottom","Organic","Price","Proteinlevel","rbSTfree","Sidebysidecup","Taste","Texture")
dodge = position_dodge(width=.75)
plotdata <- data.frame(impMeans, impVars2, impSD)
gp = ggplot(plotdata,aes(y=impMeans,x=impVars2,ymax=impMeans+impSD,ymin=impMeans-impSD))
gp + geom_bar(position=dodge,stat="identity",col=brewer.pal(7, "Set3")[4],fill=brewer.pal(7, "Set3")[4],width=.75) + 
  geom_errorbar(position=dodge,width=1) + 
  labs(x="Attribute Importance",y="Mean Rating (1-7)") + 
  theme(axis.text.x=element_text(angle=45,hjust=1, colour="black",size = 5))


# 3. Compare the brand attribute ratings for Fage (Q24) versus Oikos (Q30) given by those who 
#purchased a brand in the past month. Do these brands differ from one another on perceptions of 
#(1) All natural, (2) Price, and (3) Taste?

pastMonthPurchase = wegmans[wegmans$Question1HaveyoupurchasedGreekYogurtinthepastmonth == "Yes",]

pastFage = wegmans[wegmans$Question21HaveyoupurchasedFageGreekYogurtinthepastmonth=='Yes',]
pastOikos = wegmans[wegmans$Question28HaveyoupurchasedStonyfieldOikosGreekYogurtinthepastmon=='Yes',]

pastFage=as.data.frame(sapply(pastFage,as.factor))
pastOikos=as.data.frame(sapply(pastOikos,as.factor))

FAttributes = c("Question24Allnatural","Question24Price", "Question24Taste")
OAttributes = c('Question30Allnatural','Question30Price','Question30Taste')


#Revalue 'unsure' to 'NA'
for (i in 1:3) {
  pastFage[[FAttributes[i]]]=revalue(pastFage[[FAttributes[i]]], c(Unsure=NA))
  pastOikos[[OAttributes[i]]]=revalue(pastOikos[[OAttributes[i]]], c(Unsure=NA))
}
pastFage=as.data.frame(sapply(pastFage,as.numeric))
pastOikos=as.data.frame(sapply(pastOikos,as.numeric))

#All natural
a<-c(pastFage[,'Question24Allnatural'],na.rm=T)
b<-c(pastOikos[,'Question30Allnatural'],na.rm=T)
t.test(a,b,paired=FALSE)
#p-value = 0.8986

#p-value = 0.6076

a2<-c(pastFage[,'Question24Price'],na.rm=T)
b2<-c(pastOikos[,'Question30Price'],na.rm=T)
t.test(a2,b2,paired=FALSE)
#p-value = 0.00197

#0.08587

a3<-c(pastFage[,'Question24Taste'],na.rm=T)
b3<-c(pastOikos[,'Question30Taste'],na.rm=T)
t.test(a3,b3,paired=FALSE)
#p-value = 1.338e-14

#p-value < 2.2e-16

#Calculate the mean rating for the three attributes under different usage situation
ratingFage= colMeans(pastFage[,FAttributes], na.rm = TRUE)
ratingOikos = colMeans(pastOikos[,OAttributes], na.rm = TRUE)
FandO= rbind(ratingFage, ratingOikos) #put the results in a dataframe
colnames(FandO) =c ("All nature", "Price",'taste')

# 5. Greek yogurt has at least two distinct usage situations‐‐as a snack and as an ingredient in cooking. 
#Do people who use Greek yogurt for cooking rate higher the importance of allnatural/organic/rbst free? Do they think price is more or less important? 
#What does this suggest about this segment and possible strategies for this segment? 

ratingAttributes = c("Question6Allnatural","Question6Organic", "Question6rbSTfree","Question6Price")
#Find the sample consumers who use greek yogurt for cooking
forCooking = wegmans[wegmans$Question12DoyouuseGreekYogurtforcooking == 'Yes',ratingAttributes]
#Find the sample consumers who eat greek yogurt as mid-morning or afternoon snack
forSnack = wegmans[wegmans$Question20MidMorningsnack == "1"|wegmans$Question20Afternoonsnack == '1',ratingAttributes]


forCooking=as.data.frame(sapply(forCooking,as.factor))
forSnack=as.data.frame(sapply(forSnack,as.factor))

#Revalue 'unsure' to 'NA'
for (i in 1:4) {
  forCooking[[ratingAttributes[i]]]=revalue(forCooking[[ratingAttributes[i]]], c(Unsure=NA))
  forSnack[[ratingAttributes[i]]]=revalue(forSnack[[ratingAttributes[i]]], c(Unsure=NA))
}
forCooking=as.data.frame(sapply(forCooking,as.numeric))
forSnack=as.data.frame(sapply(forSnack,as.numeric))

#Calculate the mean rating for the three attributes under different usage situation
ratingForCooking = colMeans(forCooking[,ratingAttributes], na.rm = TRUE)
ratingForSnack = colMeans(forSnack[,ratingAttributes], na.rm = TRUE)
snackCookingCompare = rbind(ratingForCooking, ratingForSnack) #put the results in a dataframe
colnames(snackCookingCompare) =c ("All nature", "Organic" , "rbsTfree" , "Price")
write.csv(snackCookingCompare, "data.csv")
#ANS: sample consumers who use yogurt to cook consider these three attributes more important than those who eat yogurt as snack

#Whether price is more important or not for cooking group?
#Check statistically different for price ratings
t.test(forCooking$Question6Allnatural,forSnack$Question6Allnatural,paired=FALSE)
t.test(forCooking$Question6Price,forSnack$Question6Price,paired=FALSE)
t.test(forCooking$Question6Organic,forSnack$Question6Organic,paired=FALSE)
t.test(forCooking$Question6rbSTfree,forSnack$Question6rbSTfree,paired=FALSE)

# 6. Flavor insights
#Demand for flavors among the current flavors of yogurt
ite<-sqldf('select distinct("Item.Num"),Sales from randItemSales')
brand<-NewAttributes[c('Item.Num','Brand')]
brandandprice<-merge(brand,ite, by="Item.Num")

Q1.1<-sqldf('select "Item.Num",Sales, Coupon from randItemSales')
Q1.2<-sqldf('select "Item.Num",Brand, Flavor1 from NewAttributes')
Qdata<-merge(Q1.1,Q1.2,by='Item.Num')
Qdata$Sales<-as.numeric(Qdata$Sales)
Qdata$Coupon<-as.numeric(Qdata$Coupon)
Qdata$price<-Qdata$Sales+Qdata$Coupon
Qallprice<-sqldf('select "Flavor1",price from Qdata where price between 0.99 and 2.19')
Q1FlavorAlldata<-sqldf('select "Flavor1", sum(price) as sales from Qallprice group by "Flavor1" order by sum(price) 
                       desc')


Q1brand<- sqldf('select "Flavor1", Brand, sum(price) as sales from Qdata where price between 0.99 and 2.19 group by "Flavor1", Brand order by sum(price) 
                desc')

#Identify future markets
Q2.1<- sqldf('select IncomeBin, "Household.Num" from hshldDemograph')
Q2.2<-sqldf('select "Item.Num","Household.Num","User.ID",Coupon,Sales from survItemSales')
Q2.3<-merge(Q2.1,Q2.2, by='Household.Num')
survResponses$User.ID<-survResponses$ID
Q2.4<-sqldf('select "User.ID",Q12_1_Group,from survResponses')
QIncomeID<-merge(Q2.4,Q2.3,by='User.ID')
QIncomeID$price<-QIncomeID$Sales+QIncomeID$Coupon
QIncomeIDFlavor<-merge(Q1.2,QIncomeID, by='Item.Num')

QIncome150<-sqldf('select Flavor1, IncomeBin, sum(price) as sales from QIncomeIDFlavor where price between 0.99 and 2.19
                  group by Flavor1, IncomeBin')

test<-hist(sqldf('select Q12_4_Group from survResponses where Q12_4_Group != "NA"')$Q12_4_Group,xlab="like",col="yellow",border="blue")
test<-hist(sqldf('select Q12_20_Group from survResponses where Q12_20_Group != "NA"')$Q12_20_Group,xlab="like",col="yellow",border="blue")
test<-hist(sqldf('select Q12_19_Group from survResponses where Q12_19_Group != "NA"')$Q12_19_Group,xlab="like",col="yellow",border="blue")
test<-hist(sqldf('select Q12_3_Group from survResponses where Q12_3_Group != "NA"')$Q12_3_Group,xlab="like",col="yellow",border="blue")
