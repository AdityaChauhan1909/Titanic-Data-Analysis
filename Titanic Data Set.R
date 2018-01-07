
install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)

load("titanic.raw.rdata")
View(titanic.raw)
summary(titanic.raw)

t <- titanic.raw 

## Percentage of people that survived

PeopleSurvived <- ((length(grep("Yes",t$Survived)))/ length(t$Survived))
PeopleSurvived

#compute the percentage of people that were children
PeopleChildren <- ((length(grep("Child",t$Age)))/ length(t$Age)) 
PeopleChildren

#Percentage of People that were female
PeopleFemale<- ((length(grep("Female",t$Sex)))/ length(t$Sex)) 
PeopleFemale

#compute the percentage of people that were in first class
PeopleClass<-  ((length(grep("1st",t$Class)))/ length(t$Class) )
PeopleClass

# What percentage of children survived?

ChildrenSurvived <- nrow(t[t$Survived=="Yes" & t$Age=="Child",])/nrow(t[t$Age=="Child",])
ChildrenSurvived

#What percentage of female survived?
FemaleSurvived <- nrow(t[t$Survived=="Yes" & t$Sex=="Female",])/nrow(t[t$Sex=="Female",])
FemaleSurvived

#	What percentage of first class people survived?

FirstClassPeopleSurvived <- nrow(t[t$Survived=="Yes" & t$Class=="1st",])/nrow(t[t$Class=="1st",])
FirstClassPeopleSurvived

#What percentage of 3rd class people survived?

ThirdClassPeopleSurvived <- nrow(t[t$Survived=="Yes" & t$Class=="3rd",])/nrow(t[t$Class=="3rd",])
ThirdClassPeopleSurvived

#	Write a function that returns the a new dataframe of people that satisfy 
#the specified criteria of sex, age, class and survived as parameters

Peopledf <- function(sex,age,class,survived){
  
  df<- t[(t$Sex==sex)&(t$Age==age)&(t$Class==class)&(t$Survived==survived),]
  df
}

Peopledf('Male','Adult','2nd','No')

#2)	Write a function, using the previous function, that calculates
#the percentage (who lives, who dies) for a specified (parameters) of age, class and sex.

PercentageFunction <- function(sex,age,class,survived){
  df.new <- Peopledf(sex,age,class,survived)
  PeopleLived <- nrow(df.new[df.new$Survived==survived,])/nrow(t)
  PeopleLived
}

PercentageFunction('Male','Adult','2nd','No')


# 3)	Use the function to compare age & 3rd class male survival rates

#For child    
Peopledf('Male','Child','3rd','Yes')
PercentageFunction('Male','Child','3rd','Yes')

#For Adult
Peopledf('Male','Adult','3rd','Yes')
PercentageFunction('Male','Adult','3rd','Yes')

#4)	Use the function to compare age & 1st class female survival rates

#For Child
Peopledf('Female','Child','1st','Yes')
PercentageFunction('Female','Child','1st','Yes')

#ForAdult
Peopledf('Female','Adult','1st','Yes')
PercentageFunction('Female','Adult','1st','Yes')

#	Use arules to calculate some rules (clusters) for the titanic dataset

ruleset <- apriori(t,parameter=list(support=0.01,confidence=0.5),appearance=list(default="lhs",rhs=("Survived=Yes")))
#Summary of the plot
summary(ruleset)
#visualize the plot
plot(ruleset)

goodrules <- ruleset[quality(ruleset)$lift > 2 ]
goodrules
inspect(goodrules)
goodrules <- sort(goodrules,by='lift',decreasing=T)
inspect(goodrules)
plot(goodrules)

##Support factor = 0.005 , confidence = 0.5
ruleset2 <- apriori(t,parameter=list(support=0.005,confidence=0.5),appearance=list(default="lhs",rhs=("Survived=Yes")))
summary(ruleset2)

goodrules2 <- ruleset2[quality(ruleset2)$lift > 2 ]
goodrules2 <- sort(goodrules2,by='lift',decreasing=T)
inspect(goodrules2)

plot(ruleset2)
plot(goodrules2)

## 3 most interesting and useful rules
#Class=2nd,Sex=Female,Age=Child ( have confidence = 1.0000000)
#Class=1st,Sex=Female,Age=Adult ( common in both ruleset)
#Class=2nd,Sex=Female,Age=Adult ( common in both ruleset)


#4)	How does this compare to the descriptive analysis we did on the same dataset? 

# From the total population 4.9 % ( 0.04952294 ) were children and 21.3 % (0.2135393) were female


#Also of the total population of children on board 52.29358% children survived and 73.19149% females survived
#Children and Female survivor were in majority

# I took the best three Associative Mining rules mentioned above and is in sync with the descriptive analysis done in question 1 and 2
#Apriori suggested that major survivors are 2nd class females(both adults and childs) and 1st class Female Adults.

#Hence the rules filtered and the descriptive analysis done are in compliance with each other support each other strongly






