#Author: Hassan Anees
#PSYC 3250
library(psych)
library(tidyverse)
library(readxl)
library(apaTables)

dataSet <- read_excel("questionare.xlsx")
View(dataSet)

#dataset without ID coloumn
firstSet <- select(dataSet,IM1, IM2,IM3,IM4,IM5,IM6,IM7,IM8,IM9,IM10)

#want to get some descriptive statistics from the dataset
psych::describe(firstSet)

#make a correlation matrix between all the items
#basically because you have an n/a thing, you have an error in analysis
correlation_matrix<-cor(firstSet, use = "complete.obs") 
correlation_matrix

table1<-apa.cor.table(firstSet, filename = "CorTable1.doc", table.number = 2)
table1
#reliability analyses-------------------------------
#look at situations here that if we remove this item, then how much would the alpha change
#average_r is the intercorrelation, the avergae of all the correlations if we deleted item X
#then look at item statistics and look at col raw.r which says that we create a total score
#and we try to corelate item X with the total score (this has the item itself in it),
#r.cor is the one where where the item itself is removed from the total score when correlateing
#r.cor is the one we want to intepret. You can check 
reliability_analysis<- psych::alpha(as.data.frame(firstSet))
reliability_analysis

#Total score creation-------------------------
totalDataset <- mutate(firstSet, NAaverage = (IM1 + IM2 + IM3 + IM4 + IM5 + IM6 + IM7+IM8+IM9+IM10)/10)
View(totalDataset)

totalDescribe <- psych::describe(as.data.frame(totalDataset))
totalDescribe


#######################################################################
#now removing item 2 to increase the raw alpha
secondSet <- select(dataSet,IM1,IM3,IM4,IM5,IM6,IM7,IM8,IM9,IM10)
psych::describe(secondSet)

correlation_matrix_second<-cor(secondSet, use = "complete.obs") 
correlation_matrix_second

reliability_analysis_second<- psych::alpha(as.data.frame(secondSet))
reliability_analysis_second


#####################n
##now removing item 7 to see
thirdSet <- select(dataSet,IM1,IM3,IM4,IM5,IM6,IM8,IM9,IM10)
psych::describe(thirdSet)

correlation_matrix_third<-cor(thirdSet, use = "complete.obs") 
correlation_matrix_third

reliability_analysis_third<- psych::alpha(as.data.frame(thirdSet))
reliability_analysis_third

totalDatasetThird <- mutate(thirdSet, NAaverage = (IM1+IM3+IM4+IM5+IM6+IM8+IM9+IM10)/10)
View(totalDatasetThird)

totalDescribeThird <- psych::describe(as.data.frame(totalDataset))
totalDescribeThird

table2<-apa.cor.table(thirdSet, filename = "CorTable2.doc", table.number = 2)
table2

###########################################

fourSet <- select(dataSet,IM1,IM3,IM4,IM5,IM6,IM8,IM9)
reliability_analysis_fourth<- psych::alpha(as.data.frame(fourSet))
reliability_analysis_fourth


View(thirdSet)
#now trying to do factor analysis
factor_axis<-fa(thirdSet[1:8], nfactors = 3, fm="pa", rotate="varimax", missing = TRUE) 

print(factor_axis, cut = .30)

scree(thirdSet[1:8])

#######################################
# now principal loadings 
#To run principal components
principalData<-principal(thirdSet[1:8], nfactors = 3, rotate="varimax", missing = TRUE) #pricipal specifies principal components
#there is no "fa =" option 
#everything else is same
#to get a scree plot
print(principalData, cut = .30)
thirdSet
scree(principalData[1:8])

#an oblique rotation, this is where you let factors be correlated wiht one another
principal_axis_oblique<-fa(thirdSet[1:8], nfactors = 3, fm="pa", rotate="oblimin", missing = TRUE) 
print(principal_axis_oblique, cut=.30) 











