library(cplm)
library(dplyr)
library(caret)

data('AutoClaim')
summary(AutoClaim)
hist(AutoClaim$CLM_FREQ5)
hist(AutoClaim$CLM_AMT5) #Right skewed
hist(AutoClaim$CLM_AMT) #Right skewed
hist(AutoClaim$KIDSDRIV) #Very right skewed
hist(AutoClaim$TRAVTIME) #Normalized
summary(AutoClaim$CAR_USE)
table(AutoClaim$CAR_TYPE)
table(AutoClaim$CAR_USE,AutoClaim$CAR_TYPE)
summary(AutoClaim$BLUEBOOK)
hist(AutoClaim$BLUEBOOK) #Right Skewed
hist(log(AutoClaim$BLUEBOOK)) #Near Normalized
summary(AutoClaim$RETAINED)
hist(AutoClaim$RETAINED) #Right Skewed
hist(AutoClaim$NPOLICY) #Gapped right skewed

table(AutoClaim$RED_CAR,AutoClaim$CLM_FLAG) #No significance seen of red color
table(AutoClaim$REVOLKED)
table(AutoClaim$CLM_FLAG)
table(AutoClaim$REVOLKED,AutoClaim$CLM_FLAG) # No significance of Revokation licence
hist(AutoClaim$MVR_PTS) #Right skewed and discontinuous
summary(AutoClaim$MVR_PTS)
hist(AutoClaim$AGE) #Normalized
hist(AutoClaim$HOMEKIDS) #Breaks
hist(AutoClaim$YOJ) # skew + ormalize at gap
hist(AutoClaim$INCOME) #Slightly right skewed (NAs also there) For NA, avg of prefession and age can be taken
table(AutoClaim$GENDER)
table(AutoClaim$GENDER,AutoClaim$CAR_TYPE)
table(AutoClaim$MARRIED)
table(AutoClaim$MARRIED,AutoClaim$CLM_FLAG)
table(AutoClaim$PARENT1,AutoClaim$MARRIED)
table(AutoClaim$JOBCLASS)
table(AutoClaim$JOBCLASS,AutoClaim$CLM_FLAG)
table(AutoClaim$MAX_EDUC)
AutoClaim[AutoClaim$MAX_EDUC=="<High School","MAX_EDUC"] <- "High School"
AutoClaim$MAX_EDUC <- droplevels(AutoClaim$MAX_EDUC)
hist(AutoClaim$HOME_VAL) #Right skewed
hist(AutoClaim$SAMEHOME) # Right Skewed
table(AutoClaim$AREA)
table(AutoClaim$AREA,AutoClaim$CLM_FLAG)
table(AutoClaim$JOBCLASS,AutoClaim$AREA)

#CHecking YOJ for Homemaker and Student
by(AutoClaim$YOJ,AutoClaim$JOBCLASS,summary)
pairs(AutoClaim[sapply(AutoClaim,is.numeric)])

plot(AutoClaim$AGE,AutoClaim$YOJ)
plot(AutoClaim$YOJ,AutoClaim$AGE)

#Replacing Null Values
#Income
by(AutoClaim$INCOME,AutoClaim$JOBCLASS,summary)
hist(AutoClaim[AutoClaim$JOBCLASS=="Professional","INCOME"])
hist(AutoClaim[AutoClaim$JOBCLASS=="Professional" & AutoClaim$MAX_EDUC=="High School","INCOME"])

AutoClaim_income_group <- summarise(group_by(AutoClaim,JOBCLASS)
          ,median_income=median(INCOME[!is.na(INCOME)]))

for(i in 1:nrow(AutoClaim_income_group)){
AutoClaim[AutoClaim$JOBCLASS==AutoClaim_income_group$JOBCLASS[i] & is.na(AutoClaim$INCOME),"INCOME"] <- AutoClaim_income_group$median_income[i]

}


#YOJ, HOMEVAL,SAME_HOME- replacing with 0
AutoClaim[is.na(AutoClaim$YOJ),"YOJ"] <- 0
AutoClaim[is.na(AutoClaim$HOME_VAL),"HOME_VAL"] <- 0
AutoClaim[is.na(AutoClaim$SAMEHOME),"SAMEHOME"] <- 0

table(traindata$CLM_FLAG)
#Feature Engineering
AutoClaim$CLM_AMT5_BKT <- cut(AutoClaim$CLM_AMT5,c(-1,0,5000,10000,30000,50000,100000),labels=c("A","B","C","D","E","F"))
AutoClaim$LOG_BLUEBOOK <- log(AutoClaim$BLUEBOOK)
AutoClaim$MVR_PTS_BKT <- cut(AutoClaim$MVR_PTS,c(-1,0,5,20),labels=c("A","B","C"))

#Splitting in Train and Test
table(AutoClaim$CLM_FLAG)
intrain<-createDataPartition(y=AutoClaim$CLM_FLAG,
                             p=0.50,list=FALSE)

traindata<-AutoClaim[intrain,]
testdata<-AutoClaim[-intrain,]

#LOgistic Regression
model <- glm(CLM_FLAG~AGE + LOG_BLUEBOOK + CLM_AMT5_BKT + MVR_PTS_BKT + TRAVTIME + RETAINED + NPOLICY + GENDER + JOBCLASS + MARRIED + MAX_EDUC + AREA + REVOLKED,data=traindata,family=binomial)

#Prediction
testdata$probability <- predict(model,testdata,type='response')
testdata$CLASSIFICATION <- NA

for(i in 1:nrow(testdata)){
testdata$CLASSIFICATION[i] <- ifelse(testdata$probability[i]>mean(testdata$probability),"YUP","NOPE")
}

table(testdata$CLASSIFICATION,testdata$CLM_FLAG)

sum(testdata[testdata$CLASSIFICATION=="YUP" & testdata$CLM_FLAG=="Yes","CLM_AMT"])

sum(testdata$CLM_AMT)

ratio_claim_caught <- sum(testdata[testdata$CLASSIFICATION=="YUP" & testdata$CLM_FLAG=="Yes","CLM_AMT"])/sum(testdata$CLM_AMT)
ratio_claim_caught
