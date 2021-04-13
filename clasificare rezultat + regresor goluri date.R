library(tidyverse)
library(dplyr)
library(e1071)
library(randomForest)

# ----------DATA PREPROCESSING ---------------------------------------------------------------------------------------

tabel<-read.csv2("wc2018_natlig_euro.csv")

test<-read.csv2("meciuri31.03.csv")


tabel$FTR=factor(as.character(tabel$FTR))

#tabel$HomeTeam=factor(as.character(tabel$HomeTeam))
#tabel$AwayTeam=factor(as.character(tabel$AwayTeam))

tabel$FTHG=as.numeric(tabel$FTHG)
tabel$FTAG=as.numeric(tabel$FTAG)
tabel$OddsHome=as.numeric(tabel$OddsHome)
tabel$OddsDraw=as.numeric(tabel$OddsDraw)
tabel$OddsAway=as.numeric(tabel$OddsAway)

test$FTR=factor(as.character(test$FTR))

#test$HomeTeam=factor(as.character(test$HomeTeam))
#test$AwayTeam=factor(as.character(test$AwayTeam))

test$FTHG=as.numeric(test$FTHG)
test$FTAG=as.numeric(test$FTAG)
test$OddsHome=as.numeric(test$OddsHome)
test$OddsDraw=as.numeric(test$OddsDraw)
test$OddsAway=as.numeric(test$OddsAway)

probHome=1/tabel$OddsHome
probDraw=1/tabel$OddsDraw
probAway=1/tabel$OddsAway

tabelprob=cbind(tabel,probHome,probDraw,probAway)

probHome=1/test$OddsHome
probDraw=1/test$OddsDraw
probAway=1/test$OddsAway


testprob=cbind(test,probHome,probDraw,probAway)

#-----Construire Testare----------------------------------------------------------------------------------------------

library(caTools)

set.seed(123)

split = sample.split(tabelprob$FTR, SplitRatio = 0.8)
training_set = subset(tabelprob, split == TRUE)
test_set = subset(tabelprob, split == FALSE)

training_set=cbind(training_set[,1:2],training_set$FTR, training_set[,9:11])
test_set=cbind(test_set[,1:2],test_set$FTR,test_set[,9:11])

# ---- Testare CLASIFICATOR SOLIST ------------------------------------------------------------------------------------------



set.seed(123)

#clasificator svm

classifier = svm(formula = training_set$`training_set$FTR` ~ probHome +probAway,
                 data = training_set,
                 type = 'nu-classification',
                 kernel = 'sigmoid')

#clasificator naiveBayes

classifier=naiveBayes(x=training_set[,-3],
                      y=training_set[,3])

#clasificator randomForest

classifier=randomForest(x=training_set[,-3],
                      y=training_set[,3],
                      ntree=500)

y_pred = predict(classifier, newdata = test_set[-3])

#y_pred2=predict(classifier, newdata=training_set[-1])

# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)
#cm2=table(training_set[,1],y_pred2)

cm

#------CLASIFICARE SOLIST----------------------------------------------------------------------------------------------

test_set2=bind_cols(testprob[,1:2], testprob[,5:11])
test_set2=cbind(test_set2[,1:3],test_set2[,7:9])

tabel2=bind_cols(tabelprob[,1:2], tabelprob[5:11])
tabel2=cbind(tabel2[,1:3],tabel2[,7:9])

#clasificator svm

classifier2 = svm(formula = FTR ~ probHome+probDraw+probAway,
                 data = tabel2
                 )

#clasificator naiveBayes

classifier2=naiveBayes(x=tabel2[,-3],
                      y=tabel2$FTR)

#clasificator randomForest
classifier2=randomForest(x=tabel2[,-3],
                       y=tabel2$FTR, ntree=1000)

#decision tree
#library(rpart)
classifier2 = rpart(formula = FTR~probHome+probDraw+probAway,
                   data = tabel2)
{
# Predicting the Test set results
y_pred2 = predict(classifier2, newdata = test_set2[-3], type = 'class')
}


# Predicting the Test set results
y_pred2 = predict(classifier2, newdata = test_set2[-3])

y_pred2

test_set3=bind_cols(test[,1:4],y_pred2,test[,5:8],testprob[,9:11])


#testare edge din predictie nn

}

# ----- Testare REGRESOR GOLURI DATE ------------------------------------------------------------------------------------------

training_set=bind_cols(training_set[,1:4],training_set[,6:8])
test_set=bind_cols(test_set[,1:4],test_set[,6:8])

trainhome=training_set[,-4]
trainaway=training_set[,-3]
testhome=test_set[,-4]
testaway=test_set[,-3]

regressorhome = svm(formula = FTHG~probHome+probAway, data=trainhome,
                    type = 'eps-regression',
                    kernel = 'radial')

regressoraway = svm(formula=FTAG~probHome+probAway, data=trainaway,
                    type = 'eps-regression',
                    kernel = 'radial')

# Predicting the Test set results
predhome = predict(regressorhome, newdata = testhome[,-3])
predaway = predict(regressoraway, newdata = testaway[,-3])


predh<-ifelse(predhome<0.5,0,ifelse(predhome<1.5,1,ifelse(predhome<2.5,2,ifelse(predhome<3.5,3,ifelse(predhome<4.5,4,ifelse(predhome>4.5,5,NA))))))
preda<-ifelse(predaway<0.5,0,ifelse(predaway<1.5,1,ifelse(predaway<2.5,2,ifelse(predaway<3.5,3,ifelse(predaway<4.5,4,ifelse(predaway>4.5,5))))))

 
#View(predh)
#View(preda)
 
df<-cbind(test_set[,1:4],predh,preda,predhome,predaway)
#View(df)

 kh <- vector(mode="numeric", length=nrow(test_set))
 ka <- vector(mode="numeric", length=nrow(test_set))

 df2<-data.frame(df,kh,ka)

for (i in 1:nrow(df2))
{
  if(df2$FTHG[i]==df2$predh[i])
    {df2$kh[i]=1}

}

for (i in 1:nrow(df))
{
  if(df2$FTAG[i]==df2$preda[i])
  {df2$ka[i]=1}

}

sum(df2$kh)
sum(df2$ka)

x=0

for (i in 1:nrow(df))
{
  if(df2$kh[i]==1 && df2$ka[i]==1)
  {x=x+1}

}

x

 dfscor=df %>% filter(
   df2$kh==1 & df2$ka==1
 )

 dean<-bind_cols(test_set,predh,preda,predhome,predaway)

# ------- Analiza pe Meciuri -------------------------------------------------------------------------------------------

 tabel2=tabelprob[,-5]
 test2=testprob[,-5]
 
 trainhome=cbind(tabel2[,1:3],tabel2[,8:10])
 trainaway=cbind(tabel2[,1:4],tabel2[,8:10])
 trainaway=trainaway[,-3]
 testhome=cbind(test2[,1:3],test2[,8:10])
 testaway=cbind(test2[,1:4],test2[,8:10])
 testaway=testaway[,-3]
 

 
 regressorhome3 = glm(formula = FTHG~probHome+probAway, data=trainhome,
                     )
                     
 
 regressoraway3 = glm(formula= FTAG~probHome+probAway, data=trainaway,
                      )
 
 # Predicting the Test set results
 predhome = predict(regressorhome3, newdata = testhome[,-3])
                   
 predaway = predict(regressoraway3, newdata = testaway[,-3])
 
 
                    
 
 
 predh<-ifelse(predhome<0.5,0,ifelse(predhome<1.5,1,ifelse(predhome<2.5,2,ifelse(predhome<3.5,3,ifelse(predhome<4.5,4,ifelse(predhome>4.5,5,NA))))))
 preda<-ifelse(predaway<0.5,0,ifelse(predaway<1.5,1,ifelse(predaway<2.5,2,ifelse(predaway<3.5,3,ifelse(predaway<4.5,4,ifelse(predaway>4.5,5,NA))))))
 
 
#construire dataframe pentru testare
 df_svm_2<-cbind(test[,1:2],predh,preda,predhome,predaway,test[,6:8])

 
 

