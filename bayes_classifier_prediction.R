library(tidyverse)
library(dplyr)

# ----------DATA PREPROCESSING ---------------------------------------------------------------------------------------

e0<-read.csv("E0.csv")
e02019<-read.csv("E0 (1).csv")

tabel=bind_rows(e02019,e0) 

tabel$FTR=as.factor(tabel$FTR)

tabel$HTR=as.factor(tabel$HTR)

tabel$HomeTeam=as.factor(tabel$HomeTeam)
tabel$AwayTeam=as.factor(tabel$AwayTeam)

summary(tabel)

tabel=tabel[,-4]

train=tabel[1:605,]
test=tabel[,]

x=c("Wolves", "Leicester", "Crystal Palace", "Man City", "Brighton")
y=c("Southampton", "Liverpool", "Burnley", "Tottenham", "Aston Villa")
z=as.factor(as.character(c("A","H","A","H","D")))

test2=as.data.frame(cbind(HomeTeam=x, AwayTeam =y, FTR=z))

library(e1071)

classifier = naiveBayes(x = train[-3],
                        y = train$FTR)


y_pred = predict(classifier, newdata = test2[-3])


cm = table(test2[, 3], y_pred)

y_pred
cm




