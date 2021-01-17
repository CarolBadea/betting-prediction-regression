

premier1<-read.csv2("premierlig2021.csv")


premier=premier1[,-1]


data <- data.frame(apply(premier, 2, function(x) as.numeric(as.character(x))))

datahome=cbind(data[,2:11],data[23:25])

dataaway=data[13:25]

datahome=datahome[,-7]
dataaway=dataaway[,-7]


# datahome_std=scale(datahome)
# datahome_std=as.data.frame(datahome_std)

x<-lm(formula = Gfh~., data=datahome)#goluri date acs
x2<-lm(formula = Gfa~., data=dataaway)#goluri date deplasare

summary(x)
summary(x2)
summary(x3)
summary(x4)

y=predict(x, newdata = data)
y2=predict(x2,newdata = data)


x3<-lm(formula = Gah~., data=datahome)
x4<-lm(formula=Gaa~.,data=dataaway)

y3=predict(x3, newdata=data)#goluri luate acs
y4=predict(x4, newdata=data)#goluri luate deplasare

z=y/premier$Mph
z2=y2/premier$Mpa
z3=y3/premier$Mph
z4=y4/premier$Mpa

matrice_pred=cbind(z,z2,z3,z4)

coefhome=z*z4
coefaway=z2*z3

#echipele care joaca in meciul analizat
home=11
away=13

a0<-matrix(dpois(x=0:8, lambda = coefhome[home])*dpois(x=0, lambda = coefaway[away])*100)
a1<-matrix(dpois(x=0:8, lambda = coefhome[home])*dpois(x=1, lambda = coefaway[away])*100)
a2<-matrix(dpois(x=0:8, lambda = coefhome[home])*dpois(x=2, lambda = coefaway[away])*100)
a3<-matrix(dpois(x=0:8, lambda = coefhome[home])*dpois(x=3, lambda = coefaway[away])*100)
a4<-matrix(dpois(x=0:8, lambda = coefhome[home])*dpois(x=4, lambda = coefaway[away])*100)
a5<-matrix(dpois(x=0:8, lambda = coefhome[home])*dpois(x=5, lambda = coefaway[away])*100)
a6<-matrix(dpois(x=0:8, lambda = coefhome[home])*dpois(x=6, lambda = coefaway[away])*100)
a7<-matrix(dpois(x=0:8, lambda = coefhome[home])*dpois(x=7, lambda = coefaway[away])*100)
a8<-matrix(dpois(x=0:8, lambda = coefhome[home])*dpois(x=8, lambda = coefaway[away])*100)

tabelpoisson = matrix(c(a0,a1,a2,a3,a4,a5,a6,a7,a8), nrow=9, ncol=9)


sum(tabelpoisson)

doi<-sum(tabelpoisson[upper.tri(tabelpoisson)])
unu<-sum(tabelpoisson[lower.tri(tabelpoisson)])
ics<-sum(diag(tabelpoisson))

unu
ics
doi

print(tabelpoisson)
formatC(tabelpoisson, format = 'f', digits = 2)

cota1=1/(unu/100)
cotax=1/(ics/100)
cota2=1/(doi/100)

print(cota1)
print(cotax)
print(cota2)

