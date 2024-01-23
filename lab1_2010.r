EPI_data <- read.csv("PATH/2010EPI_data.csv")
View(EPI_data)
attach(EPI_data)  
fix(EPI_data) 
EPI
tf <- is.na(EPI) 
E <- EPI[!tf] 
E

summary(EPI)
fivenum(EPI,na.rm=TRUE)
tf <- is.na(EPI) 

stem(EPI)
hist(EPI)
hist(EPI, seq(30., 95., 1.0), prob=TRUE)

lines(density(EPI,na.rm=TRUE,bw=1.))
rug(EPI) 


plot(ecdf(EPI), do.points=FALSE, verticals=TRUE) 

par(pty="s")
qqnorm(EPI); qqline(EPI)
x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for tdsn")
qqline(x)
#------------------------------------------------------------------------------#
DALY
summary(DALY)
fivenum(DALY,na.rm=TRUE)
tf <- is.na(DALY) 

stem(DALY)
hist(DALY)
hist(DALY, seq(0., 92., 1.0), prob=TRUE)
lines(density(DALY,na.rm=TRUE,bw=1.))
rug(DALY) 

plot(ecdf(DALY), do.points=FALSE, verticals=TRUE) 

par(pty="s")
qqnorm(DALY); qqline(DALY)
x<-seq(0,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for tdsn")
qqline(x)

#------------------------------------------------------------------------------#
WATER_H
summary(WATER_H)
fivenum(WATER_H,na.rm=TRUE)
tf <- is.na(WATER_H) 

stem(WATER_H)
hist(WATER_H)
hist(WATER_H, seq(0., 100., 1.0), prob=TRUE)
lines(density(WATER_H,na.rm=TRUE,bw=1.))
rug(WATER_H) 

plot(ecdf(WATER_H), do.points=FALSE, verticals=TRUE) 

par(pty="s")
qqnorm(WATER_H); qqline(WATER_H)
x<-seq(0,100,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for tdsn")
qqline(x)
#------------------------------------------------------------------------------#
boxplot(EPI,DALY) 
qqplot(EPI,DALY)

#----------------------intercompare--------------------------------------------#
#ENVHealth
boxplot(EPI,ENVHEALTH)
qqplot(EPI,ENVHEALTH)
#Ecosystem
boxplot(EPI,ECOSYSTEM)
qqplot(EPI,ECOSYSTEM)
#AIR_H
boxplot(EPI,AIR_H)
qqplot(EPI,AIR_H)
#WATER_H
boxplot(EPI,WATER_H)
qqplot(EPI,WATER_H)
#AIR_EWATER_E
boxplot(EPI,AIR_E)
qqplot(EPI,AIR_E)
#BIODIVERSITY
boxplot(EPI,BIODIVERSITY)
qqplot(EPI,BIODIVERSITY)

#------------------------------------------------------------------------------#
EPILand<-EPI[!Landlock]
Eland <- EPILand[!is.na(EPILand)]
fivenum(Eland,na.rm=TRUE)
hist(Eland)
hist(Eland, seq(0., 95., 1.0), prob=TRUE)
lines(density(Eland,na.rm=TRUE,bw=1.))
rug(Eland) 

plot(ecdf(Eland), do.points=FALSE, verticals=TRUE) 
par(pty="s")
qqnorm(Eland); qqline(Eland)
x<-seq(0,100,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for tdsn")
qqline(x)
#------------------------------------------------------------------------------#
EPI_data <- read.csv("PATH/2010EPI_data.csv")
attach(EPI_data)  
EPI_South_Asia_G <- EPI[GEO_subregion == 'South Asia']
EPI_South_Asia_E <- EPI[EPI_regions == 'South Asia']
EPI_South_Asia <- union(EPI_South_Asia_E,EPI_South_Asia_G)
EPI_South_Asia



