remove(list=ls()) #Eliminar todos los objetos de la memoria
graphics.off()

#Lectura de datos confirmados y decesos, y conversión a vectores

casos<-read.csv("casos diarios edo nal 240623.csv")
casos.nal<-as.numeric(casos[33,4:1217]) #27/02/20-24/06/23
casos.nal.20<-as.numeric(casos[33,4:312]) #27/02/20-31/12/20
casos.nal.21<-as.numeric(casos[33,313:677]) #01/01/21-31/12/21
casos.nal.24jun23<-as.numeric(casos[33,678:1217]) #01/01/22-24/06/23
plot(casos.nal,type="l",col="red")
#par(mfrow=c(1,2))
tiempo<-seq(as.Date("27/02/2020",format="%d/%m/%Y"),
            as.Date("30/06/2023",format="%d/%m/%Y"),length=1214)
plot(tiempo,casos.nal,type="l", lwd=2, col="red",
     #xlab="UNIT OF TIME: DAYS",
     xlab="UNIDAD DE TIEMPO: DIAS",
     #ylab= "NUMBER OF CONFIRMED CASES",
     ylab= "NÚMERO DE CASOS CONFIRMADOS",
     main= "CONFIRMADOS DIARIOS DE COVID-19 EN MÉXICO DEL 27/02/2020 AL 30/06/2023")
     #main = "CONFIRMED DAILY COVID-19 CASES IN MEXICO FROM 02/27/2020 TO 06/30/2023")
     grid(col="gray")
     #Distinción de olas
     lines(tiempo[1:210],casos.nal[1:210],col="green",lwd=2) #19/02/20 - 23/09/20
     lines(tiempo[211:430],casos.nal[211:430],col="magenta",lwd=2) #24/09/20 - 01/05/21
     lines(tiempo[431:641],casos.nal[431:641],col="orange",lwd=2) #02/05/21 - 28/11/21
     lines(tiempo[642:780],casos.nal[642:780],col="red",lwd=2) #29/11/21 - 16/04/22
     lines(tiempo[781:970],casos.nal[781:970],col="azure4",lwd=2) #17/04/21 - 23/10/22 
     lines(tiempo[971:1214],casos.nal[971:1214],col="coral2",lwd=2) #24/10/22 - 30/06/23
     legend(x="topleft",
            legend = c("1a ola","2a ola","3a ola","4a ola","5a ola","6a ola"),
           #legend = c("1st wave","2nd wave","3rd wave","4th wave","5th wave","6th wave"),
            col=c("green","magenta","orange","red","azure4","coral2"),
            pch=c(15,15,15,15,15,15),bty="n",cex=0.95)
     hist(log(casos.nal)/max(log(casos.nal)),
          main = "Datos normalizados de Log(Confirmados) años 2020-2022",
          ylab="Frecuencia",xlab="Normalización de Log(Confirmados)")
#Confirmados por año, en ventana anual
tiempo<-seq(as.Date("01/01/2020",format="%d/%m/%Y"),
            as.Date("31/12/2020",format="%d/%m/%Y"),length=366)
casosnal20<-c(rep(0,57),casos.nal.20) #31 dias enero+26 dias feb = 57 dias
plot(tiempo,casosnal20,type="l",ylim=c(0,81000),
     col="#8B4815",lwd=2,
     xlab="Unidad de tiempo: Dias",
     ylab= "Número de confirmados",
     main="Aparición de valores pico durante cada año de confirmados diarios")
    grid(col="blue")   
lines(tiempo[1:365],casos.nal.21,col="#8B008B",lwd=2)
lines(tiempo[1:365],casos.nal.22feb23[1:365],col="red",lwd=2)
lines(tiempo[1:59],casos.nal.22feb23[366:424],col="orange",lwd=2)
legend(x="topright",legend = c("Año 2020","Año 2021","Año 2022","Ene-Feb 2023"),
       col=c("#8B4815","#8B008B","red","orange"),lty=c(1,1,1,1),lwd=c(2,2,2,2))
#1er pico: 20/07/2020 con 9133 casos ese dia
points(tiempo[202],9133, pch=16, col="#8B4815",cex=2)
#2o pico: 11/01/2021 con 21044 casos ese dia
points(tiempo[11],21044, pch=16, col="#8B008B",cex=2)
#3er pico: 09/08/2021 con 25605 casos ese dia
points(tiempo[221],25605, pch=16, col="#8B008B",cex=2)
#4o pico: 17/01/2022 con 81279 casos ese dia
points(tiempo[17],81279, pch=16, col="red",cex=2)
#5o pico: 11/07/2022 con 44471 casos ese dia
points(tiempo[192],44471, pch=16, col="red",cex=2)
#6o pico: 19/12/2022 con 6704 casos ese dia
points(tiempo[354],6704, pch=16, col="red",cex=2)

#suma  de casos diarios de cada mes 01ene20-28feb23
casos.mes<-c(0,8,3122,29650,92470,158886,212274,170819,147468,172147,206038,
             329996,424394,195234,143254,93647,65871,107135,381611,523222,
             273774,124864,82496,120519,1357056,292940,49420,16767,37905,
             321837,752595,190715,37218,13073,27596,116436,118214,75554)
plot(casos.mes[1:12],type="l",ylim=c(0,1400000),col="#8B4815",
     lwd=2,xlab="Unidad de tiempo: Meses", ylab="Casos confirmados",
     main="Suma mensual de casos confirmados, años 2020-2022 y Ene-Feb 2023")
     grid(col="blue")
lines(casos.mes[13:24],col="#8B008B",lwd=2)
lines(casos.mes[25:36],col="red",lwd=2)
lines(casos.mes[37:38],col="orange",lwd=2)
legend(x="topright",legend = c("Año 2020","Año 2021","Año 2022","Ene-Feb 2023"),
       col=c("#8B4815","#8B008B","red","orange"),lty=c(1,1,1,1),lwd=c(2,2,2,2))
points(7,212274, pch=16, col="#8B4815",cex=2)
points(1,424394, pch=16, col="#8B008B",cex=2)
points(8,523222, pch=16, col="#8B008B",cex=2)
points(1,1357056, pch=16, col="red",cex=2)
points(7,752595, pch=16, col="red",cex=2)
points(12,116436,pch=16, col="red",cex=2)
points(1,118214,pch=18,col="orange",cex=1)
points(2,75554,pch=18,col="orange",cex=1)

#Meses de mayor intensidad de la pandemia en México, a 28/02/23
casos.mes.ts<-ts(casos.mes,start=c(2020,1),end=c(2023,2),freq=12)
#plot(casos.mes.ts,type="l")
medias<-boxplot(casos.mes.ts ~ cycle(casos.mes.ts),
        xlab="Meses del año", col="orange", 
        ylab="Número de confirmados",names=c("E","F","Mr","Ab","My","J","Jl",
                                             "Ag","S","O","N","D"))

#http://software-tecnico-libre.es/es/articulo-por-tema/analisis-de-datos/
#analisis-r/redes-neuronales-con-r/redes-neuronales-recurrentes-y-
#series-de-tiempo
library(RSNNS)
library(quantmod)

casos<-read.csv("casos diarios edo nal 130323.csv")
casos.nal<-as.numeric(casos[33,4:1101]) #27/02/20 - 28/02/23
casosnal<-casos.nal/max(casos.nal)
slog<-as.ts(casosnal)
train<-1:1039 #índices para los datos 1 a 1039 (hasta 31/12/22)
y<-as.zoo(slog)
x1<-Lag(y,k=1)
x2<-Lag(y,k=2)
x3<-Lag(y,k=3)
x4<-Lag(y,k=4)
x5<-Lag(y,k=5)
x6<-Lag(y,k=6)
x7<-Lag(y,k=7)
x8<-Lag(y,k=8)
x9<-Lag(y,k=9)
x10<-Lag(y,k=10)
slog<-cbind(y,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10)
slog<-slog[-(1:10),]
inputs<-slog[,2:11]
outputs<-slog[,1]
fit<-elman(inputs[train],
           outputs[train],
           size=c(3,3),
           learnFuncParams=c(0.01),
           maxit=1000)
y<-as.vector(outputs[-train]) #datos 1040 a 1098

tiempo<-seq(as.Date("01/02/2023",format="%d/%m/%Y"),
            as.Date("28/02/2023",format="%d/%m/%Y"),length=28)
plot(tiempo[11:28],y[32:49],type="l",lwd=2,col="blue", xlab="Unidad de tiempo: Dias",
     ylab="Normalización de casos confirmados",ylim = c(0,0.1))
points(tiempo[11:28],casosnal[1081:1098])
grid(col="gray")
legend(x="topright",legend = c("Datos","Elman","Jordan"),
       col=c("blue","green","red"),lty=c(1,1,1),lwd=c(2,2,2),horiz = T)
#points(tiempo,casosnal[1041:1089]) #Datos 1041 a 1089

predElm<-predict(fit,inputs[-train])
lines(tiempo[11:28],predElm[32:49],col="green",lwd=2) #gráfica de pronóstico por Elman

fitJ<-jordan(inputs[train],
            outputs[train],
            size=4,
            learnFuncParams=c(0.01),
            maxit=1000)

predJ<-predict(fitJ,inputs[-train])
lines(tiempo[11:28],predJ[32:49],col="red",lwd=2) #gráfica de pronóstico por Jordan
par(mfrow=c(2,1))
plotIterativeError(fit)
plotIterativeError(fitJ)

###################
#Del libro Deep Learning Made Easy with R, ejemplo Red Elman
require(RSNNS)
require(quantmod)
data("UKLungDeaths",package ="datasets")
par( mfrow =c(3 ,1))
plot(ldeaths, xlab=" Year", ylab ="Both sexes", main ="Total")
plot( mdeaths , xlab ="Year",ylab ="Males", main ="Males")
plot( fdeaths , xlab ="Year",ylab ="Females",main ="Females")
sum(is.na( ldeaths))
par(mfrow = c(3, 1))
plot(ldeaths)
x<-density(ldeaths)
plot(x,main ="UK total deaths from lung diseases")
polygon (x, col="green", border ="black")
boxplot(ldeaths,col="cyan",ylab ="Number of deaths per month")
y<-as.ts(ldeaths)
y<-log(y)
y<- as.ts(scale(y))
y<-as.zoo(y)
x1 <-Lag (y, k = 1)
x2 <-Lag (y, k = 2)
x3 <-Lag (y, k = 3)
x4 <-Lag (y, k = 4)
x5 <-Lag (y, k = 5)
x6 <-Lag (y, k = 6)
x7 <-Lag (y, k = 7)
x8 <-Lag (y, k = 8)
x9 <-Lag (y, k = 9)
x10 <-Lag (y, k = 10)
x11 <-Lag (y, k = 11)
x12 <-Lag (y, k = 12)
deaths <-cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12)
deaths <-cbind(y,deaths)
deaths <- deaths [-(1:12),]
n=nrow(deaths)
set.seed(465)
n_train <- 45
train <- sample(1:n,n_train,FALSE)
inputs <- deaths[,2:13]
outputs <- deaths[,1]
fit <- elman(inputs[train], outputs[train],
               size =c(1,1),
               learnFuncParams = c(0.1),
               maxit =1000)


############################################

#Suavizado exponencial simple y Holter
library(forecast)
plot(u,type = "l",col="gray")
fit_ses<-ses(logcasosnal,h=3,initial = "simple")
fit_ses
lines(fit_ses$fitted,col="green")
fit_h<-holt(logcasosnal,h=3)
fit_h
lines(fit_h$fitted,col="blue")

#Suavizado con Savitzky Golay
library(signal)
sg<-filter(sgolay(3,21,0),u)
lines(sg,col="red")

#Suavizado con media móvil exponencial
library(pracma)
maexp <- movavg(logcasosnal, n=7, type='e')
lines(maexp,type = "l",col="yellow",lwd=2)


#Uso de suavizado Savitsky Golay a los datos 
library(signal)
sg<-filter(sgolay(3,21,0),casos.nal.20)
plot(casos.nal.20,col="red",type="l",
xlab="tiempo (dias)",ylab= "Confirmados diarios",
main="Confirmados por Covid-19 a nivel nacional: Implementación de suavizado")
lines(sg,col="blue",lwd=2)
legend(x="topleft",legend = c("Datos (Nacional)","Savitzky Golay"),
       col=c("red","blue"),lty=c(1,1),lwd=c(1,2))



#Suavizado exponencial simple
#install.packages("forecast")
library(forecast)
library(fpp2)
#y<-ts(feb.may20,start=1,freq=1)
tiempo<-seq(as.Date("01/01/2020",format="%d/%m/%Y"),
                  as.Date("31/12/2020",format="%d/%m/%Y"),length=366)
plot(tiempo,casos.nal.20,type="l",ylim=c(0,81000))
lines(tiempo[1:365],casos.nal.21,col="red")
lines(tiempo[1:304],casos.nal.22,col="green")
fit_ses<-ses(feb.may20,h=1,initial = "simple",alpha=0.6)
fit_ses
lines(tiempo,fit_ses$fitted[1:95],col="green")
fit_h<-holt(feb.may20,h=1,initial = "simple")
fit_h
lines(tiempo,fit_h$fitted[1:95],col="blue")

library(signal)
sg<-filter(sgolay(3,309,0),casos.nal/max(casos.nal))
lines(sg,col="green")

#############
library(quantmod)
library(tseries)
library(fImport)
## ASIMETRÍA
skewness(c2-c1)
set.seed (1)

#generar una lista de 100 variables aleatorias distribuidas normalmente
datos <- rnorm (100, 0, 1)

#conduct prueba de Ljung-Box
Box.test(u,  lag = 2, type =  c ("Box-Pierce", "Ljung-Box"), 
          fitdf = 0)

#Prueba de normalidad J B
library(tseries)
jarque.bera.test(u)

###########
#install.packages(«quantmod»)
library(quantmod)
#install.packages(«tseries»)
library(tseries)
#install.packages(«forecast»)
library(forecast)
#install.packages(«TSA»)
library(TSA)
#install.packages(«Quandl»)
library(Quandl)

ARcn<-Arima(casos.nal,order=c(1,0,0))
AR1cn<-Arima(casos.nal,order=c(1,1,1))






