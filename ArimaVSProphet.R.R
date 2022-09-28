#LIBRERIE UTILIZZATE
library(tidyverse)
library(forecast)
library(tseries)
library(readxl)
library(prophet)
library(Metrics)
#------------------------------------------------------------------------

###1)CARICAMENTO DATI

##a)Training set

Training_set <- read_excel("Training_set.xlsx", 
                           col_types = c("date", "numeric"))
View(Training_set)

Training_set$Date<- as.Date(Training_set$Date, format= "%Y/%m/%d")

summary(Training_set)
head(Training_set)

##b)Testing set

Testing_set <- read_excel("Testing_set.xlsx", 
                          col_types = c("date", "numeric"))
View(Testing_set)

Testing_set$Date<- as.Date(Testing_set$Date, format= "%Y/%m/%d")

summary(Testing_set)
head(Testing_set)

#------------------------------------------------------------------------

###2)CARICAMENTO DATI IN FORMATO SPECIFICO PER PROPHET

##a)Training set

PTraining_set <- read_excel("PTraining_set.xlsx", 
                            col_types = c("date", "numeric"))
View(PTraining_set)
head(PTraining_set)

##b)Testing set
PTesting_set <- read_excel("PTesting_set.xlsx", 
                           col_types = c("date", "numeric"))
View(PTesting_set)
head(PTesting_set)

#------------------------------------------------------------------------

###3)ARIMA

##a)Trasformazione in time-series dei dati

t_Training_set <-ts(Training_set[,c("Chiusura")])

##b)ADF test
adf.test(t_Training_set)

##c)Costruzione modello
fit_ARIMA <- auto.arima(t_Training_set,ic="aic",trace = TRUE)
print(summary(fit_ARIMA))

##d)Forecast di agosto 2022
fcast_ARIMA <- forecast(fit_ARIMA,h=31)
summary(fcast_ARIMA)
checkresiduals(fcast_ARIMA)

#d.i)Figura 2
plot(fcast_ARIMA)

##e)Confronto tra valori predetti e reali

Reali=as.numeric(Testing_set$Chiusura)
Predetti_ARIMA=as.numeric(fcast_ARIMA$mean)

#e.i)RMSE

rmse_ARIMA= rmse(Reali,Predetti_ARIMA)
print(rmse_ARIMA)

#e.ii)MAPE
mape_ARIMA= mape(Reali,Predetti_ARIMA)
print(mape_ARIMA)

Accuratezza_ARIMA= 1-mape_ARIMA
print(Accuratezza_ARIMA)

#------------------------------------------------------------------------

###4)PROPHET

##a)Costruzione modello 

Model_Prophet <- prophet(PTraining_set, daily.seasonality = FALSE)
summary(Model_Prophet)

##b)Forecast di agosto 2022

Futuro<- make_future_dataframe(Model_Prophet, periods = 31)
tail(Futuro)

fcast_Prophet <- predict(Model_Prophet, Futuro)
tail(fcast_Prophet[c('ds','yhat','yhat_lower','yhat_upper')])

#b.i)Figura 3

p=dyplot.prophet(Model_Prophet, fcast_Prophet)
p



##c)Confronto tra valori predetti e reali

Predetti_Prophet=as.numeric(fcast_Prophet$yhat[941:971])

#c.i)RMSE

rmse_Prophet= rmse(Reali,Predetti_Prophet)
print(rmse_Prophet)

#c.ii)MAPE

mape_Prophet<- mape(Reali,Predetti_Prophet)
print(mape_Prophet)

Accuratezza_Prophet= 1-mape_Prophet
print(Accuratezza_Prophet)

#------------------------------------------------------------------------

###5)GRAFICO FIGURA 4

Data_Frame_Predetti_ARIMA<-data.frame(Testing_set$Date,Predetti_ARIMA)
Data_Frame_Predetti_Prophet<-data.frame(Testing_set$Date,
                                        Predetti_Prophet)

Luglio= data.frame(Date= Training_set$Date[911:940],
                   Chiusura= Training_set$Chiusura[911:940])

Figura_4<- ggplot()+
  geom_line(data= Luglio,
            mapping = aes(Date,Chiusura,
                          color="Training set luglio"))+
  geom_line(data= Testing_set,
            mapping = aes(Date,Chiusura,
                          color="Andamento effettivo Agosto"))+
  geom_line(data= Data_Frame_Predetti_ARIMA,
            mapping = aes(Testing_set.Date,Predetti_ARIMA,
                          color="Predizione di ARIMA(0,1,0)"))+
  geom_line(data= Data_Frame_Predetti_Prophet,
            mapping = aes(Testing_set.Date,Predetti_Prophet,
                          color="Predizione di Prophet"))
Figura_4
