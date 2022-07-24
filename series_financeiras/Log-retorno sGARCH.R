install.packages("fGarch")
install.packages("quantmod")
install.packages("rugarch")
install.packages("tidyverse")
install.packages("ggthemes")
install.packages("forecast")
install.packages("tseries")
install.packages("gridExtra")

## Fonte dos c�digos
## https://rpubs.com/ionaskel/VaR_Garch_market_risk
library(fGarch)
library(quantmod)
library(rugarch)

library(tidyverse)
library(ggthemes)
library(forecast)
library(tseries)
library(gridExtra)

## Carregando os dados
getSymbols(c('TIMS3.SA'),
           periodicity='daily', 
           from='2000-12-01',
           to='2022-07-14')

dados <- merge(dailyReturn(TIMS3.SA[,6],type='log')[-1,],
               TIMS3.SA[,6])
dados <- na.omit(dados)
names(dados) <- c("TIM_log","TIM")
length(dados$TIM)

## Grafico da serie
qplot(x = 1:length(dados$TIM) , y = dados$TIM , geom = 'line') +
  geom_line(color = 'darkblue') +
  labs(x = '' , y = 'Pre�o' , title = "TIMS3.sa") + 
  geom_hline(yintercept = mean(dados$TIM) , color = 'red')

## Gr�fico do Log-Retorno

p1 = qplot(x = 1:length(dados$TIM_log) , y = dados$TIM_log , geom = 'line') + 
  geom_line(color = 'darkblue') + 
  geom_hline(yintercept = mean(dados$TIM_log) , color = 'red' , size = 1) + 
  labs(x = '' , y = 'Log-retornos di�rios')

p2 = qplot(dados$TIM_log , geom = 'density') +
  coord_flip() + 
  geom_vline(xintercept = mean(dados$TIM_log) , color = 'red' , size = 1) +
  geom_density(fill = 'lightblue' , alpha = 0.4) + 
  labs(x = '')

grid.arrange(p1 , p2 , ncol = 2)

## Estacionariedade do Log-Retorno

adf.test(dados$TIM_log) 
### Um valor P pequeno (<0,01), ent�o a serie do log-retorno � estacion�ria.



# Estima��o de modelos Metodologia Box-Jenkins
model.arima = auto.arima(dados$TIM_log , max.order = c(3 , 0 ,3) , stationary = TRUE , trace = T , ic = 'aicc')
### O melhor modelo neste caso � o ARIMA(5,0,0)
model.arima
## Verifica��o de diagn�stico para o modelo ARIMA(5,0,0)
model.arima$residuals %>% ggtsdisplay(plot.type = 'hist' )
## Teste de hipotese para os parametros do modelo ARIMA
ar.res = model.arima$residuals
Box.test(model.arima$residuals , lag = 20 , fitdf = 2 , type = 'Ljung-Box')
### Rejeitamos a hipotese nula e temos evidencias de que os residuos 
###  se comportam como residuo branco
## QQPLOT dos residuos
qqnorm(ar.res, pch = 1, frame = FALSE)
qqline(ar.res, col = "steelblue", lwd = 2)


## Analisando a autocorrela��o dos residuos ao quadrado
tsdisplay(ar.res^2 , main = 'Squared Residuals')


## Modelo para os residuos do log-retorno
model.spec = ugarchspec(variance.model = list(model = 'sGARCH' , 
                        garchOrder = c(1 , 1)) , 
                        mean.model = list(armaOrder = c(1 , 0)))
model.fit = ugarchfit(spec = model.spec , data = dados$TIM_log , 
                      solver = 'solnp')
options(scipen = 999)
model.fit@fit$matcoef
### Tanto a1 quanto ??1 s�o significativamente diferentes de zero, portanto, �
###  razo�vel assumir a volatilidade dos res�duos com varia��o no tempo.
model.fit

length(dados$TIM_log)
