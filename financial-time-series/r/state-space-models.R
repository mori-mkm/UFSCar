################# https://kevin-kotze.gitlab.io/tsm/ts-5-tut/ #################
library(fGarch)
library(quantmod)
library(rugarch)
library(tidyverse)
library(ggthemes)
library(forecast)
library(tseries)
library(gridExtra)
install.packages('dlm', repos='https://cran.rstudio.com/', dependencies=TRUE)
library(tsm)
library(dlm)
library(tidyverse)
library(lubridate)
library(sarb2020q1)
#######################################################

getSymbols(c('TIMS3.SA', 'PETR4.SA', 'ITUB4.SA', 'VALE3.SA', 'BOVA11.SA'),
           periodicity='daily', 
           from='2020-01-01',
           to='2022-09-20')
acoes<- getSymbols(c('TIMS3.SA', 'PETR4.SA', 'ITUB4.SA', 'VALE3.SA', 'BOVA11.SA'),
                   periodicity='daily', from='2020-01-01',to='2022-09-20')

TIMS3.SA  <- merge(dailyReturn(TIMS3.SA[,6],  type='log')[-1,], TIMS3.SA[,6])
TIMS3.SA  <- na.omit(TIMS3.SA)
PETR4.SA  <- merge(dailyReturn(PETR4.SA[,6],  type='log')[-1,], PETR4.SA[,6])
PETR4.SA  <- na.omit(PETR4.SA)
ITUB4.SA  <- merge(dailyReturn(ITUB4.SA[,6],  type='log')[-1,], ITUB4.SA[,6])
ITUB4.SA  <- na.omit(ITUB4.SA)
VALE3.SA  <- merge(dailyReturn(VALE3.SA[,6],  type='log')[-1,], VALE3.SA[,6])
VALE3.SA  <- na.omit(VALE3.SA)
BOVA11.SA <- merge(dailyReturn(BOVA11.SA[,6], type='log')[-1,], BOVA11.SA[,6])
BOVA11.SA <- na.omit(BOVA11.SA)

names(TIMS3.SA)  <- c("TIM_log","TIM")
names(PETR4.SA)  <- c("PET_log","PET")
names(ITUB4.SA)  <- c("ITU_log","ITU")
names(VALE3.SA)  <- c("VAL_log","VAL")
names(BOVA11.SA) <- c("BOV_log","BOV")

dados <- merge(TIMS3.SA, PETR4.SA, ITUB4.SA, VALE3.SA, BOVA11.SA)
head(dados)
length(dados$TIM)


df <- dados
df <- as.data.frame(df)
df$date <- index(dados)
rownames(df) <- seq(length=nrow(df)) 
head(df)

df_log   <- df[,c(11,1,3,5,7,9)]
df_serie <- df[,c(11,2,4,6,8,10)]

dados_log <- dados[,c(1,3,5,7,9)]
dados_ser <- dados[,c(2,4,6,8,10)]
####################O modelo de nível local#############################

y <- df$TIM
plot.ts(y)

#  o primeiro modelo que vamos construir é um modelo de nível local, 
#  que inclui uma inclinação estocástica.

fn <- function(parm) {
  dlmModPoly(order = 1,
             dV = exp(parm[1]),
             dW = exp(parm[2]))
}

# A partir daí, vamos assumir que os valores iniciais para os parâmetros
#  são pequenos (ou seja, zero) e proceder para ajustar o modelo aos dados 
#  com o auxílio de métodos de máxima verossimilhança.

fit <- dlmMLE(y, rep(0, 2), build = fn, hessian = TRUE)
(conv <- fit$convergence)  # zero for converged

# Observe que, colocando colchetes ao redor do comando, ele imprimirá 
#  o resultado no console, onde notamos que a convergência foi alcançada. 
#  Podemos então construir a função de verossimilhança e gerar estatísticas 
#  para os critérios de informação.

loglik <- dlmLL(y, dlmModPoly(1))
n.coef <- 2
r.aic <- (2 * (loglik)) + 2 * (sum(n.coef)) #dlmLL caculates the neg. LL
r.bic <- (2 * (loglik)) + (log(length(y))) * (n.coef)

# As estatísticas para a matriz de variância-covariância também podem 
#  ser extraídas do objeto modelo com o uso dos comandos:
mod <- fn(fit$par)
obs.error.var <- V(mod)
state.error.var <- W(mod)
######### 1.3 Filtro Kalman e mais suave
# Vamos extrair informações relativas ao filtro Kalman e suavizador
filtered <- dlmFilter(y, mod = mod)
smoothed <- dlmSmooth(filtered)
# Depois disso, podemos olhar para os resultados do filtro de Kalman 
#  para derivar os erros de medição. Estes foram armazenados no residsobjeto. 
#  Também poderíamos traçar os valores suavizados da tendência estocástica, 
#  que foi rotulada como mu.

resids <- residuals(filtered, sd = FALSE)
mu <- dropFirst(smoothed$s)
mu.1 <- mu[1]
mu.end <- mu[length(mu)]

################ 1.4 Plote os resultados 
# Para plotar os resultados em dois gráficos, usamos o mfrowcomando 
#  para criar duas linhas e uma coluna para a área de plotagem. 
#  Depois disso, o primeiro gráfico inclui dados para a variável real 
#  e a linha preta refere-se à tendência estocástica e a linha cinza 
#  representa os dados reais. O gráfico abaixo mostra os erros de medição.
par(mfrow = c(2, 1),
    mar = c(2.2, 2.2, 1, 1),
    cex = 0.8)
plot.ts(
  y,
  col = "darkgrey",
  xlab = "",
  ylab = "",
  lwd = 1.5
)
lines(mu , col = "black")
legend(
  "topright",
  legend = c("Observed Deflator", "Stochastic level"),
  lwd = c(2, 1),
  col = c("darkgrey", "black"),
  bty = "n"
)

plot.ts(
  resids,
  ylab = "",
  xlab = "",
  col = "darkgrey",
  lwd = 1.5
)
abline(h = 0)
legend(
  "topright",
  legend = "Residuals",
  lwd = 1.5,
  col = "darkgrey",
  bty = "n"
)
# Também podemos imprimir os resultados dos critérios de informação, 
# bem como a variância dos respectivos erros.

cat("AIC", r.aic)
cat("BIC", r.bic)
cat("V.variance", obs.error.var)
cat("W.variance", state.error.var)

# Os diagnósticos dos resíduos, incluídos a seguir, sugerem que não 
# há evidência de correlação serial remanescente e sua distribuição 
# é relativamente normal.

par(mfrow = c(1, 1),
    mar = c(2.2, 2.2, 1, 1),
    cex = 0.8)
hist(
  resids,
  prob = TRUE,
  col = "grey",
  main = ""
)

ac(resids)  # acf

Box.test(resids,
         lag = 12,
         type = "Ljung",
         fitdf = 2)  # joint autocorrelation

shapiro.test(resids)  # normality

# Neste caso, tudo parece satisfatório, pois não há autocorrelação 
#  inexplicada quando o valor de p é inferior a 5% e o teste de Shapiro 
#  sugere que os resíduos são normalmente distribuídos quando o 
#  valor-p é inferior a 5%.

############### 2 O modelo de tendência de nível local ############
# Um exemplo do modelo de tendência de nível local está contido 
#  no arquivo T5-llm_trend.R. Para estimar os valores dos parâmetros 
#  e componentes não observados para este modelo, poderíamos começar 
#  exatamente como fizemos antes.

#rm(list=ls())
graphics.off()

y <- df$TIM
plot.ts(y)

####### 2.1 Construir o modelo e estimar os parâmetros
# No entanto, neste caso, precisamos incorporar uma equação de estado 
#  adicional para a inclinação do modelo. Este é um modelo polinomial 
#  de segunda ordem e, como existem dois erros estocásticos independentes, 
#  também precisamos incluir esse recurso no modelo.

fn <- function(parm) {
  dlmModPoly(order = 2,
             dV = exp(parm[1]),
             dW = exp(parm[2:3]))
}

# Podemos então calcular os critérios de informação da mesma forma que 
#  fizemos anteriormente, usando os seguintes comandos.
fit <- dlmMLE(y, rep(0, 3), build = fn, hessian = TRUE)
conv <- fit$convergence  # zero for converged

loglik <- dlmLL(y, dlmModPoly(2))
n.coef <- 3
r.aic <- (2 * (loglik)) + 2 * (sum(n.coef)) #dlmLL caculates the neg. LL
r.bic <- (2 * (loglik)) + (log(length(y))) * (n.coef)

mod <- fn(fit$par)
obs.error.var <- V(mod)
state.error.var <- diag(W(mod))
######### 2.2 Filtro Kalman e mais suave 
# Os valores para o filtro de Kalman e suavizador também podem ser extraídos
#  como antes, mas observe que neste caso também estamos interessados 
#  nos valores da inclinação estocástica, que é alocada ao upsilonobjeto.
filtered <- dlmFilter(y, mod = mod)
smoothed <- dlmSmooth(filtered)
resids <- residuals(filtered, sd = FALSE)
mu <- dropFirst(smoothed$s[, 1])
upsilon <- dropFirst(smoothed$s[, 2])
mu.1 <- mu[1]
mu.end <- mu[length(mu)]
ups.1 <- upsilon[1]
ups.end <- upsilon[length(mu)]

######## 2.3 Plote os resultados
# Agora vamos plotar os resultados para os três gráficos um abaixo do outro, 
#  usando os comandos.
par(mfrow = c(3, 1),
    mar = c(2.2, 2.2, 1, 1),
    cex = 0.8)
plot.ts(
  y,
  col = "darkgrey",
  xlab = "",
  ylab = "",
  lwd = 2
)
lines(mu , col = "black")
legend(
  "topright",
  legend = c("Observed Deflator", "Stochastic level"),
  lwd = c(2, 1),
  col = c("darkgrey", "black"),
  bty = "n"
)

plot.ts(
  upsilon,
  col = "darkgrey",
  xlab = "",
  ylab = "",
  lwd = 2
)
legend(
  "topright",
  legend = "Slope",
  lwd = 2,
  col = "darkgrey",
  bty = "n"
)

plot.ts(
  resids,
  ylab = "",
  xlab = "",
  col = "darkgrey",
  lwd = 2
)
abline(h = 0)
legend(
  "topright",
  legend = "Residuals",
  lwd = 2,
  col = "darkgrey",
  bty = "n"
)
# Também podemos imprimir os resultados dos critérios de informação, 
# bem como a variância dos respectivos erros.
cat("AIC", r.aic)
cat("BIC", r.bic)
cat("V.variance", obs.error.var)
cat("W.variance", state.error.var)
# Os diagnósticos dos resíduos, incluídos a seguir, sugerem que não 
# há evidência de correlação serial remanescente e sua distribuição 
# é relativamente normal.

ac(resids)  # acf
Box.test(resids, lag=12, type="Ljung", fitdf=2)  # joint autocorrelation
shapiro.test(resids)  # normality



########### 3 O modelo de nível local ####################
# Um exemplo do modelo de tendência de nível local está contido no arquivo
# T5-llm_seas.R. Para estimar os valores dos parâmetros e componentes não 
# observados neste modelo, poderíamos começar exatamente como fizemos antes.

#rm(list=ls())
graphics.off()

y <- df$TIM
plot.ts(y)

######## 3.1 Construir o modelo e estimar os parâmetros
# No entanto, neste caso, precisamos incorporar equações de estado adicionais 
#  para os componentes sazonais. Este é um modelo polinomial de primeira ordem 
#  com o componente sazonal adicional que é medido em uma frequência trimestral.

fn <- function(parm) {
  mod = dlmModPoly(order = 1) + dlmModSeas(frequency = 4)
  V(mod) = exp(parm[1])
  diag(W(mod))[1:2] = exp(parm[2:3])
  return(mod)
}

# Podemos então calcular os critérios de informação da mesma forma que fizemos
#  anteriormente, usando os seguintes comandos.

fit <- dlmMLE(y, rep(0,3), build=fn, hessian=TRUE)
conv <- fit$convergence  # zero for converged

loglik <- dlmLL(y, dlmModPoly(1)+dlmModSeas(4))

n.coef <- 3
r.aic <- (2*(loglik)) + 2*(sum(n.coef)) #dlmLL caculates the neg. LL
r.bic <- (2*(loglik)) + (log(length(y)))*(n.coef)

mod <- fn(fit$par)
obs.error.var <- V(mod)
state.error.var <- diag(W(mod))
############3.2 Filtro Kalman e mais suave
# Os valores para o filtro Kalman e suavizador também podem ser extraídos 
#  como antes, mas observe que neste caso também estamos interessados 
#  nos valores da sazonalidade, que é alocada ao gammaobjeto.

filtered <- dlmFilter(y, mod=mod)
smoothed <- dlmSmooth(filtered)
resids <- residuals(filtered,sd=FALSE)
mu <- dropFirst(smoothed$s[,1])
gammas <- dropFirst(smoothed$s[,2])
mu.1 <- mu[1]
mu.end <- mu[length(mu)]
gammas.1 <- gammas[1]
gammas.end <- gammas[length(mu)]
#########3.3 Plote os resultados
# Agora vamos plotar os resultados para os três gráficos um abaixo do outro, 
# usando os comandos.

par(mfrow=c(3,1),mar=c(2.2,2.2,1,1),cex=0.8)
plot.ts(y, col="darkgrey", xlab="", ylab="", lwd=2)
lines(mu , col="black")
legend("topright", legend=c("Observed Deflator","Stochastic level"),
       lwd=c(2,1), col=c("darkgrey","black"), bty="n")

plot.ts(gammas, col="darkgrey", xlab="", ylab="", lwd=2)
legend("topright", legend="Seasonal", lwd=2, col="darkgrey", bty="n")

plot.ts(resids, ylab="", xlab="", col="darkgrey", lwd=2)
abline(h=0)
legend("topright", legend="Residuals", lwd=2, col="darkgrey", bty="n")

# Claro que, como esses elementos do modelo são aditivos, poderíamos combinar 
#  a sazonalidade e o nível, como no gráfico a seguir.

alpha <- mu + gammas

par(mfrow=c(1,1),mar=c(2.2,2.2,1,1),cex=0.8)
plot.ts(y, col="darkgrey", xlab="", ylab="", lwd=2)
lines(alpha , col="black")
legend("topright", legend=c("Observed Deflator","State Components"),
       lwd=c(2,1), col=c("darkgrey","black"), bty="n")
# Também podemos imprimir os resultados dos critérios de informação, 
#  bem como a variância dos respectivos erros.
cat("AIC", r.aic)
cat("BIC", r.bic)
cat("V.variance", obs.error.var)
cat("W.variance", state.error.var)
# Os diagnósticos dos resíduos, incluídos a seguir, sugerem que não 
# há evidência de correlação serial remanescente e sua distribuição 
# é relativamente normal.

par(mfrow = c(1, 1),
    mar = c(2.2, 2.2, 1, 1),
    cex = 0.8)
hist(
  resids,
  prob = TRUE,
  col = "grey",
  main = ""
)

ac(resids)  # acf

Box.test(resids,
         lag = 12,
         type = "Ljung",
         fitdf = 2)  # joint autocorrelation

shapiro.test(resids)  # normality

# Neste caso, tudo parece satisfatório, pois não há autocorrelação 
#  inexplicada quando o valor de p é inferior a 5% e o teste de Shapiro 
#  sugere que os resíduos são normalmente distribuídos quando o 
#  valor-p é inferior a 5%.


############ 4 Intervalos de confiança ############
# Para criar intervalos de confiança em torno das estimativas do filtro
#  de Kalman ou mais suave, podemos usar o exemplo do modelo de nível local.
#  Um exemplo de um modelo de nível local com intervalos de confiança está 
#  contido em T5-llm_conf.R, que contém os seguintes comandos:
  
#rm(list=ls())
graphics.off()

y <- df$TIM
plot.ts(y)

# Depois disso, podemos usar a mesma estrutura de modelo que tínhamos originalmente.

fn <- function(parm) {
  dlmModPoly(order = 1,
             dV = exp(parm[1]),
             dW = exp(parm[2]))
}

fit <- dlmMLE(y, rep(0, 2), build = fn, hessian = TRUE)
conv <- fit$convergence  # zero for converged

loglik <- dlmLL(y, dlmModPoly(1))
n.coef <- 2
r.aic <- (2 * (loglik)) + 2 * (sum(n.coef)) #dlmLL caculates the neg. LL
r.bic <- (2 * (loglik)) + (log(length(y))) * (n.coef)

mod <- fn(fit$par)
obs.error.var <- V(mod)
state.error.var <- W(mod)

filtered <- dlmFilter(y, mod = mod)
smoothed <- dlmSmooth(filtered)
resids <- residuals(filtered, sd = FALSE)
mu <- dropFirst(smoothed$s)
mu.1 <- mu[1]
mu.end <- mu[length(mu)]
#################### 4.1 Construindo intervalos de confiança
# Para construir os intervalos de confiança em torno da tendência estocástica
#  suavizada, invocaríamos os comandos:
  
conf.tmp <- unlist(dlmSvd2var(smoothed$U.S, smoothed$D.S))
conf <- ts(as.numeric(conf.tmp)[-1],
           start = c(1960, 2),
           frequency = 4)
wid <- qnorm(0.05, lower = FALSE) * sqrt(conf)

conf.pos <- mu + wid
conf.neg <- mu - wid
#  Da mesma forma, se quiséssemos fazer o mesmo para os valores filtrados, 
#    utilizaríamos informações relacionadas aos erros de previsão.

mu.f <- dropFirst(filtered$a)
cov.tmp <- unlist(dlmSvd2var(filtered$U.R, filtered$D.R))
if (sum(dim(mod$FF)) == 2) {
  variance <- cov.tmp + as.numeric(V(mod))
} else {
  variance <-
    (sapply(cov.tmp, function(x)
      mod$FF %*% x %*% t(mod$FF))) + V(mod)
}
###########  4.2 Plotando os resultados
# Isso nos permitiria traçar os resultados

par(mfrow = c(1, 1),
    mar = c(2.2, 2.2, 1, 1),
    cex = 0.8)
plot.ts(
  y,
  col = "darkgrey",
  xlab = "",
  ylab = "",
  lwd = 1.5
)
lines(mu, col = "black")
lines(as.numeric(conf.pos) , col = "red")
lines(as.numeric(conf.neg), col = "red")
legend(
  "topright",
  legend = c("Observed Deflator", "Stochastic level", "Confidence Interval"),
  lwd = c(1.5, 1, 1),
  col = c("darkgrey", "black", "red"),
  bty = "n"
)

############## 6 Previsão ############## 
# Para usar o modelo para prever o futuro, considere o exemplo contido no 
#  arquivo T5-llm_fore.R. As linhas iniciais de código são exatamente as 
#  mesmas que sempre tivemos.
#rm(list=ls())
graphics.off()

y <- df$TIM
plot.ts(y)


# Podemos usar novamente uma estrutura de modelo de nível local, que pode 
#  ser construída como antes, onde incluímos intervalos de confiança.

fn <- function(parm) {
  dlmModPoly(order = 1,
             dV = exp(parm[1]),
             dW = exp(parm[2]))
}

fit <- dlmMLE(y, rep(0, 2), build = fn, hessian = TRUE)
conv <- fit$convergence  # zero for converged

loglik <- dlmLL(y, dlmModPoly(1))
n.coef <- 2
r.aic <- (2 * (loglik)) + 2 * (sum(n.coef)) #dlmLL caculates the neg. LL
r.bic <- (2 * (loglik)) + (log(length(y))) * (n.coef)

mod <- fn(fit$par)
obs.error.var <- V(mod)
state.error.var <- W(mod)

filtered <- dlmFilter(y, mod = mod)
smoothed <- dlmSmooth(filtered)
resids <- residuals(filtered, sd = FALSE)
mu <- dropFirst(smoothed$s)

conf.tmp <- unlist(dlmSvd2var(smoothed$U.S, smoothed$D.S))
conf <- ts(conf.tmp[-1], start = c(1960, 2), frequency = 4)
wid <- qnorm(0.05, lower = FALSE) * sqrt(conf)

conf.pos <- mu + wid
conf.neg <- mu - wid

comb.state <- cbind(mu, conf.pos, conf.neg)
#  Para prever adiante usamos o dlmForecastcomando, onde neste caso estamos 
#   interessados em prever dez passos à frente.

forecast <- dlmForecast(filtered, nAhead = 12)
var.2 <- unlist(forecast$Q)
wid.2 <- qnorm(0.05, lower = FALSE) * sqrt(var.2)
comb.fore <- cbind(forecast$f, forecast$f + wid.2, forecast$f - wid.2)

result <-
  ts(rbind(comb.state, comb.fore),
     start = c(2020, 1),
     frequency = 12)
##########6.1 Plotando os resultados 
# Para desenhar um gráfico que contenha os resultados, utilizaríamos os 
#  seguintes comandos:
  
par(mfrow = c(1, 1),
      mar = c(2.2, 2.2, 1, 1),
      cex = 0.8)
plot.ts(
  result,
  col = c("black", "red", "red"),
  plot.type = "single",
  xlab = "",
  ylab = "",
  lty = c(1, 2, 2)
)

legend(
  "topright",
  legend = c("Observed Deflator", "Stochastic level"),
  lwd = c(1.5, 1),
  col = c("darkgrey", "black"),
  bty = "n"
)

