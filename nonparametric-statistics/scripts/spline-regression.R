library(splines)
library(MASS)
dados <- mcycle
View(dados)

X <- dados$times
Y <- dados$accel

plot(Y~X, xlab = "Tempo", ylab = "Aceleração")

#Criando nós com os valores dos quantis
knots = attr(bs(dados$times, df = 6),"knots")
knots             

#Calculando o spline com 3 nós
spl3 <- lm(Y ~ bs(X, knots = c(knots)), dados)
plot(Y~X, xlab = "Tempo", ylab = "Aceleração", main = "Spline com 3 Nós")
x.grid <- seq(min(dados$times),max(dados$times), length = 133)
yhat3 <- predict(spl3, newdata = data.frame(X = x.grid), interval = "confidence")

lines(x.grid, yhat3[,1], col = "seagreen", lwd = 2)
matlines(x.grid, yhat3[,2:3], col = "red", lwd = 1, lty = 2)
abline(v = c(knots), col = "gray", lty = 2)



#Modelo de spline com 10 nós
spl10 <- lm(Y ~ bs(X, knots = c(5,10,15,20,25,30,35,40,45,50)), dados)
summary((spl10))

par(mfrow = c(1,1))
plot(Y~X, xlab = "Tempo", ylab = "Aceleração", main = "Spline com 10 Nós")
yhat10 <- predict(spl10, newdata = data.frame(X = x.grid), interval = "confidence")


lines(x.grid, yhat10[,1], col = "seagreen", lwd = 2)
matlines(x.grid, yhat10[,2:3], col = "blue", lwd = 1, lty = 2)
abline(v = c(5,10,15,20,25,30,35,40,45,50), col = "gray", lty = 2)



teste <- lm(dados[,2]~poly(dados[,1],7))
summary(teste)
plot(dados[,1],dados[,2],xlab = "Tempo", ylab = "Aceleração")
lines(dados[,1],predict(teste), col = "red") 



###### spline cúbica com 10 nós manualmente para saber o valor dos coeficientes e comparar
D <- 3
K <- 10
nos<-c(5,10,15,20,25,30,35,40,45,50)
X1<-cbind(rep(1,nrow=nrow(dados)),outer(dados[,1],1:D,"^"))
X2<-outer(dados[,1],nos,">")*outer(dados[,1],nos,"-")^D
X<-cbind(X1,X2)
X
tita_spl10<-lm(dados[,2]~X-1)
tita_spl10
plot(dados[,1],dados[,2],xlab = "Tempo", ylab = "Aceleração")
lines(dados[,1],predict(tita_spl10), col = "red") 
abline(v = c(5,10,15,20,25,30,35,40,45,50), col = "gray", lty = 2)


#Plotando os gráficos com as duas linhas para comparação
plot(Y~X, xlab = "Tempo", ylab = "Aceleração", main = "Spline")
lines(x.grid, yhat10[,1], col = "red", lwd = 2)
lines(x.grid, yhat3[,1], col = "blue", lwd = 2,lty = 2)
legend("bottomright", legend = c(paste0("10 nós"),"3 nós"),
       col = c("red","blue"), lty = 1:2, lwd = 2)