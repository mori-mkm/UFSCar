install.packages("ggplot2", dependencies = TRUE)
install.packages("qqplotr", dependencies = TRUE)
install.packages("dplyr", dependencies = TRUE)
install.packages("DescTools", dependencies = TRUE)
install.packages("FSA", dependencies = TRUE)
install.packages("PMCMRplus", dependencies = TRUE)
install.packages("magrittr") 
install.packages("dplyr")    
#####################################
library(magrittr) 
library(dplyr)    
library(tidyverse)
library(ggpubr)
library(ggplot2)
library(qqplotr)
library(rstatix)
library(lattice)
library(dplyr)
library(DescTools)
library(FSA)
library(PMCMRplus)

####################################
### Criando os dados
n <- 1000

#Amostra do tramento A
set.seed(1) #Fixando a amostra 
A <- rnorm(n, mean=3 , sd = 1) # Gerando os dados de uma distribuicao normal
A_ <-rep("A",n)
A

#Amostra do tramento B
set.seed(2) #Fixando a amostra 
B <- rnorm(n, sd = 1) # Gerando os dados de uma distribuicao normal
B_ <-rep("B",n)
B


#Amostra do tramento C
set.seed(3) #Fixando a amostra 
C <- rnorm(n, mean = 2, sd = 1) # Gerando os dados de uma distribuicao normal
C_ <-rep("C",n)
C

valores <- c(A,B,C)
tratamentos <- c(A_,B_,C_)

df <- data.frame(valores, tratamentos) #Bancos de dados 
df
###################################

# Visualizacao dos dados
### Boxplot
ggboxplot(df, x = "tratamentos", y = "valores", fill="tratamentos")

## Histograma
histogram(~ valores | tratamentos, data=df, layout=c(1,3))


##################################
# Analise descritiva

df %>% select(tratamentos, valores) %>% group_by(tratamentos) %>% 
  summarise(n=n(), 
            mean=mean(valores, na.rm = TRUE), 
            sd=sd(valores, na.rm = TRUE),
            stderr=sd/sqrt(n),
            LCL = mean - qt(1 - (0.05 / 2), n - 1) * stderr,
            UCL = mean + qt(1 - (0.05 / 2), n - 1) * stderr,
            median=median(valores, na.rm = TRUE),
            min=min(valores, na.rm = TRUE), 
            max=max(valores, na.rm = TRUE),
            IQR=IQR(valores, na.rm = TRUE))

##################################

#Teste de normalidade
df %>%
  group_by(tratamentos) %>%
  summarise(W = shapiro.test(valores)$statistic,
            p.value = shapiro.test(valores)$p.value)

#QQ plots by group
ggplot(data = df, mapping = aes(sample = valores, color = tratamentos, fill = tratamentos)) +
  geom_qq_band(alpha=0.5, conf=0.95, bandType = "pointwise") +
  stat_qq_line() +
  stat_qq_point(col="black") +
  facet_wrap(~ tratamentos, scales = "free") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + theme_bw() 

##################################
#Teste da mediana
mediana <- median(c(A,B,C))
medA <- c(sum(A > mediana), sum(A <= mediana))
medB <- c(sum(B > mediana), sum(B <= mediana))
medC <- c(sum(C > mediana), sum(C <= mediana))
d <- as.table(cbind(medA,medB,medC))
chisq.test(d)

#Perform the Kruskal-Wallis test
m1<-kruskal.test(valores ~ tratamentos, data=df)
print(m1)
