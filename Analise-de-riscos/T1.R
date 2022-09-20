library(data.table)
library(rjson)
library("XML")
library(ggplot2)
library("GGally")
library(scatterplot3d)
library(FactoClass)
library("stats")

########## Importando os dados ##########

url = "https://raw.githubusercontent.com/mori-mkm/UFSCar/main/Analise-de-riscos/dados.txt"
df <- fread(url,data.table=getOption("datatable.fread.datatable", FALSE),  sep = " ",  header= FALSE)
df <- data.frame(df)
names(df) <- c('Status', "Duracao", "Historico", "Proposito", "Qt_credito", 
               "Conta_poupanca","Emprego", "rendimento", "Status_pessoal", "Outros_devedores",
               "Residencia_tempo", "Propriedade", "Idade", "Outros_planos", "Residencia",
               "Qt_credito_banco", "Trabalho", "Qt_pessoas_manutencao", "Telefone","Trabalhador_estrangeiro",
               "tipo_pagador")

df$Status                  <- as.factor(df$Status)
df$Historico               <- as.factor(df$Historico)
df$Proposito               <- as.factor(df$Proposito)
df$Conta_poupanca          <- as.factor(df$Conta_poupanca)
df$Emprego                 <- as.factor(df$Emprego)
df$Status_pessoal          <- as.factor(df$Status_pessoal)
df$Outros_devedores        <- as.factor(df$Outros_devedores)
df$Propriedade             <- as.factor(df$Propriedade)
df$Outros_planos           <- as.factor(df$Outros_planos)
df$Residencia              <- as.factor(df$Residencia)
df$Trabalho                <- as.factor(df$Trabalho)
df$Telefone                <- as.factor(df$Telefone)
df$Trabalhador_estrangeiro <- as.factor(df$Trabalhador_estrangeiro)
df$tipo_pagador            <- as.factor(df$tipo_pagador)


df_quali <- df[c(1,3,4,6,7,9,10,12,14,15,17,19,20,21)]
df_quant <- df[c(2,5,8,11,13,16,18)]

# Inspect the data
sample_n(df, 3)

########## Visualização dos dados ##########
t(summary(df_quant))
ggpairs(df_quant)

# Boxplot tipo_pagador vs Qt_credito
ggplot(df, aes(x=tipo_pagador, y=Qt_credito)) + 
  geom_boxplot()+
  labs(title="Quant. de credito por Tipo de pagador",x="tipo_pagador", y = "Qt_credito")

# Boxplot tipo_pagador vs Duracao
ggplot(df, aes(x=tipo_pagador, y=Duracao)) + 
  geom_boxplot()+
  labs(title="Quant. de credito por Duracao",x="tipo_pagador", y = "Duracao")

# Boxplot tipo_pagador vs Idade
ggplot(df, aes(x=tipo_pagador, y=Idade)) + 
  geom_boxplot()+
  labs(title="Quant. de credito por Idade",x="tipo_pagador", y = "Idade")


# Dispersão tipo de tipo_pagador
ggplot(df, aes(x=Qt_credito, y=Duracao, color=tipo_pagador, shape=tipo_pagador)) +
  geom_point() + 
  geom_smooth(method=lm, aes(fill=tipo_pagador))

# Dispersão tipo de Status
ggplot(df, aes(x=Qt_credito, y=Duracao, color=Status, shape=Status)) +
  geom_point() + 
  geom_smooth(method=lm, aes(fill=Status))

# Dispersão tipo de Outros_devedores
ggplot(df, aes(x=Qt_credito, y=Duracao, color=Outros_devedores, shape=Outros_devedores)) +
  geom_point() + 
  geom_smooth(method=lm, aes(fill=Outros_devedores))

# Dispersão tipo de Telefone
ggplot(df, aes(x=Qt_credito, y=Duracao, color=Telefone, shape=Telefone)) +
  geom_point() + 
  geom_smooth(method=lm, aes(fill=Telefone))

# Dispersão tipo de Trabalhador_estrangeiro
ggplot(df, aes(x=Qt_credito, y=Duracao, color=Trabalhador_estrangeiro, shape=Trabalhador_estrangeiro)) +
  geom_point() + 
  geom_smooth(method=lm, aes(fill=Trabalhador_estrangeiro))

# Dispersao 3d
# Add regression plane
my.lm <- lm(df$Qt_credito ~ df$Duracao + df$Idade)

# Dispersao 3d tipo_pagador
colors <- c("#56B4E9", "#E69F00")#, "999999")
s3d <- scatterplot3d(df[, c(2,13,5)], pch = 16, color = colors[df$tipo_pagador],
              grid = FALSE, box = FALSE, xlab = "Qt_credito", 
              ylab = "Idade", zlab = "Duracao")
addgrids3d(df[, c(2,13,5)], grid = c("xy", "xz", "yz"))

s3d$plane3d(my.lm)

########## Modelo de regressao logistica ##########
# http://www.sthda.com/english/articles/36-classification-methods-essentials/151-logistic-regression-essentials-in-r/

library(tidyverse)
library(caret)

# Dividindo os dados em conjunto de treinamento e teste
set.seed(42)
training.samples <- df$tipo_pagador %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- df[training.samples, ]
test.data <- df[-training.samples, ]

# Criando um modelo
model <- glm( tipo_pagador ~., data = train.data, family = binomial)

# Criando um modelo2
model2 <- glm( tipo_pagador ~Status+Duracao+Proposito+Qt_credito+Conta_poupanca+rendimento+Outros_planos,
               data = train.data, family = binomial)

# Sumarizando o modelo
summary(model)$coef
summary(model2)$coef
# Fazendo predicoes
probabilities <- model %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "2", "1")

probabilities2 <- model2 %>% predict(test.data, type = "response")
predicted.classes2 <- ifelse(probabilities2 > 0.5, "2", "1")

# Acurracia do modelo
mean(predicted.classes == test.data$tipo_pagador)
mean(predicted.classes2 == test.data$tipo_pagador)

train.data %>%
  mutate(prob = ifelse(tipo_pagador == "2", 1, 0)) %>%
  ggplot(aes(Qt_credito, prob)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    title = "Logistic Regression Model", 
    x = "Qt_credito",
    y = "Probability of being mal pagador"
  )

train.data %>%
  mutate(prob = ifelse(tipo_pagador == "2", 1, 0)) %>%
  ggplot(aes(Duracao, prob)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    title = "Logistic Regression Model", 
    x = "Duracao",
    y = "Probability of being mal pagador"
  )
# Stapwise
#https://www.utstat.toronto.edu/~brunner/oldclass/appliedf11/handouts/2101f11StepwiseLogisticR.pdf
backwards = step(model)
formula(backwards)
summary(backwards)

back2 = glm(tipo_pagador ~ Status + Duracao + Historico + Proposito + 
              Qt_credito + Conta_poupanca + rendimento + Status_pessoal + 
              Outros_devedores + Outros_planos + Telefone + Trabalhador_estrangeiro,
            data = train.data,family=binomial)
summary(back2)
model$deviance; model2$deviance; back2$deviance


