# Análise de Riscos - Modelo de Regressão Logística

Este repositório contém um script em R desenvolvido para a disciplina de **Análise de Riscos**. O objetivo do estudo é explorar dados de risco de crédito e construir um modelo de regressão logística para prever a classificação de pagadores.

## 📂 Estrutura do Projeto

- `dados.txt`: Arquivo contendo os dados utilizados na análise.
- `analise_riscos.R`: Script principal com importação, exploração, visualização e modelagem dos dados.

## 📊 Descrição do Projeto

### 1️⃣ Importação dos Dados
Os dados são carregados diretamente de um repositório remoto e transformados em um `data.frame`. Os nomes das colunas são definidos conforme suas respectivas variáveis.

### 2️⃣ Exploração e Visualização
- Geração de **sumários estatísticos** das variáveis quantitativas.
- Construção de **gráficos de dispersão**, **boxplots** e **gráficos 3D** para identificar padrões nos dados.

### 3️⃣ Modelagem Estatística
- Construção de um **modelo de regressão logística** para prever a classificação do pagador.
- Testes de desempenho e seleção de variáveis via **stepwise**.
- Visualização dos resultados do modelo.

## 📌 Tecnologias Utilizadas

- `R` e pacotes:
  - `data.table`
  - `ggplot2`
  - `GGally`
  - `scatterplot3d`
  - `FactoClass`
  - `caret`
  - `tidyverse`

## 📈 Resultados Obtidos

Os modelos construídos foram avaliados com base em sua **acurácia preditiva** e **deviance**. O melhor modelo encontrado foi aquele que inclui variáveis selecionadas via `stepwise`, garantindo um melhor ajuste aos dados.


## 📜 Licença
Este projeto é distribuído sob a licença MIT.

---
✍️ **Autor:** Matheus Mori


