####librarys####
#ACP
library("readxl")
library('FactoMineR')
library('factoextra')

#Visualização
library("reshape2")
library("GGally")
library("ggplot2")
library("mvtnorm")
library("corrplot")


####Determinando o ditetório de destino do nosso código#### 
setwd("C:\\Users\\MKM\\OneDrive\\Área de Trabalho\\EAD 2021\\mult2")
getwd()

####Importando os dados####
df <- read_excel("dataset.xlsx")
colnames(df) <- c("comp", "fb_longa","fb_fina","trac_alc","comp_quebra","elast","estres","forca_exp")
head(df)

#### Correlação ####

cormat <- round(cor(df),2)
melted_cormat <- melt(cormat)
reorder_cormat <- function(cormat){
  
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

cormat <- reorder_cormat(cormat)
cormat
get_upper_tri <- function(cormat) {
  cormat[lower.tri(cormat)] <- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat)

melted_cormat <- melt(upper_tri, na.rm = TRUE)

ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 8, barheight = 1,
                               title.position = "top", title.hjust = 0.5))


#### ggpairs plot with heatmap of correlation values####
p <-8

# Matrix of plots
p1 <- ggpairs(df, lower = list(continuous = "smooth"))  
# Correlation matrix plot
p2 <- ggcorr(df, label = TRUE, label_round = 2)

# Get list of colors from the correlation matrix plot

g2 <- ggplotGrob(p2)
colors <- g2$grobs[[6]]$children[[3]]$gp$fill

# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1,k1,k2) +
      theme(panel.background = element_rect(fill = colors[idx], color="white"),
            panel.grid.major = element_line(color=colors[idx]))
    p1 <- putPlot(p1,plt,k1,k2)
    idx <- idx+1
  }
}
print(p1)


#### ACP ####
acp <- PCA(df, scale.unit=TRUE, graph=FALSE, quali.sup=8)
print(acp)


#### Autovalores ####

#acp$eig
acp.val<- get_eigenvalue(acp)
print(acp.val)
cat("A variância total é igual á ", sum(acp.val[0:7]))

### Scree Plot
fviz_eig(acp, addlabels=TRUE, ylim = c(0, 90))


var <- get_pca_var(acp)
var

# Coordinates
head(var$coord)
# Cos2: quality on the factore map
head(var$cos2)
# Contributions to the principal components
head(var$contrib)


fviz_pca_var(acp,col.var="black")

head(var$cos2, 4)
corrplot(var$cos2, is.corr=FALSE)

# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(acp, choice = "var", axes = 1:2)

# Color by cos2 values: quality on the factor map
fviz_pca_var(acp, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)



# Change the transparency by cos2 values
fviz_pca_var(acp, alpha.var = "cos2")


head(var$contrib, 8)
corrplot(var$contrib, is.corr=FALSE)  

# Contributions of variables to PC1
fviz_contrib(acp, choice = "var", axes = 1, top = 10)

# Contributions of variables to PC2
fviz_contrib(acp, choice = "var", axes = 2, top = 10)

fviz_contrib(acp, choice = "var", axes = 1:2, top = 10)

fviz_pca_var(acp, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

# Change the transparency by contrib values
fviz_pca_var(acp, alpha.var = "contrib")


#### Gráfico de indivíduos####

ind <- get_pca_ind(acp)
ind
# Coordinates of individuals
head(ind$coord)
# Quality of individuals
head(ind$cos2)
# Contributions of individuals
head(ind$contrib)

### Parcelas: qualidade e contribuição

fviz_pca_ind(acp)

fviz_pca_ind(acp, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)

fviz_pca_ind(acp, col.ind = "cos2", pointsize = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)

fviz_cos2(acp, choice = "ind")

# Total contribution on PC1 and PC2
fviz_contrib(acp, choice = "ind", axes = 1:2)
