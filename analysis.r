library(MASS)
library(ca)
library(RColorBrewer)
library(gplots)

####################################### Analiza Korespondencji
# Odstrzały ---------------------------------------------------------------
data = Odstrzaly
data = data[-1,]
rownames(data) <- data$Nazwa
head(data)
data2 <- data[,c(14:23)]
#rownames(data) <- data$Nazwa
head(data2)
data2 <- as.matrix(data2)


colnames(data2) = c('daniele', 'muflony', 'jelenie', 'sarny', 'dziki', 'lisy', 'zajace', 'bazanty', 'kuropatwy', 'kaczki')
rownames(data2)
colSums(data2)
rowSums(data2)
head(data2)

chisq.test(data2)

palete <- rev(brewer.pal(11,"Spectral"))

P <- data2/sum(data2)
PP <- outer(rowSums(P), colSums(P))
E <- (P - PP) / sqrt(PP)
heatmap(E, scale='none', Colv=NA, col=palete)
heatmap.2(E, scale='none', Colv=NA, density.info = 'none', trace='none', col=palete)

plot(ca(data2), mass=c(T,T))

# Odstrzały 2 (niewykorzystany) ---------------------------------------------------------------
data = Odstrzaly
data = data[-1,]
rownames(data) <- data$Nazwa
head(data)
data2 <- data[,c(14,16:17,19,21:23)]
#rownames(data) <- data$Nazwa
head(data2)
data2 <- as.matrix(data2)


colnames(data2) = c('daniele', 'jelenie', 'dziki', 'zajace', 'bazanty', 'kuropatwy', 'kaczki')
rownames(data2)
colSums(data2)
rowSums(data2)
head(data2)

chisq.test(data2)

palete <- rev(brewer.pal(11,"Spectral"))

P <- data2/sum(data2)
PP <- outer(rowSums(P), colSums(P))
E <- (P - PP) / sqrt(PP)
heatmap(E, scale='none', Colv=NA, col=palete)
heatmap.2(E, scale='none', Colv=NA, density.info = 'none', trace='none', col=palete)

plot(ca(data2), mass=c(T,T))




# Absolwenci Uczelni wyzszych ---------------------------------------------------------------------
# Wojewodzctwo a płeć ---------------------------------------------------------------------
data = plec
data = data[-1,]
rownames(data) <- data$Nazwa
head(data)
data2 <- data[,c(3:4)]

#rownames(data) <- data$Nazwa
head(data2)
data2 <- as.matrix(data2)


colnames(data2) = c('mezczyzni', 'kobiety')
chisq.test(data2)

palete <- rev(brewer.pal(11,"Spectral"))

P <- data2/sum(data2)
PP <- outer(rowSums(P), colSums(P))
E <- (P - PP) / sqrt(PP)
heatmap(E, scale='none', Colv=NA, col=palete)
heatmap.2(E, scale='none', Colv=NA, density.info = 'none', trace='none', col=palete)

plot(ca(data2), mass=c(T,T))

# Wojewódzctwo a uczelnia ---------------------------------------------------------------------
data = uczelnia
data = data[-1,]
rownames(data) <- data$Nazwa
head(data)
data2 <- data[,c(4:13)]
#rownames(data) <- data$Nazwa
head(data2)
data2 <- as.matrix(data2)


colnames(data2) = c('uniwersytety',
                    'techniczne', 
                    'rolnicze', 
                    'ekonomiczne',
                    'pedagogiczne',
                    'morskie',
                    'medyczne',
                    'wf',
                    'artystyczne',
                    'teologiczne')

chisq.test(data2)

palete <- rev(brewer.pal(11,"Spectral"))

P <- data2/sum(data2)
PP <- outer(rowSums(P), colSums(P))
E <- (P - PP) / sqrt(PP)
heatmap(E, scale='none', Colv=NA, col=palete)
heatmap.2(E, scale='none', Colv=NA, density.info = 'none', trace='none', col=palete)

plot(ca(data2), mass=c(T,T))


# Spis_powszechny ---------------------------------------------------------
data = spis
data = data[-1,]
rownames(data) <- data$Nazwa
head(data)
data2 <- data[,c(4:10)]
#rownames(data) <- data$Nazwa
head(data2)
data2 <- as.matrix(data2)


colnames(data2) = c('0-9_lat', 
                    '10-19_lat', '20-29_lat', 
                    '29-39_lat', '40-49_lat',
                    '50-59_lat', '60_i_wiecej')
rownames(data2)
colSums(data2)
rowSums(data2)
head(data2)

chisq.test(data2)

palete <- rev(brewer.pal(11,"Spectral"))

P <- data2/sum(data2)
PP <- outer(rowSums(P), colSums(P))
E <- (P - PP) / sqrt(PP)
heatmap(E, scale='none', Colv=NA, col=palete)
heatmap.2(E, scale='none', Colv=NA, density.info = 'none', trace='none', col=palete)

plot(ca(data2), mass=c(T,T))

# dzikie_wysypiska (niewykorzystany) --------------------------------------------------------

data = wysypiska
data = data[-1,]
rownames(data) <- data$Nazwa
head(data)
data2 <- data[,c(4:5)]
#rownames(data) <- data$Nazwa
head(data2)
data2 <- as.matrix(data2)


colnames(data2) = c('istniejace', 'zlikwidowane')

rownames(data2)
colSums(data2)
rowSums(data2)
head(data2)

chisq.test(data2)

palete <- rev(brewer.pal(11,"Spectral"))

P <- data2/sum(data2)
PP <- outer(rowSums(P), colSums(P))
E <- (P - PP) / sqrt(PP)
heatmap(E, scale='none', Colv=NA, col=palete)
heatmap.2(E, scale='none', Colv=NA, density.info = 'none', trace='none', col=palete)

plot(ca(data2), mass=c(T,T))

# msc_pracy (niewykorzystany) ---------------------------------------------------------------

data = msc_pracy
data = data[-1,]
rownames(data) <- data$Nazwa
head(data)
data2 <- data[,c(4:5)]
#rownames(data) <- data$Nazwa
head(data2)
data2 <- as.matrix(data2)


colnames(data2) = c('', 'zlikwidowane')

rownames(data2)
colSums(data2)
rowSums(data2)
head(data2)

chisq.test(data2)

palete <- rev(brewer.pal(11,"Spectral"))

P <- data2/sum(data2)
PP <- outer(rowSums(P), colSums(P))
E <- (P - PP) / sqrt(PP)
heatmap(E, scale='none', Colv=NA, col=palete)
heatmap.2(E, scale='none', Colv=NA, density.info = 'none', trace='none', col=palete)

plot(ca(data2), mass=c(T,T))


# Anazlia wieloczynnikowa - Herbata ---------------------------------------------------------------
require(FactoMineR)
require(ggplot2)
data(tea)
newtea = tea[, c("sugar", "frequency", 'healthy', 'effect.on.health')] # 'age_Q',


cats = apply(newtea, 2, function(x) nlevels(as.factor(x)))
cats
newtea
mca1 = MCA(newtea)

(mca1_vars_df = data.frame(mca1$var$coord, Variable = rep(names(cats), 
                                                          cats)))
mca1_obs_df = data.frame(mca1$ind$coord)
data("esoph")
(mca2_vars_df = data.frame(mca2$var$coord))
(mca2_obs_df = data.frame(mca2$ind$coord))
ggplot(data = mca1_vars_df, aes(x = Dim.1, y = Dim.2, label = rownames(mca1_vars_df))) + 
  geom_hline(yintercept = 0, colour = "gray70") + geom_vline(xintercept = 0, colour = "gray70") + 
  geom_text(aes(colour = Variable))  


require(MASS, quietly = TRUE)
mca2 = mca(newtea, nf = 5)
mca2$d^2
mca2_vars_df = data.frame(mca2$cs, Variable = rep(names(cats), cats))
ggplot(data = mca1_obs_df, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(colour = "gray50", alpha = 0.7) +
  geom_density2d(colour = "gray80") +
  geom_text(data = mca1_vars_df, aes(x = Dim.1, y = Dim.2, label = rownames(mca1_vars_df), colour = Variable)) +
  ggtitle("MCA plot of variables using R package FactoMineR") +
  scale_colour_discrete(name = "Variable")


# Istotnosc --------------------------------------------------------------

dane = newtea
for (var1 in 1:4){ for (var2 in 4:1) {
  contingency <- table(dane[,var1], dane[, var2]) 
  chi2 <- chisq.test(contingency) 
  writeLines( paste("p-Value for", colnames(dane)[var1], "and", colnames(dane)[var2],chi2$p.value)) }
}
# Analiza Wieloczynnikowa - Poison (niewykorzystany)  ---------------------------------------------

library("FactoMineR")
library("factoextra")
data(poison)
head(poison)
poison.active <- poison[1:55, 5:15]
head(poison.active[, 1:6], 3)
res.mca <- MCA(poison.active, graph = FALSE)


fviz_mca_biplot(res.mca, 
                repel = TRUE, # Avoid text overlapping (slow if many point)
                ggtheme = theme_minimal())


fviz_mca_var(res.mca, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())

fviz_mca_var(res.mca, 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())



####################################### Skalowanie Wielowymiarowe
# Skalowanie Wielowymiarowe - Niemetryczne ----------------------------
data("mtcars")
res.cor <- cor(mtcars, method = "spearman")
mds.cor <- (1 - res.cor) %>%
  cmdscale() %>%
  as_tibble()
colnames(mds.cor) <- c("Dim.1", "Dim.2")
ggscatter(mds.cor, x = "Dim.1", y = "Dim.2", 
          size = 1,
          label = colnames(res.cor),
          repel = TRUE)
# Skalowanie Wielowymiarowe - Metryczne-------------------------------------------------

data("swiss")
head(swiss)

swiss = swiss[,c(1,2,5,6)]

# Load required packages
library(magrittr)
library(dplyr)
library(ggpubr)
# Cmpute MDS
mds <- swiss %>%
  dist() %>%          
  cmdscale() %>%
  as_tibble()
colnames(mds) <- c("Dim.1", "Dim.2")
# Plot MDS
ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          label = rownames(swiss),
          size = 1,
          repel = TRUE)

# K-means clustering
clust <- kmeans(mds, 3)$cluster %>%
  as.factor()
mds <- mds %>%
  mutate(groups = clust)
# Plot and color by groups
ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          label = rownames(swiss),
          color = "groups",
          palette = "jco",
          size = 1, 
          ellipse = TRUE,
          ellipse.type = "convex",
          repel = TRUE)

# Skalowanie Wielowymiarowe - Swiss ---------------------------------------

data("swiss")
head(swiss)

# Load required packages
library(magrittr)
library(dplyr)
library(ggpubr)
# Cmpute MDS
mds <- swiss %>%
  dist() %>%          
  cmdscale() %>%
  as_tibble()
colnames(mds) <- c("Dim.1", "Dim.2")
# Plot MDS
ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          label = rownames(swiss),
          size = 1,
          repel = TRUE)

# K-means clustering
clust <- kmeans(mds, 3)$cluster %>%
  as.factor()
mds <- mds %>%
  mutate(groups = clust)
# Plot and color by groups
ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          label = rownames(swiss),
          color = "groups",
          palette = "jco",
          size = 1, 
          ellipse = TRUE,
          ellipse.type = "convex",
          repel = TRUE)

# Wykład (niewykorzystany) --------------------------------------------------------------------
J = c(0.83, 0.88, 0.69, 0.92, 0.63, 0.91, 0.83)
D = c(0.53, 0.61, 0.69, 0.64, 0.52, 0.37, 0.64)
B = c(0.41, 0.56, 0.39, 0.64, 0.38, 0.67, 0.52)
M = c(0.19, 0.16, 0.11, 0.44, 0.23, 0.11, 0.19)
A = c(0.32, 0.35, 0.43, 0.39, 0.58, 0.27, 0.37)
F = c(0.59, 0.45, 0.67, 0.23, 0.65, 0.25, 0.44)
M <- rbind(J,D,B,M,A,F) 
M
d <- dist(M)
d
cmds<-cmdscale(d,k=2,add=T,eig=T,x.ret=T)
x<-cmds$points[,1]
y<-cmds$points[,2]
plot(x,y,type='n')
text(x,y,labels=rownames(M))
print(cmds) 
points_cmd<-cmds$points

#metody nieparametryczne
library(MASS) 
d2<-dist(M,method='euclidean') 
mds<-isoMDS(d2)
plot(mds$points ,type='n') 
text(mds$points , label=rownames(M)) 
points_mds<-mds$points

# Dopasowanie konfiguracji punktów otrzymanej za pomocą funkcji cmdscale do konfiguracji otrzymanej
# przy pomocy isoMDS
#install.packages('shapes')
library(shapes) 
A<-as.matrix(points_cmd) 
B<-as.matrix(points_mds)
par(pty='s')
out<-procOPA(A,B,scale=T, reflect=TRUE) 
plot(out$Bhat,type=’n’,xlim=c(-0.8,0.9),ylim=c
                                             (-0.4,0.4)) text(out$Bhat,labels=as.character(rownames(points_mds)
                                             ))


