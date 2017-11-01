rm(list = ls())
######################################################
# C L A S S I F I C A Ç ÃO   D O S   P R O D U T O S #
######################################################

# lendo os dados
setwd("C:/Users/bkuasney/Desktop/Machine Learning A-Z/DESAFIO LUIZA LABS")
dt = read.csv('desafio.csv', sep=',', dec='.', encoding = 'uft8')

# Tratamento
require(sqldf)
dtrat = sqldf('select 
              order_id,
              code,
              quantity,
              cast(price as decimal(2)),
              cast(pis_cofins as decimal(2)),
              cast(icms as decimal(2)),
              cast(tax_substitution as decimal(2)),
              category,
              cast(liquid_cost as decimal(2)),
              order_status,
              capture_date,
              process_date,
              process_status,
              source_channel
              from dt')

dcluster = sqldf('select 
                 code,
                 quantity,
                 cast(price as decimal(2)) as price,
                 cast(pis_cofins as decimal(2)) as pis_cofins,
                 cast(icms as decimal(2)) as icms,
                 cast(tax_substitution as decimal(2)) as tax_substitution,
                 cast(liquid_cost as decimal(2)) as liquid
                 from dt')
dcluster = na.omit(dcluster)

# Produtos únicos e suas quantidades.
cluster = sqldf('Select code, sum(price) as price, sum(quantity) as qtd, sum(liquid) as liquid, sum(pis_cofins) as pis, sum(icms) as icms, sum(tax_substitution) as tax
                from dcluster group by code')
row.names(cluster)=cluster[,1]


# Plot para ver outliers e retirar outliers para estimação
plot(qtd ~ price, data = cluster)
cluster = cluster[-c(25L, 27L, 28L,46L),]
d.stand <- scale(cluster[-1])

# Descobrir valores de K através da metodlogia "Elbow"
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(d.stand, nc=20)


# Descobrir valores de K através da metodologia "silhouette"
library(fpc)
library(cluster)
pamk.best <- pamk(d.stand)
plot(pam(d.stand, pamk.best$nc))
cat("number of clusters estimated by optimum average silhouette width:", pamk.best$nc, "\n")

# Visualização gráfica de Silhouette
library("factoextra")
fviz_nbclust(d.stand, kmeans, method = "silhouette")


# Função da metodologia de silhouette para buscar os valores de cada K
# silhouette maior que 0,7 indica boa modelagem
library("vegan")
dados.dist <- vegdist(d.stand,method = "euclidean")
silh  <- matrix(NA, ncol = 1, nrow = 6)
for(i in 2:6) {
  value <- summary(silhouette(cutree(hclust(dados.dist,method="average"), i), 
                              dist(dados.dist)))$avg.width  
  silh[i, ]  <- value
}
silh


# utilizando k=2 e clusterizando através de kmeans
km.res <- kmeans(d.stand, 2, nstart = 25)
fviz_cluster(km.res, data = d.stand,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())


# voltando com os outliers para identificar um valor de K bom.
cluster = sqldf('Select code, sum(price) as price, sum(quantity) as qtd, sum(liquid) as liquid, sum(pis_cofins) as pis, sum(icms) as icms, sum(tax_substitution) as tax
                from dcluster group by code')
row.names(cluster)=cluster[,1]
nrow(cluster)

# normalização dos dados
d.stand <- scale(cluster[-1])

dados.dist <- vegdist(d.stand,method = "euclidean")
silh  <- matrix(NA, ncol = 1, nrow = 6)
for(i in 2:6) {
  value <- summary(silhouette(cutree(hclust(dados.dist,method="average"), i), 
                              dist(dados.dist)))$avg.width  
  silh[i, ]  <- value
}
silh

# Resultado final da clusterização
km.res <- kmeans(d.stand, 3, nstart = 25)
# Visualize
fviz_cluster(km.res, data = d.stand,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())


# Tratamento da base para dar match entre base original e cluster
clus = as.matrix(km.res$cluster)
cluster$clus = clus[,1]

# Voltando com a base original para termos as datas e dando left com os clusters
dtfinal = sqldf('select * from dt left join cluster using(code)')

# Arrumando formato da data
library(lubridate)
dtfinal$capture_date <- mdy(dtfinal$capture_date, locale="en_US.UTF-8")
dtfinal$process_date <- mdy(dtfinal$process_date, locale="en_US.UTF-8")

dtfinal = dtfinal[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,21)]


# Salvando arquivo
# Inputar um caminho válido em setwd (início do código)
write.table(x = dtfinal, file = "dtfinal.txt",
            sep="\t", row.names = FALSE)


############################################
# P R E V I S Ã O   D O   C L U S T E R  1 #
############################################

# Tratamento
library(data.table)
library(sqldf)
library(lubridate)
tabela <- dtfinal

# Configurações
DT=subset(tabela, clus==1)
DT=aggregate(DT$quantity, list(date = DT$capture_date), sum)
DT$month_year = dmy(paste("01",substr(DT$date, 6, 7),substr(DT$date, 1, 4), sep="/"))
DT = subset(DT, month_year != "2017-06-01")
names(DT) = c("date", "value2", "month_year")
DT = aggregate(DT$value2, list(month_year = DT$month_year), sum)
names(DT) = c("month_year", "value2")
head(DT)

# Plot da série temporal
data <- ts(DT[,2], start = c(2016, 6),frequency = 12)
plot(data, xlab='Month/Years', ylab = 'Qtd')


# Característica da série temporal
acf(ts(diff(log10(data))),main='Qtd')
pacf(ts(diff(log10(data))),main='Qtd')

# Modelagem
require(forecast)
ARIMAfit = auto.arima(log10(data), approximation=FALSE,trace=FALSE)
summary(ARIMAfit)

# Validação do modelo
pred2 = forecast(ARIMAfit, h = 3)
acf(ts(pred2$residuals),main='ACF Residual')
pacf(ts(pred2$residuals),main='PACF Residual')
auto.arima(pred2$residuals, approximation=FALSE,trace=FALSE)

# Predição do modelo
pred = predict(ARIMAfit, n.ahead = 3)
pred2 = forecast(ARIMAfit, h = 3)
plot(data,type='l',xlim=c(2016.4, 2017.6),ylim=c(4000,23000),xlab = 'Year',ylab = 'Qtd')
lines(10^(pred$pred),col='blue')

# Valores obtidos da previsão
10^pred2$mean
jun1=round(10^pred2$mean[1],0)
jul1=round(10^pred2$mean[2],0)
ago1=round(10^pred2$mean[3],0)
paste("Junho: ",jun1,"itens")
paste("Julho: ",jul1,"itens")
paste("Agosto: ",ago1,"itens")



############################################
# P R E V I S Ã O   D O   C L U S T E R  2 #
############################################

# Configurações
DT=subset(tabela, clus==2)
DT=aggregate(DT$quantity, list(date = DT$capture_date), sum)
DT$month_year = dmy(paste("01",substr(DT$date, 6, 7),substr(DT$date, 1, 4), sep="/"))
DT = subset(DT, month_year != "2017-06-01")
names(DT) = c("date", "value2", "month_year")
DT = aggregate(DT$value2, list(month_year = DT$month_year), sum)
names(DT) = c("month_year", "value2")
head(DT)

# Plot da série temporal
data <- ts(DT[,2], start = c(2016, 6),frequency = 12)
plot(data, xlab='Month/Years', ylab = 'Qtd')


# Característica da série temporal
acf(ts(diff(log10(data))),main='Qtd')
pacf(ts(diff(log10(data))),main='Qtd')

# Modelagem
require(forecast)
ARIMAfit = auto.arima(log10(data), approximation=FALSE,trace=FALSE)
summary(ARIMAfit)

# Validação do modelo
pred2 = forecast(ARIMAfit, h = 3)
acf(ts(pred2$residuals),main='ACF Residual')
pacf(ts(pred2$residuals),main='PACF Residual')
auto.arima(pred2$residuals, approximation=FALSE,trace=FALSE)

# Predição do modelo
pred = predict(ARIMAfit, n.ahead = 3)
pred2 = forecast(ARIMAfit, h = 3)
plot(data,type='l',xlim=c(2016.4, 2017.6),ylim=c(min(DT[,2])-1000,11000),xlab = 'Year',ylab = 'Qtd')
lines(10^(pred$pred),col='blue')

# Valores obtidos da previsão
10^pred2$mean
jun2=round(10^pred2$mean[1],0)
jul2=round(10^pred2$mean[2],0)
ago2=round(10^pred2$mean[3],0)
paste("Junho: ",jun2,"itens")
paste("Julho: ",jul2,"itens")
paste("Agosto: ",ago2,"itens")



############################################
# P R E V I S Ã O   D O   C L U S T E R  3 #
############################################

# Configurações
DT=subset(tabela, clus==3)
DT=aggregate(DT$quantity, list(date = DT$capture_date), sum)
DT$month_year = dmy(paste("01",substr(DT$date, 6, 7),substr(DT$date, 1, 4), sep="/"))
DT = subset(DT, month_year != "2017-06-01")
names(DT) = c("date", "value2", "month_year")
DT = aggregate(DT$value2, list(month_year = DT$month_year), sum)
names(DT) = c("month_year", "value2")
head(DT)

# Plot da série temporal
data <- ts(DT[,2], start = c(2016, 6),frequency = 12)
plot(data, xlab='Month/Years', ylab = 'Qtd')


# Característica da série temporal
acf(ts(diff(log10(data))),main='Qtd')
pacf(ts(diff(log10(data))),main='Qtd')

# Modelagem
require(forecast)
ARIMAfit = auto.arima(log10(data), approximation=FALSE,trace=FALSE)
summary(ARIMAfit)

# Validação do modelo
pred2 = forecast(ARIMAfit, h = 3)
acf(ts(pred2$residuals),main='ACF Residual')
pacf(ts(pred2$residuals),main='PACF Residual')
auto.arima(pred2$residuals, approximation=FALSE,trace=FALSE)

# Predição do modelo
pred = predict(ARIMAfit, n.ahead = 3)
pred2 = forecast(ARIMAfit, h = 3)
plot(data,type='l',xlim=c(2016.4, 2017.6),ylim=c(min(DT[,2])-300,2500),xlab = 'Month/Year',ylab = 'Qtd')
lines(10^(pred$pred),col='blue')

# Valores obtidos da previsão
10^pred2$mean
jun3=round(10^pred2$mean[1],0)
jul3=round(10^pred2$mean[2],0)
ago3=round(10^pred2$mean[3],0)
paste("Junho: ",jun3,"itens")
paste("Julho: ",jul3,"itens")
paste("Agosto: ",ago3,"itens")



##############
# MERGE BASE #
##############

final = sqldf('select distinct code, clus from dtfinal')

dt_1 = subset(subset(final, clus==1))
jun1 = matrix(jun1,nrow=nrow(subset(final, clus==1)),ncol=1)
dt_1$jun = matrix(jun1,nrow=nrow(subset(final, clus==1)),ncol=1)

dt_2 = subset(subset(final, clus==2))
jun2 = matrix(jun2,nrow=nrow(subset(final, clus==2)),ncol=1)
dt_2$jun = matrix(jun2,nrow=nrow(subset(final, clus==2)),ncol=1)

dt_3 = subset(subset(final, clus==3))
jun3 = matrix(jun3,nrow=nrow(subset(final, clus==3)),ncol=1)
dt_3$jun = matrix(jun3,nrow=nrow(subset(final, clus==3)),ncol=1)

final = sqldf('select * from dt_1 union select * from dt_2 union select * from dt_3')

dt_1 = subset(subset(final, clus==1))
jul1 = matrix(jul1,nrow=nrow(subset(final, clus==1)),ncol=1)
dt_1$jul = matrix(jul1,nrow=nrow(subset(final, clus==1)),ncol=1)

dt_2 = subset(subset(final, clus==2))
jul2 = matrix(jul2,nrow=nrow(subset(final, clus==2)),ncol=1)
dt_2$jul = matrix(jul2,nrow=nrow(subset(final, clus==2)),ncol=1)

dt_3 = subset(subset(final, clus==3))
jul3 = matrix(jul3,nrow=nrow(subset(final, clus==3)),ncol=1)
dt_3$jul = matrix(jul3,nrow=nrow(subset(final, clus==3)),ncol=1)

final = sqldf('select * from dt_1 union select * from dt_2 union select * from dt_3')

dt_1 = subset(subset(final, clus==1))
ago1 = matrix(ago1,nrow=nrow(subset(final, clus==1)),ncol=1)
dt_1$ago = matrix(ago1,nrow=nrow(subset(final, clus==1)),ncol=1)

dt_2 = subset(subset(final, clus==2))
ago2 = matrix(ago2,nrow=nrow(subset(final, clus==2)),ncol=1)
dt_2$ago = matrix(ago2,nrow=nrow(subset(final, clus==2)),ncol=1)

dt_3 = subset(subset(final, clus==3))
ago3 = matrix(ago3,nrow=nrow(subset(final, clus==3)),ncol=1)
dt_3$ago = matrix(ago3,nrow=nrow(subset(final, clus==3)),ncol=1)

final = sqldf('select * from dt_1 union select * from dt_2 union select * from dt_3')

# Salvando arquivo
write.table(x = final, file = "Classificacao_predicao_final.txt",
            sep="\t", row.names = FALSE)


