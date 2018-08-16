
library(stringr)
library(ggplot2)
library(grid)
library(xlsx)
library(gdata)
library(gmum.r)
library(raster)
library(caret)
# Zonificaci?n M?xico
# Hugo Andres y Andres Aguilar
# 22-09-2017

rm(list =ls())

setwd("//dapadfs/workspace_cluster_8/AEPS/CIMMYT_2016/Clustering_Zonificacion/DATOS_CLUSTERING_ZONIFICACION")

libs=c("dbscan",'caret','FactoMineR',"rgdal","raster","sp","rgeos","sf", 'tmaptools','Rtsne')
sapply(libs,require,character.only= T)


# Cargar filtros e informacion que va a ser utilizada en la zonificacion

load("filtro_final.RData")

load("stack_fix_climate.RData")

load("stack_fix_soil.RData")

load("Altitud.RData")

# Extraer valores con referencia al sitio de analisis

rasterRef <- stack_fix_climate[[1]] # referencia de un raster ya cortado

levelsWNA <- which(!is.na(rasterRef[])) # Extraer referencia de celdas

coordenadasRef <- xyFromCell(rasterRef,levelsWNA) # Extraer coordenadas

dataSoil <- extract(list_soil_stac,coordenadasRef) # Extraer datos de suelo 

dataWeather <- extract(stack_fix_climate,coordenadasRef) # Extraer datos de clima

datafiltro <- extract(filtro_final,coordenadasRef) # Extraer filtro final

Sloperast <- terrain(newAltitude, out=c('slope'), unit='radians',  neighbors=8)

windows()

plot(Sloperast)

dataSlope=extract(Sloperast,coordenadasRef) # Extraer la pendiente

dataAltitude=extract(newAltitude,coordenadasRef) # Extraer alitud

dataClasificacion=cbind(dataSoil,dataWeather,Altitude = dataAltitude,slope= dataSlope) # Juntar bases de datos

dataClasificacion=as.data.frame(dataClasificacion) 


# Aplicar filtros finalese

dataClasificacion$Altitude[dataClasificacion$Altitude <0 | dataClasificacion$Altitude >3000] <- NA #Altura menor a 3000

dataClasificacion$slope[dataClasificacion$slope>=0.60] <- NA # Pendiente menor a 0.60

SecondLevelsWNA <- which(!complete.cases(dataClasificacion)) # Detectamos faltantes

dataClasificacionNoNA <- dataClasificacion[-SecondLevelsWNA,] # Removemos faltantes

head(dataClasificacionNoNA)

#Normalizamosd

modNorm <- preProcess(dataClasificacionNoNA,method = "range")

dataClasificacionNorm <- predict(modNorm,dataClasificacionNoNA)

# Reduccion de dimensiones

# Utilizando PCA

PCAdataClss<- PCA(dataClasificacionNorm,ncp = 7) # PCA

PCAdataClss$eig

# k medias para cluster

k.max <- 40

data <- as.matrix(PCAdataClss$ind$coord)

set.seed(1234)

wss <- sapply(5:k.max, 
              function(k){kmeans(data, k, iter.max = 100)$tot.withinss})

# Grafico de reduccion de varianza entre cluster

png("total_within-clust.png",width = 600,height =  370)
plot(5:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
dev.off()

# Cluster definitivo con k means

cluster.kmeans <- kmeans(data, 38, iter.max = 10000)

dataClust <- data.frame(nceldas=1:nrow(dataClasificacion),cluster=NA) # Generar columnas vacias de cluster

dataClust$cluster[-c(SecondLevelsWNA)]<- cluster.kmeans$cluster # Introducir los datos faltantes en cluster

rasterRef[] <- NA 

rasterRef[levelsWNA] <- dataClust[,2]

plot(rasterRef)

writeRaster(rasterRef, filename="Classkmeans_38clust.tif", format="GTiff", overwrite=T)

matrix.Clasificacion <- data.frame(dataClust,dataClasificacion)

matrix.Clasificacion <- matrix.Clasificacion[complete.cases(matrix.Clasificacion),]

save(matrix.Clasificacion,file="matrix.Clasificacion.kmeans38.RData")

nam <- names(matrix.Clasificacion)


#----

clustResults <- Mclust( data , G = 38)

#Realizar graficos


c <- c(1,2,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4)


vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)


#pdf primer grupo

library(gridExtra)

pdf("descrip_cluster_1.pdf",width = 10,height = 12)
grid.newpage()
pushViewport(viewport(layout = grid.layout(4, 1)))


for(i in 3:6)
{ 
  print(ggplot(matrix.Clasificacion,aes(x=factor(cluster),y=get(nam[i])))+geom_boxplot()+theme_bw()+ggtitle(nam[i])+
    ylab(nam[i]),vp = vplayout(i-2,1))
  #plot(factor(matrix.Clasificacion$cluster),matrix.Clasificacion$Limo_prc,las=2,main = nam[i])
}

dev.off()

for(j in seq(3,31,4)){
  
  if(j==31){
    i= j 
    g1 <- ggplot(matrix.Clasificacion,aes(x=factor(cluster),y=get(nam[i])))+geom_boxplot()+theme_bw()+ggtitle(nam[i])+ylab(nam[i])    
    png(paste0("descrip_cluster_",j,".png"),height =  240,width =  800)
    print(g1)
    dev.off()
  }else{
    u = melt(matrix.Clasificacion[c(2,seq(j,length.out = 4))],id.vars = "cluster")
    

    png(paste0("descrip_cluster_",j,".png"),height =  950,width =  800)
    print(ggplot( u,aes(x=factor(cluster),y=value))+geom_boxplot()+theme_bw()+facet_grid(variable~.,scales = "free_y"))
    dev.off()
  }
}  



#pdf segundo grupo

pdf("descrip_cluster_2.pdf",width = 10,height = 12)
grid.newpage()
pushViewport(viewport(layout = grid.layout(4, 1)))


for(i in 7:10)
{ 
  print(ggplot(matrix.Clasificacion,aes(x=factor(cluster),y=get(nam[i])))+geom_boxplot()+theme_bw()+ggtitle(nam[i])+
          ylab(nam[i]),vp = vplayout(c[i],1))
  #plot(factor(matrix.Clasificacion$cluster),matrix.Clasificacion$Limo_prc,las=2,main = nam[i])
}

dev.off()

#pdf tercer grupo

pdf("descrip_cluster_3.pdf",width = 10,height = 12)
grid.newpage()
pushViewport(viewport(layout = grid.layout(4, 1)))


for(i in 11:14)
{ 
  print(ggplot(matrix.Clasificacion,aes(x=factor(cluster),y=get(nam[i])))+geom_boxplot()+theme_bw()+ggtitle(nam[i])+
          ylab(nam[i]),vp = vplayout(c[i],1))
  #plot(factor(matrix.Clasificacion$cluster),matrix.Clasificacion$Limo_prc,las=2,main = nam[i])
}

dev.off()

#pdf cuarto grupo

pdf("descrip_cluster_4.pdf",width = 10,height = 12)
grid.newpage()
pushViewport(viewport(layout = grid.layout(4, 1)))


for(i in 15:18)
{ 
  print(ggplot(matrix.Clasificacion,aes(x=factor(cluster),y=get(nam[i])))+geom_boxplot()+theme_bw()+ggtitle(nam[i])+
          ylab(nam[i]),vp = vplayout(c[i],1))
  #plot(factor(matrix.Clasificacion$cluster),matrix.Clasificacion$Limo_prc,las=2,main = nam[i])
}

dev.off()

#pdf quinto grupo

pdf("descrip_cluster_5.pdf",width = 10,height = 12)
grid.newpage()
pushViewport(viewport(layout = grid.layout(4, 1)))


for(i in 19:22)
{ 
  print(ggplot(matrix.Clasificacion,aes(x=factor(cluster),y=get(nam[i])))+geom_boxplot()+theme_bw()+ggtitle(nam[i])+
          ylab(nam[i]),vp = vplayout(c[i],1))
  #plot(factor(matrix.Clasificacion$cluster),matrix.Clasificacion$Limo_prc,las=2,main = nam[i])
}

dev.off()

#pdf sexto grupo

pdf("descrip_cluster_6.pdf",width = 10,height = 12)
grid.newpage()
pushViewport(viewport(layout = grid.layout(4, 1)))


for(i in 23:26)
{ 
  print(ggplot(matrix.Clasificacion,aes(x=factor(cluster),y=get(nam[i])))+geom_boxplot()+theme_bw()+ggtitle(nam[i])+
          ylab(nam[i]),vp = vplayout(c[i],1))
  #plot(factor(matrix.Clasificacion$cluster),matrix.Clasificacion$Limo_prc,las=2,main = nam[i])
}

dev.off()

# pdf septimo grupo

pdf("descrip_cluster_7.pdf",width = 10,height = 12)
grid.newpage()
pushViewport(viewport(layout = grid.layout(4, 1)))


for(i in 27:30)
{ 
  print(ggplot(matrix.Clasificacion,aes(x=factor(cluster),y=get(nam[i])))+geom_boxplot()+theme_bw()+ggtitle(nam[i])+
          ylab(nam[i]),vp = vplayout(c[i],1))
  #plot(factor(matrix.Clasificacion$cluster),matrix.Clasificacion$Limo_prc,las=2,main = nam[i])
}

dev.off()

# pdf octavo grupo

pdf("descrip_cluster_8.pdf",width = 10,height = 12)
grid.newpage()
pushViewport(viewport(layout = grid.layout(4, 1)))


for(i in 31)
{ 
  print(ggplot(matrix.Clasificacion,aes(x=factor(cluster),y=get(nam[i])))+geom_boxplot()+theme_bw()+ggtitle(nam[i])+
          ylab(nam[i]),vp = vplayout(c[i],1))
  #plot(factor(matrix.Clasificacion$cluster),matrix.Clasificacion$Limo_prc,las=2,main = nam[i])
}

dev.off()



tsne_end_20_1000 <- Rtsne(dataClasificacionNorm, dims = 2, pca = F,initial_dims = 50, perplexity = 20, max_iter = 1000)

tsne_end_50_1000 <- Rtsne(dataClasificacionNorm, dims = 2, pca = F,initial_dims = 50, perplexity = 30, max_iter = 1000)


tsne_end_20_1000 <- Rtsne(dataClasificacionNorm, dims = 2, pca = F,initial_dims = 50, perplexity = 300, max_iter = 100)

tsne_end_50_1000 <- Rtsne(dataClasificacionNorm, dims = 2, pca = F,initial_dims = 50, perplexity = 1000, max_iter = 100)

tsne_end_20_1000 <- Rtsne(dataClasificacionNorm, dims = 2, pca = F,initial_dims = 50, perplexity = 80, max_iter = 100)

save(PCAdataClss,tsne_end_20_1000,tsne_end_50_1000,file="reductionDim.RData")


#### Unsupervised Classification DBSCAN



dataClasificacion <- as.data.frame(dataClasificacion)



#-----------------------

kNNdistplot(as.matrix(dataClasificacion[-c(SecondLevelsWNA),]), k=ncol(dataClasificacion)+1)
dbResults <- dbscan::dbscan(as.matrix(dataClasificacion[-c(SecondLevelsWNA),]), eps=40, minPts=ncol(dataClasificacion)+1) 


dataClust=data.frame(nceldas=1:nrow(dataClasificacion),cluster=NA)
dataClust$cluster[-c(SecondLevelsWNA)]=dbResults$cluster

table(dbResults$cluster)
plot(rasterRef)
rasterRef[]=NA
rasterRef[levelsWNA]=dataClust[,2]
plot(rasterRef)


writeRaster(rasterRef, filename="ClassDBSCAN_80nn.tif", format="GTiff", overwrite=T)


#### Unsupervised Classification kmeans

library(cclust)

windows()
m=cclust(as.matrix(dataClasificacion[-c(SecondLevelsWNA),]),6,500,verbose=T,method="kmeans")


dataClust=data.frame(nceldas=1:nrow(dataClasificacion),cluster=NA)
dataClust$cluster[-c(SecondLevelsWNA)]=m$cluster

table(dataClust$cluster)
plot(rasterRef)
rasterRef[]=NA
rasterRef[levelsWNA]=dataClust[,2]
plot(rasterRef)
dataFrameCluster[]

writeRaster(rasterRef, filename="Classkmeans_6clust.tif", format="GTiff", overwrite=T)


set.seed(1990)
k.max <- 15
data <- as.matrix(dataClasificacion[-c(SecondLevelsWNA),])
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})


k.max2 <- 60
wss2 <- sapply(1:k.max2, 
               function(k){kmeans(data, k, nstart=50,iter.max = 100 )$tot.withinss})


wss2
plot(1:k.max2, wss2,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
save(wss2, file="SumSquares.RData")

kmeansClust=kmeans(data, 18, nstart=50,iter.max = 100 )

dataClust=data.frame(nceldas=1:nrow(dataClasificacion),cluster=NA)
dataClust$cluster[-c(SecondLevelsWNA)]=kmeansClust$cluster

table(dbResults$cluster)
plot(rasterRef)
rasterRef[]=NA
rasterRef[levelsWNA]=dataClust[,2]
plot(rasterRef)

#TSNE

tsne_end_20_1000 <- Rtsne(dataClasificacionNorm, dims = 2, pca = F,initial_dims = 50, perplexity = 20, max_iter = 1000)

tsne_201000 <- as.data.frame(tsne_end_20_1000$Y)

ggplot(tsne_201000,aes(x=V1,y=V2))+geom_point(size=0.0000000001)

tsne_end_50_1000 <- Rtsne(dataClasificacionNorm, dims = 2, pca = F,initial_dims = 50, perplexity = 30, max_iter = 1000)

tsne_end_501000 <- as.data.frame(tsne_end_50_1000$Y)


Valgng <- GNG(as.matrix(tsne_201000),max.nodes = 38,max.iter = 1000)

tsne_201000_clust <- data.frame(tsne_201000,clust = clustering(Valgng))

###------

dataClust_tsne <- data.frame(nceldas=1:nrow(dataClasificacion),cluster=NA) # Generar columnas vacias de cluster

dataClust_tsne$cluster[-c(SecondLevelsWNA)]<- tsne_201000_clust$clust # Introducir los datos faltantes en cluster

rasterRef[] <- NA 

rasterRef[levelsWNA] <- dataClust_tsne[,2]

plot(rasterRef)

writeRaster(rasterRef, filename="Classkmeans_38clust.tif", format="GTiff", overwrite=T)

matrix.Clasificacion <- data.frame(dataClust,dataClasificacion)

matrix.Clasificacion <- matrix.Clasificacion[complete.cases(matrix.Clasificacion),]

save(matrix.Clasificacion,file="matrix.Clasificacion.kmeans38.RData")

###------

tsne_end_80_1000 <- as.data.frame(tsne_end_20_1000$Y)


png("tsne_final_cluster.png",height =  650, width =  650)
print(ggplot(tsne_201000_clust,aes(x = V1,y =  V2)) + geom_point( aes(colour = factor(clust) )))
dev.off()

png("tsne_final.png",height =  650, width =  650)
print(ggplot(tsne_end_501000,aes(x=V1,y=V2))+geom_point(size=0.0000000001))
dev.off()

png("tsne_final_1.png",height =  650, width =  650)
print(ggplot(tsne_end_80_1000,aes(x=V1,y=V2))+geom_point(size=0.0000000001))
dev.off()


save(PCAdataClss,tsne_end_20_1000,tsne_end_50_1000,file="reductionDim.RData")

