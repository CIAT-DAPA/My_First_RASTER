################
###########
###########  Extract raster info for unsupervised classification

rm(list =ls())

list.of.packages <- c("rgdal","raster","sp","ff","cclust","FactoMineR","dbscan","caret","RColorBrewer","gmum.r", "ff")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
sapply(list.of.packages,require,character.only= T)




###########---> Functions

DefineColors=function(clusterArray){
  cols=colorRampPalette(brewer.pal(length(unique(clusterArray)),"Accent"))
  colors=cols(length(unique(clusterArray)))
  names(colors) = unique(clusterArray)
  return(colors)
}

graph_tsneFunction=function(clusterArray=1,dataT_SNE=datat,MethodRed="PCA",graph=T){
  if(clusterArray==1){
    clusterArray=rep(1,dim(datat$Y)[2])
  }
  colors=DefineColors(clusterArray)
  datosColor=factor(clusterArray)
  levels(datosColor)=colors
  if(MethodRed=="PCA"){
    dataGraph=data.frame(dataT_SNE)
  }else{
    dataGraph=data.frame(dataT_SNE$Y)
  }
  dataGraph$Color=datosColor
  m=ggplot(dataGraph,aes( dataGraph[,1], dataGraph[,2],colour=Color))+geom_point(colour=datosColor)

  if(graph){
    dir.create("TSNEGraphs")
    ggsave(m,filename = paste0("TSNEGraphs/TSNE_RedVar_Norm_Per_",dataT_SNE$perplexity,".jpg"))
  }
  print(m)
  return(colors)
}


varCorrelacionadas=function(matrixCor=matrixCor2){
  allVariables=colnames(matrixCor)
  Tablecount=data.frame(Variable=allVariables,count=0)
  Tableord=matrix(ncol = length(allVariables), nrow = length(allVariables),0)
  Tableord=data.frame(Tableord)
  names(Tableord)=allVariables
  row.names(Tableord)=allVariables
  
  for(i in 1:dim(Tableord)[1]){
    Tableord[i,which(matrixCor[i,]>0.85 | matrixCor[i,]< -0.85)]=Tableord[i,which(matrixCor[i,]>0.85 | matrixCor[i,]< -0.85)]+1
    Tablecount[i,2]=sum(matrixCor[i,]>0.85 | matrixCor[i,]< -0.85)
  }
  listTables=list(Tablecount,Tableord)
  names(listTables)=c("Conteo","Variables")
  return(listTables)
}

###########---> folder paths


VectorFile="//dapadfs/workspace_cluster_8/AEPS/HONDURAS/SHP_File"
ElevationFile="//dapadfs/workspace_cluster_8/AEPS/HONDURAS/Elevacion/"
SoilFiles="//dapadfs/workspace_cluster_8/AEPS/HONDURAS/Suelo/"
ClimeFiles="//dapadfs/workspace_cluster_8/AEPS/HONDURAS/Clima/WorldClim"
Ourset <- "D:/OneDrive - CGIAR/My_first_RASTER/My_First_RASTER"

###########---> Read tif Layers

setwd(SoilFiles)
listFiles=list.files(pattern = "*.tif$")
soilsInfo=stack(listFiles)

setwd(ClimeFiles)
listFiles=list.files(pattern = "*.tif$")
ClimeInfo=stack(listFiles)

setwd(ElevationFile)
listFiles=list.files(pattern = "*.tif$")
ElevationInfo=stack(listFiles[1])

###########---> Homogenized coordinates system
##Information of soil.
crsSystem=ClimeInfo@crs
soilsInfo_Projected=projectRaster(soilsInfo, crs = crsSystem)
plot(soilsInfo_Projected)


###########---> Calculate slope and Aspect from DEM

SlopeAspectInfo=slopeAspect(ElevationInfo[[1]], out=c('slope', 'aspect'), unit='radians',  neighbors=8)
SlopeAspectInfo=stack("terrainPropertiesCholCop.tif")
names(SlopeAspectInfo)=c('slope', 'aspect')
plot(SlopeAspectInfo)
plot(ElevationInfo)

###### Get Coordinates 

rasterRef=ClimeInfo$bio_10
plot(rasterRef)
levelsWNA=which(!is.na(rasterRef[]))
coordinatesExtract=xyFromCell(rasterRef,levelsWNA)
plot(coordinatesExtract)

###### save information in a table

numImages=sum(nlayers(ElevationInfo),nlayers(SlopeAspectInfo),nlayers(soilsInfo_Projected),nlayers(ClimeInfo))
LayerNames=c("ElevationInfo","SlopeAspectInfo","soilsInfo_Projected","ClimeInfo")


###########---> Extract information from the raster files

dataImages= ff(vmode="double",dim=c(length(levelsWNA),numImages))

posCol=0
for(i in 1:length(LayerNames)){
  LayerStack=LayerNames[i]
  CellValuesExtract=cellFromXY(xy = coordinatesExtract,object = eval(parse(text=LayerStack)))
  posCol=posCol+nlayers(eval(parse(text=LayerStack)))
  levelsCol=((posCol-nlayers(eval(parse(text=LayerStack))))+1):posCol
  dataImages[,levelsCol]=eval(parse(text=LayerStack))[CellValuesExtract]
}


dataExport=data.frame(as.matrix(dataImages[1:nrow(dataImages),]))

posCol=0
for(i in 1:length(LayerNames)){
  LayerStack=LayerNames[i]
  posCol=posCol+nlayers(eval(parse(text=LayerStack)))
  levelsCol=((posCol-nlayers(eval(parse(text=LayerStack))))+1):posCol
  names(dataExport)[levelsCol]=names(eval(parse(text=LayerStack)))
}
names(dataExport)

save(dataExport,file="//dapadfs/workspace_cluster_8/AEPS/HONDURAS/RData/AllVariablesCholuCopan.RData")
load(file="//dapadfs/workspace_cluster_8/AEPS/HONDURAS/RData/AllVariablesCholuCopan.RData")


###########---> Finding Correlation Variables

## avlaute if there are pixels with NA values

SecondLevelsWNA=unique(unlist(sapply(1:ncol(dataExport),function(x){
  which(is.na(dataExport[,x]))
})))

AllVariables=list(SecondLevelsWNA,levelsWNA)
save(AllVariables,file="//dapadfs/workspace_cluster_8/AEPS/HONDURAS/RData/Levels_WNA.RData")

matrixCor=cor(as.matrix(dataExport[-c(SecondLevelsWNA),]))

highlyCorDescr <- findCorrelation(matrixCor, cutoff = .90)
dataKmeans=dataExport[-c(SecondLevelsWNA),-c(highlyCorDescr)]



###########---> Normalize Data
setwd("//dapadfs/workspace_cluster_8/AEPS/HONDURAS/ ")

dimData <- dim(dataKmeans)


normMatkMeans=apply(dataKmeans,2,function(x)(x-min(x))/(max(x)-min(x)))
normMatkMeans = SIS::standardize(dataKmeans)
dataRange=preProcess(dataKmeans,method="range")
dataTSNE=tsne::tsne(dataKmeans)


###########--->###########--->###########--->###########--->###########--->###########--->


###########---> Normalize --> reduction Dimension (TSNE) --> Cluster


dataKmeans=dataExport[-c(SecondLevelsWNA),]
normMatkMeans=apply(dataKmeans,2,function(x)(x-min(x))/(max(x)-min(x)))

dataTSNE=Rtsne(normMatkMeans,dims=2,perplexity = 30,verbose = T,max_iter = 500)

save(dataTSNE,file = "//dapadfs/workspace_cluster_8/AEPS/HONDURAS/RData/RtSNE_Norm_AllVariables.RData")
load(file = "//dapadfs/workspace_cluster_8/AEPS/HONDURAS/RData/RtSNE_Norm_AllVariables.RData")
setwd("//dapadfs/workspace_cluster_8/AEPS/HONDURAS/")
library(Rtsne)
if(file.exists("RData/RtSNE_Norm_AllVariables_MultiplePerplexity.RData")){
  dataTSNE=lapply(seq(30,300,20),function(x){Rtsne(normMatkMeans,dims=2,perplexity = x,verbose = T,max_iter = 500,pca = F)})
  save(dataTSNE,file = "RData/RtSNE_Norm_AllVariables_MultiplePerplexity.RData")
}else{
  load(file = "RData/RtSNE_Norm_AllVariables_MultiplePerplexity.RData")
  
}


### Multiple kames tot withniss

k.max <- 50
data <- as.matrix(dataTSNE[[1]]$Y)
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 100)$tot.withinss})


plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

m=kmeans(data, 11, nstart=50,iter.max = 100)

colors = rainbow(length(unique(m$cluster)))
names(colors) = unique(m$cluster)
plot(dataTSNE[[1]]$Y, t='n', main="tsne")
text(dataTSNE[[1]]$Y, labels=m$cluster, col=colors[m$cluster])


dataFrameCluster=data.frame(Levels=1:nrow(dataExport),Cluster=NA)
dataFrameCluster[-c(SecondLevelsWNA),]$Cluster=fit$cluster

### Map Cluster
plot(rasterRef)
rasterRef[]=NA
rasterRef[levelsWNA]=dataFrameCluster[,2]
plot(rasterRef)
dataFrameCluster[]


########### Remove Variables --> Normalize --> reduction Dimension (TSNE) --> Cluster



dataTSNE=tsne::tsne(normMatkMeans)

save(dataTSNE,file = "//dapadfs/workspace_cluster_8/AEPS/HONDURAS/RData/tSNE_Norm_RedcVariables.RData")

dataRange=predict(dataRange,dataKmeans)
### Multiple kames tot withniss

k.max <- 50
data <- as.matrix(dataTSNE)
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 100)$tot.withinss})


plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


m=kmeans(data, 10, nstart=50,iter.max = 100)

dataFrameCluster=data.frame(Levels=1:nrow(dataExport),Cluster=NA)
dataFrameCluster[-c(SecondLevelsWNA),]$Cluster=m$cluster

### Map Cluster
plot(rasterRef)
rasterRef[]=NA
rasterRef[levelsWNA]=dataFrameCluster[,2]
plot(rasterRef)
dataFrameCluster[]




########## --> Normalize --> reduction Dimension (PCA) --> Cluster

dataPCANorm=FactoMineR::PCA(normMatkMeans,ncp = 8)

k.max <- 70
data <- as.matrix(dataPCANorm$ind$coord)
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 100)$tot.withinss})
hclust(data)

dir.create("PCAGraphs/")
jpeg('PCAGraphs/SumerrorSquares_kmeans_PCA_RedcVariables_Norm_dim8.jpg')
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
dev.off()

NumCluster=15
for(NumCluster in 8:30){
  m=kmeans(data,NumCluster, nstart=50,iter.max = 100)
  
  dataFrameCluster=data.frame(Levels=1:nrow(dataExport),Cluster=NA)
  dataFrameCluster[-c(SecondLevelsWNA),]$Cluster=m$cluster
  clusterArray=m$cluster
  colors = rainbow(length(unique(clusterArray)))
  names(colors) = unique(clusterArray)
  datosColor=factor(clusterArray)
  levels(datosColor)=colors
  
  ### Map Cluster

  rasterRef[]=NA
  rasterRef[levelsWNA]=dataFrameCluster[,2]
  
  jpeg(paste0('PCAGraphs/Map_kmeans_PCA_RedcVariables_Norm_dim8_NClu',NumCluster,'.jpg'), width = 1200, height = 800)
  plot(rasterRef,col=colors)
  dev.off()
  
}
dataFrameCluster[]

#################### Remove Variables --> Normalization --> PCA --> dbscan
x11()
dataPCANorm=FactoMineR::PCA(normMatkMeans,ncp = 11)
x11()
kNNdistplot(dataPCANorm$ind$coord, k=(dim(dataPCANorm$ind$coord)[2]+1))
dataDBSCAN=dataPCANorm$ind$coord
for(DistDb in seq(0.3,2.7,0.3)){
  
  dbResults <- dbscan::dbscan(dataDBSCAN, eps=DistDb, minPts=(dim(dataDBSCAN)[2]+1)) 
  colors=graptsneFunction(clusterArray = dbResults$cluster,dataDBSCAN,MethodRed = "PCA",graph=F)
  
  dataFrameCluster=data.frame(Levels=1:nrow(dataExport),Cluster=NA)
  dataFrameCluster[-c(SecondLevelsWNA),]$Cluster=dbResults$cluster
  length(unique(dbResults$cluster))
  ### Map Cluster
  
  rasterRef[]=NA
  rasterRef[levelsWNA]=dataFrameCluster[,2]
  
  jpeg(paste0('PCAGraphs/Map_DBscan_PCA_RedcVariables_Norm_Dim_11_eps_',DistDb,'_NClu_',length(unique(dbResults$cluster)),'.jpg'), width = 1200, height = 800)
  plot(rasterRef,col=colors)  
  dev.off()
  
}

#################### Remove Variables --> Normalization --> tsne --> dbscan

load(file = "//dapadfs/workspace_cluster_8/AEPS/HONDURAS/RData/RtSNE_Norm_AllVariables.RData")
load(file = "//dapadfs/workspace_cluster_8/AEPS/HONDURAS/RData/tSNE_Norm_RedcVariables.RData")

dataKmeans=dataExport[-c(SecondLevelsWNA),-c(highlyCorDescr)]

normMatkMeans=apply(dataKmeans,2,function(x)(x-min(x))/(max(x)-min(x)))
if(!file.exists("RData/RtSNE_Norm_RedVariables_MultiplePerplexity.RData")){
  dataTSNE=lapply(seq(30,300,20),function(x){Rtsne(normMatkMeans,dims=2,perplexity = x,verbose = T,max_iter = 500,pca = F)})
  save(dataTSNE,file = "//dapadfs/workspace_cluster_8/AEPS/HONDURAS/RData/RtSNE_Norm_RedVariables_MultiplePerplexity.RData")
}else{
  load(file = "//dapadfs/workspace_cluster_8/AEPS/HONDURAS/RData/RtSNE_Norm_RedVariables_MultiplePerplexity.RData")
}


dataTSNE=lapply(seq(30,300,20),function(x){Rtsne(normMatkMeans,dims=2,perplexity = x,verbose = T,max_iter = 500)})
save(dataTSNE,file = "//dapadfs/workspace_cluster_8/AEPS/HONDURAS/RData/RtSNE_Norm_RedVariables_MultiplePerplexity.RData")

prueba=Rtsne(normMatkMeans,dims=2,perplexity = 1200,verbose = T,max_iter = 500)

clusters=rep(10,length(normMatkMeans[,1]))
lapply(dataTSNE,function(datat){graptsneFunction(clusterArray = clusters,datat,graph=T)})
x11()
graptsneFunction(clusterArray = clusters,MethodRed = "RTSNE",prueba,graph=T)

datacl=dataTSNE[[9]]
datacl$perplexity

kNNdistplot(datacl$Y, k=(dim(datacl$Y)[2]+1))
lapply(dataTSNE,function(datacl){
  for(DistDb in seq(0.4,0.8,0.02)){
    
    dbResults <- dbscan::dbscan(datacl$Y, eps=DistDb, minPts=(dim(datacl$Y)[2]+1)) 
    colors=graptsneFunction(clusterArray = dbResults$cluster,datacl,graph=F)
    
    dataFrameCluster=data.frame(Levels=1:nrow(dataExport),Cluster=NA)
    dataFrameCluster[-c(SecondLevelsWNA),]$Cluster=dbResults$cluster
    length(unique(dbResults$cluster))
    ### Map Cluster
    
    rasterRef[]=NA
    rasterRef[levelsWNA]=dataFrameCluster[,2]
    
    jpeg(paste0('TSNEGraphs/Map_DBscan_TSNE_RedcVariables_Norm_Per_',datacl$perplexity,'_eps_',DistDb,'_NClu_',length(unique(dbResults$cluster)),'.jpg'), width = 1200, height = 800)
    plot(rasterRef,col=colors)  
    dev.off()
    
  }
})

#################### Remove Variables --> Normalization --> PCA --> Growing Nueral Network




#################### Remove Variables --> Normalization --> tsne --> Growing Nueral Network
load(file = "RData/RtSNE_Norm_AllVariables_MultiplePerplexity.RData")
dataTSNE[[13]]$perplexity

datatest=dataTSNE[[13]]


Valgng <- GNG(datatest$Y,max.nodes = 11)

colors=graptsneFunction(clusterArray = clustering(Valgng),dataT_SNE = datatest,MethodRed = "TSNE",graph = F)



lapply(dataTSNE,function(datacl){
  for(DistDb in seq(8,30,1)){
    
    Valgng <- GNG(datacl$Y,max.nodes = DistDb)
    colors=graptsneFunction(clusterArray = clustering(Valgng),dataT_SNE = datacl,MethodRed = "TSNE",graph = F)
    
    
    dataFrameCluster=data.frame(Levels=1:nrow(dataExport),Cluster=NA)
    dataFrameCluster[-c(SecondLevelsWNA),]$Cluster=clustering(Valgng)
    cat(length(unique(clustering(Valgng))))
    ### Map Cluster
    
    rasterRef[]=NA
    rasterRef[levelsWNA]=dataFrameCluster[,2]
    
    jpeg(paste0('TSNEGraphs/Map_GNG_TSNE_RedVariables_Norm_Per_',datacl$perplexity,'_NClu_',length(unique(clustering(Valgng))),'.jpg'), width = 1200, height = 800)
    plot(rasterRef,col=colors)  
    dev.off()
    
  }
})


dataFrameCluster=data.frame(Levels=1:nrow(dataExport),Cluster=NA)
dataFrameCluster[-c(SecondLevelsWNA),]$Cluster=clustering(Valgng)
length(unique(clustering(Valgng)))
t(table(clustering(Valgng)))
### Map Cluster
plot(rasterRef)
rasterRef[]=NA
rasterRef[levelsWNA]=dataFrameCluster[,2]
plot(rasterRef,col=colors)

plot(dataTSNE$Y, t='n', main="tsne")
text(dataTSNE$Y, labels=m$cluster, col=colors[m$cluster])

dataFrameCluster[]

nb <- NbClust(as.matrix(dataImages[-c(SecondLevelsWNA),]), diss=NULL, distance = "euclidean", 
              min.nc=2, max.nc=5, method = "kmeans", 
              index = "all", alphaBeale = 0.1)



###################ml clust

library(mclust)
normMatkMeans=apply(dataKmeans,2,function(x)(x-min(x))/(max(x)-min(x)))
fit <- Mclust(normMatkMeans,verbose = T,G=6:20)
plot(fit) # plot results 
summary(fit, parameters = TRUE)

dataPCANorm=FactoMineR::PCA(normMatkMeans,ncp = 11)
fitPCA <- Mclust(dataPCANorm$ind$coord,verbose = T,G=8:24)

save(fitPCA,file="RData/PCA_mclust_norm_RedVariables_Dim11.RData")
fit$classification
plot(fitPCA)



load(file = "//dapadfs/workspace_cluster_8/AEPS/HONDURAS/RData/RtSNE_Norm_RedVariables_MultiplePerplexity.RData")
dataTSNE[[13]]$perplexity
fitTSNE <- Mclust(dataTSNE[[13]]$Y,verbose = T,G=6:20)
colors=graptsneFunction(clusterArray = fitTSNE$classification,dataT_SNE = dataTSNE[[13]],MethodRed = "TSNE",graph = F)



dataFrameCluster=data.frame(Levels=1:nrow(dataExport),Cluster=NA)
dataFrameCluster[-c(SecondLevelsWNA),]$Cluster=fitPCA$classification

t(table(fitPCA$classification))
### Map Cluster
plot(rasterRef)
rasterRef[]=NA
rasterRef[levelsWNA]=dataFrameCluster[,2]
plot(rasterRef)


#### Unsupervised Classification Hierarchical Clustering on Principal Components


library(FactoMineR)
data(tea)

dataframedata=as.data.frame(dataImages[-c(SecondLevelsWNA),])

res.mca <- PCA(dataframedata,ncp = 14)

summary(res.mca)
res.mca = MCA(dataframedata, quanti.sup=1:74,ncp=20, graph=TRUE)
vres.hcpc = HCPC(res.mca)
res.hcpc = HCPC(res.mca, nb.clust=1, conso=0, min=6, max=6)
res.hcpc

data(hobbies)

data(wine)
hierar <- list(1:3,4:7, 8:74)
res.hmfa <- HMFA(dataframedata, H = hierar)

PCASoil=FactoMineR::PCA(dataframedata,ncp = 14)


#### Unsupervised Classification DBSCAN
kNNdistplot(as.matrix(dataImages[-c(SecondLevelsWNA),]), k=74+1)
dbResults <- dbscan::dbscan(as.matrix(dataImages[-c(SecondLevelsWNA),]), eps=160, minPts=74+1) 

### Map Cluster

dataFrameCluster=data.frame(Levels=1:nrow(dataImages),Cluster=NA)
dataFrameCluster[-c(SecondLevelsWNA),]$Cluster=dbResults$cluster

#plot(rasterRef)
rasterRef[]=NA
rasterRef[levelsWNA]=dataFrameCluster[,2]
plot(rasterRef)
dataFrameCluster[]

################## other test


install.packages("SIS")
library(SIS)

set.seed(0)
n = 400; p = 50; rho = 0.5
corrmat = diag(rep(1-rho, p)) + matrix(rho, p, p)
corrmat[,4] = sqrt(rho)
corrmat[4, ] = sqrt(rho)
corrmat[4,4] = 1
corrmat[,5] = 0
corrmat[5, ] = 0
corrmat[5,5] = 1
cholmat = chol(corrmat)
x = matrix(rnorm(n*p, mean=0, sd=1), n, p)
x = x%*%cholmat

b = c(4,4,4,-6*sqrt(2),4/3)
y=x[, 1:5]%*%b + rnorm(n)
model11=SIS(x, y, family='gaussian', tune='bic')
model12=SIS(dataImages[-c(SecondLevelsWNA),], family='gaussian', tune='bic', varISIS='aggr', seed=11)
x.standard = standardize(dataImages[-c(SecondLevelsWNA),])


library(cluster)
data(ruspini)
pr4 <- pam(ruspini, 4)
str(si <- silhouette(pr4))
(ssi <- summary(si))
plot(si) # silhouette plot
plot(si, col = c("red", "green", "blue", "purple"))# with cluster-wise coloring

si2 <- silhouette(m$cluster, dist(dataImages[-c(SecondLevelsWNA),], "canberra"))
summary(si2) # has small values: "canberra"'s fault
plot(si2, nmax= 80, cex.names=0.6)
install.packages(mLc)

###################################################
################# Validate best Cluster

## PCA -> k means
NumCluster=12
m=kmeans(data,NumCluster, nstart=50,iter.max = 100)

dataFrameCluster=data.frame(Levels=1:nrow(dataExport),Cluster=NA)
dataFrameCluster[-c(SecondLevelsWNA),]$Cluster=m$cluster
clusterArray=m$cluster
colors = rainbow(length(unique(clusterArray)))
names(colors) = unique(clusterArray)
datosColor=factor(clusterArray)
levels(datosColor)=colors

### Map Cluster

rasterRef[]=NA
rasterRef[levelsWNA]=dataFrameCluster[,2]
plot()

## dbscanCluster

## GNG Cluster

DistDb=4
datacl=dataTSNE[[13]]
datacl$perplexity
lapply(dataTSNE,function(datacl){
  #for(DistDb in seq(4,30,1)){
    
    Valgng <- GNG(datacl$Y,max.nodes = 14)
    colors=graph_tsneFunction(clusterArray = clustering(Valgng),dataT_SNE = datacl,MethodRed = "TSNE",graph = T)
    
    
    dataFrameCluster=data.frame(Levels=1:nrow(dataExport),Cluster=NA)
     dataFrameCluster[-c(SecondLevelsWNA),]$Cluster=clustering(Valgng)
    # cat(length(unique(clustering(Valgng))))
    ### Map Cluster

     rasterRef[]=NA
     rasterRef[levelsWNA]=dataFrameCluster[,2]
    #
    # jpeg(paste0('TSNEGraphs/Map_GNG_TSNE_AllVariables_Norm_Per_',datacl$perplexity,'_NClu_',length(unique(clustering(Valgng))),'.jpg'), width = 1200, height = 800)
    plot(rasterRef,col=colors)
    # dev.off()
    writeRaster(rasterRef, filename="TSNEGraphs/Raster_gng_tsne_RedcVariables_Norm_per270_NClu14.tif", format="GTiff", overwrite=T)
    
  .#}
})



##Plots
setwd(Ourset)
pdf("./Graphics/Solis_Info.pdf")
plot(soilsInfo_Projected)
dev.off()


pdf("./Graphics/SlopeAspect_Info.pdf")
plot(SlopeAspectInfo)
dev.off()


pdf("./Graphics/ElevationAspect_Info.pdf")
plot(ElevationInfo)
dev.off()

pdf("./Graphics/rasterRef.pdf")
plot(rasterRef)
dev.off()

pdf("./Graphics/coordinatesExtract.pdf")
plot(coordinatesExtract)
dev.off()
