#############
####
####   Extract information used for unsupervised Classification
####
####
rm(list = ls())

library(raster)
library(ff)
library(stringr)
library(caret)
library(RColorBrewer)

DefineColors=function(clusterArray){
  cols=colorRampPalette(brewer.pal(length(unique(clusterArray)),"Accent"))
  colors=cols(length(unique(clusterArray)))
  names(colors) = unique(clusterArray)
  return(colors)
}

###########---> folder paths

dirfol_principal = "//dapadfs/workspace_cluster_8/AEPS/HONDURAS/Clasificacion_no_supervisada"
setwd(dirfol_principal)


########## --- > Load data extracted

load(file="data/rdata/AllVariables_hondruras_cholu_copan.RData")
head(dataExport)
###########---> Find Correlated Variables

## remove rows tieh no data

SecondLevelsWNA=unique(unlist(sapply(1:ncol(dataExport),function(x){
  which(is.na(dataExport[,x]))
})))


## Calculate matrix correlation

matrixCor=cor(as.matrix(dataExport[-c(SecondLevelsWNA),]))

### which variables are correlated more than 90
highlyCorDescr = findCorrelation(matrixCor, cutoff = .90)

data_withoutCorrelation = dataExport[-c(SecondLevelsWNA),-c(highlyCorDescr)]


###########---> Normalize Data

dimData <- dim(data_withoutCorrelation)

normMatrix=apply(data_withoutCorrelation,2,function(x)(x-min(x))/(max(x)-min(x)))

##########################
########################## Unsupervised Classification Process
##########################

## read a raster reference

rasterRef=raster("//dapadfs/workspace_cluster_8/AEPS/HONDURAS/Clasificacion_no_supervisada/data/worldclim_cholu_copan/bio_10.tif")
levelsWNA=which(!is.na(rasterRef[]))

### First Classification: k - means

########## --> Normalize -->  Dimension reduction (PCA) --> Cluster


### Calculate pca 
dataPCANorm=FactoMineR::PCA(normMatrix,ncp = 13)
dataPCANorm$eig

### find cluster number by elbow method

k.max <- 40
data <- as.matrix(dataPCANorm$ind$coord)
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 100)$tot.withinss})
hclust(data)

dir.create("data/pca_kmeans/")

jpeg('results/pca_kmeans/SumerrorSquares_kmeans_PCA_RedcVariables_Norm_dim13.jpg')
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
dev.off()

#### k means Classification

dir.create("results/pca_kmeans_cholu_copan//")
NumCluster=3
for(NumCluster in 4:28){
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
  print(plot(rasterRef))
  
  writeRaster(rasterRef,paste0('results/pca_kmeans_cholu_copan/Map_kmeans_PCA_RedcVariables_Norm_dim13_NClu',NumCluster,'.tif'), format="GTiff",overwrite=T)
  jpeg(paste0('results/pca_kmeans_cholu_copan/Map_kmeans_PCA_RedcVariables_Norm_dim13_NClu',NumCluster,'.jpg'), width = 1200, height = 800)
  plot(rasterRef,col=colors)
  dev.off()
  
}
