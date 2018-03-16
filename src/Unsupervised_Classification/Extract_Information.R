#############
####
####   Extract information used for unsupervised Classification
####
####
rm(list = ls())

library(raster)
library(ff)
library(stringr)
###########---> folder paths

dirfol_principal = "//dapadfs/workspace_cluster_8/AEPS/HONDURAS/Clasificacion_no_supervisada"
setwd(dirfol_principal)

ElevationFile="data/dem_cholu_copan/"
SoilFiles="data/soil_cholu_copan/"
ClimeFiles="data/worldclim_cholu_copan/"
terrainfiles = "data/terrain_analysis_cholu_copan/"
###########---> Read tif Layers

listFiles=list.files(path = SoilFiles,pattern = "*.tif$")
soilsInfo=stack(paste0(SoilFiles,listFiles))

listFiles=list.files(path = ClimeFiles,pattern = "*.tif$")
ClimeInfo=stack(paste0(ClimeFiles,listFiles))

listFiles=list.files(path = ElevationFile,pattern = "*.tif$")
ElevationInfo=raster(paste0(ElevationFile,listFiles))

listFiles=list.files(path = terrainfiles,pattern = "*.tif$")
terrainInfo=raster(paste0(terrainfiles,listFiles))

###########---> Homogenized coordinates system
##Information of soil.
crsSystem=ClimeInfo@crs
soilsInfo_Projected=projectRaster(soilsInfo, crs = crsSystem)
plot(soilsInfo_Projected)


###########---> Calculate slope and Aspect from DEM

ElevationInfo_Projected=projectRaster(ElevationInfo, crs = crsSystem)
SlopeAspectInfo=slopeAspect(ElevationInfo_Projected, out=c('slope', 'aspect'), unit='radians',  neighbors=8)
plot(ElevationInfo_Projected)

###### Get Coordinates 

rasterRef=ClimeInfo$bio_10
plot(rasterRef)
levelsWNA=which(!is.na(rasterRef[]))
coordinatesExtract=xyFromCell(rasterRef,levelsWNA)
plot(coordinatesExtract)

###########---> Extract information from raster files, this info will be store in a ff variable,. to optimize space

numImages=sum(nlayers(ElevationInfo),nlayers(SlopeAspectInfo),nlayers(terrainInfo),nlayers(soilsInfo_Projected),nlayers(ClimeInfo))
LayerNames=c("ElevationInfo_Projected","SlopeAspectInfo","terrainInfo","soilsInfo_Projected","ClimeInfo")

dataImages= ff(vmode="double",dim=c(length(levelsWNA),numImages))

posCol=0
for(i in 1:length(LayerNames)){
  LayerStack=LayerNames[i]
  CellValuesExtract=cellFromXY(xy = coordinatesExtract,object = eval(parse(text=LayerStack)))
  posCol=posCol+nlayers(eval(parse(text=LayerStack)))???
  levelsCol=((posCol-nlayers(eval(parse(text=LayerStack))))+1):posCol
  dataImages[,levelsCol]=eval(parse(text=LayerStack))[CellValuesExtract]
  cat(i , "\n")
}

######## -- > Save table into a dataframe format
dataExport=data.frame(as.matrix(dataImages[1:nrow(dataImages),]))
head(dataExport)
posCol=0
for(i in 1:length(LayerNames)){
  LayerStack=LayerNames[i]
  posCol=posCol+nlayers(eval(parse(text=LayerStack)))
  levelsCol=((posCol-nlayers(eval(parse(text=LayerStack))))+1):posCol
  names(dataExport)[levelsCol]=names(eval(parse(text=LayerStack)))
}
names(dataExport)

dir.create("data/rdata")
save(dataExport,file="data/rdata/AllVariables_hondruras_cholu_copan.RData")
