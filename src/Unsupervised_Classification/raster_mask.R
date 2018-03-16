#############
####
####   Create an unsupervised Classifcation Honduras' west zone
####
####

rm(list=ls())
### Functions
MaskRasterData=function(stackfile, vectorfile){
  
  getrastercrsprojection = crs(stackfile[[1]])
  getvectorcrsprojection = crs(vectorfile)
  
  ##Compare both system reference
  if(!as.character(getrastercrsprojection)== as.character(getvectorcrsprojection)){
    # change projection
    new.CRS=getrastercrsprojection
    vectorfile = spTransform(vectorfile, new.CRS)
  }
  
  ## crop raster
  
  datacropped=crop(stackfile,vectorfile)
  
  ## mask layer
  
  datamasked=mask(datacropped,vectorfile)
  
  return(datamasked)
  
  
}

### Read Packages
library(raster)
library(rgdal)
library(stringr)
### Work directories

dirfol_principal = "//dapadfs/workspace_cluster_8/AEPS/HONDURAS/Clasificacion_no_supervisada"
worldclim2 = "//dapadfs/data_cluster_4/observed/gridded_products/worldclim/Global_30s_v2"
VectorFile = "//dapadfs/workspace_cluster_8/AEPS/HONDURAS/SHP_File"
ElevationFile = "//dapadfs/workspace_cluster_8/AEPS/HONDURAS/Elevacion/"
SoilsFiles = "//dapadfs/workspace_cluster_8/AEPS/HONDURAS/Suelo/"

######## Read shapeFile
setwd(VectorFile)
DataShape = readOGR("Choluteca_Copan_WGS84.shp")
plot(DataShape)


### crop raster layer

#######

### read worldclim data

listofvariables=c("bio","tmin","srad","tmax","prec")

namesfiles = list.files(path = worldclim2, pattern = "tif$")

# pick files that match with the variable names

posfiles=unlist(lapply(listofvariables, function(x) which(grepl(namesfiles,pattern = x))))

namesfiles=namesfiles[posfiles]

# stack raster layers
dir.create(paste0(dirfol_principal,"/data/worldclim_cholu_copan/"))
for(i in 1: length(namesfiles)){
  worldclimraster=raster(paste0(worldclim2,"/", namesfiles[i]))
  worldClim2masked=MaskRasterData(worldclimraster,DataShape)
  writeRaster(worldClim2masked,filename = paste0(dirfol_principal,"/data/worldclim_cholu_copan/",namesfiles[i]))
}



### read soil data

elevation_data=raster("//DAPADFS/workspace_cluster_8/AEPS/HONDURAS/Elevacion/srtm30mhnd_mas.TIF")

elevationmasked=MaskRasterData(elevation_data,DataShape)
plot(elevationmasked)
writeRaster(elevationmasked,filename = paste0(dirfol_principal,"/data/dem/Elevation_choluteca_copan.tif"),overwrite=T)


plot(raster("//dapadfs/workspace_cluster_8/AEPS/HONDURAS/Clasificacion_no_supervisada/data/soil_cholu_copan/silt_cholu_copan.tif"))
