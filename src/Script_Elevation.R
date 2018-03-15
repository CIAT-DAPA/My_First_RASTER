################
#####
##### Mask Elevation and Soils


rm(list =ls())

libs=c("rgdal","raster","sp")
sapply(libs,require,character.only= T)


######## folder paths

OwnProject <- "D:/OneDrive - CGIAR/My_first_RASTER/My_First_RASTER"
VectorFile="//dapadfs/workspace_cluster_8/AEPS/HONDURAS/SHP_File"
ElevationFile="//dapadfs/workspace_cluster_8/AEPS/HONDURAS/Elevacion/"
SoilsFiles="//dapadfs/workspace_cluster_8/AEPS/HONDURAS/Suelo/"
outputFile="//dapadfs/workspace_cluster_8/AEPS/HONDURAS/Suelo"

######## Read shapeFile
setwd(VectorFile)

#Read the maps *.shp
DataShape=readOGR("CholutecaCopan.shp")
plot(DataShape)


### Read Elevation file. Extension *.tif
setwd(ElevationFile)
DEM=raster("srtm30mhnd_mas.tif")
plot(DEM)


#Creo que el archivo recibe el raster
new.CRS=DEM@crs
DataShape_Projec = spTransform(DataShape, new.CRS)
plot(DataShape_Projec,col="red")


#Partir la elevacion en las zonas que queremos. Chopan. 
rast_Mask=mask(crop(DEM,DataShape_Projec),DataShape_Projec)
plot(rast_Mask)


nameFile="DEM_CholutecaCopan"
writeRaster(rast_Mask, filename=paste0(nameFile,".tif"), format="GTiff", overwrite=T)


####### Read soils Files
crsnew="+proj=utm +zone=16 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

setwd(SoilsFiles)
TifFiles=list.files(pattern = "*.asc$")

StackFiles=stack(TifFiles)


####### Define Coordinates System


new.CRS="+proj=utm +zone=16 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
DataShape_Projec = spTransform(DataShape, new.CRS)
plot(DataShape_Projec,col="red")

###### Mask raster Files






##########Plots

setwd(OwnProject)
dir.create(file.path(OwnProject, "Graphics"), showWarnings = FALSE)


# Shape file. "CholutecaCopan.shp"
pdf("./Graphics/Mapa_Choluteca_Chopan.pdf")
plot(DataShape)
dev.off()


#Elevation
pdf("./Graphics/Elevation_Map.pdf")
plot(DEM)
dev.off()

#DataShape
pdf("./Graphics/Mapa_Choluteca_Chopan_Rojo.pdf")
plot(DataShape_Projec,col="red")
dev.off()

#Mask
pdf("./Graphics/rast_Mask.pdf")
plot(rast_Mask)
dev.off()


#Datashape
pdf("./Graphics/DataShape_Projec.pdf")
plot(DataShape_Projec,col="red")
dev.off()



rast=StackFiles[[2]]
for(i in 1:length(names(StackFiles))){
  rast=StackFiles[[i]]
  crs(rast)=new.CRS
  cat("\n",names(rast))
  rast_Mask=mask(crop(rast,DataShape_Projec),DataShape_Projec)
  nameFile=paste0(outputFile,"/" ,names(rast))
  writeRaster(rast_Mask, filename=paste0(nameFile,".tif"), format="GTiff", overwrite=T)
}