#Read the name file 

#The split_name function works split name file
#-Argument: name file
#-Return: split of name file 




split_name <- function(filename)
{
  split_name <- unlist(strsplit(filename, split='_', fixed=TRUE))
  split_name <- gsub("txt","",gsub("[[:punct:]]","",split_name) )
  
  return(split_name)
}


#join_files works merging  files pdf into only one.
#-Argument: name_method  name of method of clustering
#         : num_cluter       number of cluster

#the workspace must be where are the others folders

join_files <- function(name_method, num_cluter)
{
  
  #Read files Curve Tx and TM
  graph_all_station_Curve_TX_TM(name_method)

  
  #Read files Orbothermic
  graph_all_station (name_method)
  
  #Read Soil texture  
  graph_all_texture_clus(name_method)
  
  #Elevation
  graph_all_elevation_complete(name_method)
  
  #Excel texture 
  soil_texture(name_method) 
  
  #Create folder fo each folder     
  for (i in 1:num_cluter)
  {
    mainDir <- paste0(getwd(), "/Data_Final")
    dir.create(file.path(mainDir, paste0("Cluster_", i)), showWarnings = FALSE)
    
  }
  

  for (i in 1:num_cluter)
  {
    list_files_Curve <- list.files("./Temperature_curve", pattern =paste0(i, ))
    
  }
  
  
  
  
  
}

