#List of cluster of choluteca


files_tsn_gng <- c(2,3,4,6,7,9,10,11,12,13,14)
files_pca_mcluster <- c()
files_pca_kmeans <- c()


#variable for read

values_TX <- c("GRIDCODE", "TMax_January", "TMax_February", "TMax_March", "TMax_April", "TMax_May", "TMax_June", "TMax_July", "TMax_Agust", "TMax_Sept", "TMax_Oct", "TMax_Nov", "TMax_Dic")
values_TM <- c("GRIDCODE","TMin_January", "TMin_February", "TMin_March", "TMin_April", "TMin_May", "TMin_June", "TMin_July", "TMin_Agust", "TMin_Sept", "TMin_Oct", "TMin_Nov", "TMin_Dic")
values_temp <- c("GRIDCODE", "TMean_January", "TMean_February", "TMean_March", "TMean_April", "TMean_May", "TMean_June", "TMean_July", "TMean_Agust", "TMean_Sept", "TMean_Oct", "TMean_Nov", "TMean_Dic")
values_preci <- c("GRIDCODE", "PMean_January", "PMean_February", "PMean_March", "PMean_April", "PMean_May", "PMean_June", "PMean_July", "PMean_Agust", "PMean_Sept", "PMean_Oct", "PMean_Nov", "PMean_Dic")



#Read files
#Arguments - method
#            tsne_gng
#            pca_mcluster
#            pca_kmeans
#Weather variables    -Weather   1
#                     -Soil      2
#                     -Elevation 3
#variable to graph
#values_Tx  1
#values_TM  2
#values_temp 3
#values_predci 4
                    
#Setwd must be path of directory of main code


#Number of cluster

plot <- function(method, method_variables)
{

  if(method=="pca_kmeans")
  {
      if(method_variables == 2)
      {
        tabla <- read.csv(paste0(getwd(),"/Excel_Files/AllVariables_PCA_Kmeans_Soil.csv"))
        
        tabla_TX <- tabla[, values_TX] 
            
      }
    
      if(method_variables == 1)
      {
        tabla <- read.csv(paste0(getwd(),"/Excel_Files/AllVariables_PCA_Kmeans_Weather.csv"))
        
        tabla_TX <- tabla[, values_TX]
        
        #Choose cluster
        tabla_TX  <- tabla_TX[which(tabla_TX$GRIDCODE==files_tsn_gng),]
        #tabla_TX <- as.data.frame(tabla_TX)
        
        #data_TX <-data.frame(Months=seq(1:12), Cluster_2 = as.numeric(tabla_TX[2,]) , Cluster_3=as.numeric(tabla_TX[3,]), Cluster_4=as.numeric(tabla_TX[4,]), Cluster_6=as.numeric(tabla_TX[6,]), 
        #                     Cluster_7=as.numeric(tabla_TX[7,]), Cluster_9=as.numeric(tabla_TX[9,]), Cluster_10=as.numeric(tabla_TX[10,]), Cluster_11=as.numeric(tabla_TX[11,]) ,Cluster_12=as.numeric(tabla_TX[12,]), Cluster_13=as.numeric(tabla_TX[13,]), Cluster_14=as.numeric(tabla_TX[14,])) 
        
        #ggplot(tabla_TX, aes(x=Months, y=values_TX))
        
      }
  
    
      if(method_variables == 3)
      {
        tabla <- read.csv(paste0(getwd(),"/Excel_Files/AllVariables_PCA_Kmeans_Elevation.csv"))
      }

  }
  
  if(method=="pca_mcluster")
  {
    if(method_variables == 2)
    {
      tabla <- read.csv(paste0(getwd(),"/Excel_Files/AllVariables_PCA_Mclust_Soil.csv"))
    }
    
    if(method_variables == 1)
    {
      tabla <- read.csv(paste0(getwd(),"/Excel_Files/AllVariables_PCA_Mclust_Weather.csv"))
    }
    
    
    if(method_variables == 3)
    {
      tabla <- read.csv(paste0(getwd(),"/Excel_Files/AllVariables_PCA_Mclust_Elevation.csv"))
    }
    
  }
  
  
  if(method=="tsne_gng")
  {
    if(method_variables == 2)
    {
      tabla <- read.csv(paste0(getwd(),"/Excel_Files/AllVariables_tnse_GNG_Soil.csv"))
    }
    
    if(method_variables == 1)
    {
      tabla <- read.csv(paste0(getwd(),"/Excel_Files/AllVariables_tnse_GNG_Weather.csv"))
    }
    
    
    if(method_variables == 3)
    {
      tabla <- read.csv(paste0(getwd(),"/Excel_Files/AllVariables_tnse_GNG_Elevation.csv"))
    }
    
  }
  
  
  
  return(tabla_TX  )
  
}