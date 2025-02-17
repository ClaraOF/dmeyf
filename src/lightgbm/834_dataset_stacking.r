#Necesita para correr en Google Cloud
#64 GB de memoria RAM
#256 GB de espacio en el disco local
#4 vCPU


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

setwd("C:\\Users\\Administrator\\Documents\\Maestria\\DM_EyF")

version  <- "v111"  #cambiar cada vez, asi se tiene versionado del dataset

dataset  <- fread( "./Cosas_Cloud/datasets_dataset_epic_v952.csv.gz" )
#dataset  <- copy(  dataset[  , c("numero_de_cliente","foto_mes","clase_ternaria"),  with=FALSE] )
gc()


#leo TODOS los archivos que estan en la carpeta  modelitos
#y hago el join con  dataset  <numero_de_cliente, foto_mes, clase_ternaria>

archivos  <- list.files( pattern="modelitos.csv.gz", path="./Cosas_Cloud/Modelitos Cloud/" )
for( archivo  in archivos )
{
  darchivo  <- fread( paste0("./Cosas_Cloud/Modelitos Cloud/", archivo ) )
  dataset  <- merge( dataset, darchivo, by=c("numero_de_cliente","foto_mes") )
}

gc()

fwrite( dataset,
        file=paste0( "./datasets/dataset_stacking_", version, ".csv.gz"),
        sep="," )

