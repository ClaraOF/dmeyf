#Este LightGBM fue construido  para destronar a quienes desde el inicio utilizaron XGBoost y  LightGBM
#mientras sus compañeros luchaban por correr un rpart

#Con los pibes NO

#limpio la memoria
rm( list=ls() )
gc()

require("data.table")
require("lightgbm")

<<<<<<< HEAD
setwd("C:\\Users\\Administrator\\Documents\\Maestria\\DM_EyF" )  #establezco la carpeta donde voy a trabajar

#cargo el dataset
dataset  <- fread("./datasetsOri/paquete_premium_202009.csv")
#dataset  <- fread("./datasets/paquete_premium_202009_most_important_features_ratio_new.csv")
=======
setwd("~/buckets/b1/crudoB" )  #establezco la carpeta donde voy a trabajar

#cargo el dataset
dataset  <- fread("./datasetsOri/paquete_premium_202009.csv")
>>>>>>> 7842f17395fe4d4fb2818dcff65fd523fa39b806

#creo la clase_binaria donde en la misma bolsa estan los BAJA+1 y BAJA+2
dataset[ , clase01:= ifelse( clase_ternaria=="CONTINUA", 0, 1 ) ]


#Quito el Data Drifting de  "ccajas_transacciones"
campos_buenos  <- setdiff( colnames(dataset),
                           c("clase_ternaria", "clase01","ccajas_transacciones" ) )
<<<<<<< HEAD
                           #c("clase_ternaria", "clase01","ccajas_transacciones","internet","tpaquete1", "mcaja_ahorro_dolares", "mcajeros_propios_descuento","mtarjeta_visa_descuentos","ctarjeta_master_descuentos","cmobile_app_trx", "Master_madelantodolares"))
=======
>>>>>>> 7842f17395fe4d4fb2818dcff65fd523fa39b806

#genero el formato requerido por LightGBM
dtrain  <- lgb.Dataset( data=  data.matrix(  dataset[ , campos_buenos, with=FALSE]),
                        label= dataset[ , clase01]
                      )


modelo  <- lightgbm( data= dtrain,
                      params= list( objective= "binary",
                                    max_bin= 5,   # https://www.youtube.com/watch?v=0mtctl8ba4g
                                    min_data_in_leaf= 100,
                                    num_leaves= 20,
                                    learning_rate= 0.013,
                                    num_iterations = 430,
                                    seed= 999983
                                   )  )

<<<<<<< HEAD
ganancia  <- unlist(modelo$record_evals$valid$ganancia$eval)[ modelo$best_iter ]
print(ganancia)
kfolds  <- 5
ganancia_normalizada  <- ganancia * kfolds
print(ganancia_normalizada)
#cargo el dataset donde aplico el modelo
dapply  <- fread("./datasetsOri/paquete_premium_202011.csv")
#dapply  <- fread("./datasets/paquete_premium_202011_most_important_features_ratio_new.csv")
=======

#cargo el dataset donde aplico el modelo
dapply  <- fread("./datasetsOri/paquete_premium_202011.csv")
>>>>>>> 7842f17395fe4d4fb2818dcff65fd523fa39b806

#aplico el modelo a los datos nuevos, dapply
prediccion  <- predict( modelo,  data.matrix( dapply[  , campos_buenos, with=FALSE]))

#la probabilidad de corte ya no es 0.025
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted"= as.numeric(prediccion > 0.038) ) ) #genero la salida

#genero el archivo para Kaggle
fwrite( entrega, 
<<<<<<< HEAD
        file= "./kaggle/con_los_economistas_NO_4.csv",
        sep=  "," )
=======
        file= "./kaggle/con_los_economistas_NO.csv",
        sep=  "," )
>>>>>>> 7842f17395fe4d4fb2818dcff65fd523fa39b806
