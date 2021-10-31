#Este LightGBM fue construido  para destronar a quienes desde el inicio utilizaron XGBoost y  LightGBM
#mientras sus compañeros luchaban por correr un rpart

#Con los pibes NO

#limpio la memoria
rm( list=ls() )
gc()

require("data.table")
require("lightgbm")

setwd("C:\\Users\\Administrator\\Documents\\Maestria\\DM_EyF" )  #establezco la carpeta donde voy a trabajar

#cargo el dataset
#dataset  <- fread("./datasetsOri/paquete_premium_202009.csv")
#dataset  <- fread("./datasets/paquete_premium_202009_most_important_features_ratio_new.csv")

#creo la clase_binaria donde en la misma bolsa estan los BAJA+1 y BAJA+2
#dataset[ , clase01:= ifelse( clase_ternaria=="CONTINUA", 0, 1 ) ]


#Quito el Data Drifting de  "ccajas_transacciones"
#campos_buenos  <- setdiff( colnames(dataset),
#                           c("clase_ternaria", "clase01","ccajas_transacciones" ) )
                           #c("clase_ternaria", "clase01","ccajas_transacciones","internet","tpaquete1", "mcaja_ahorro_dolares", "mcajeros_propios_descuento","mtarjeta_visa_descuentos","ctarjeta_master_descuentos","cmobile_app_trx", "Master_madelantodolares"))

#genero el formato requerido por LightGBM
#dtrain  <- lgb.Dataset( data=  data.matrix(  dataset[ , campos_buenos, with=FALSE]),
#                        label= dataset[ , clase01]
#                      )
experimento  <- 112   #NA si se corre la primera vez, un valor concreto si es para continuar procesando
kscript         <- "777_mutante"

karch_dataset    <- "./datasets/dataset_epic_simple_v007.csv.gz"   #este dataset se genero en el script 812_dataset_epic.r

kapply_mes       <- c(202011)  #El mes donde debo aplicar el modelo

ktrain_subsampling  <- 1.0   #el undersampling que voy a hacer de los continua

ktrain_mes_hasta    <- 202009  #Obviamente, solo puedo entrenar hasta 202011
ktrain_mes_desde    <- 202009

ktrain_meses_malos  <- c()  #meses que quiero excluir del entrenamiento

kgen_mes_hasta    <- 202009  #Obviamente, solo puedo entrenar hasta 202011
kgen_mes_desde    <- 202009
campos_malos  <- c("clase_ternaria", "clase01","ccajas_transacciones" )

ksemilla_azar  <- 999979  #Aqui poner la propia semilla

#------------------------------------------------------------------------------

particionar  <- function( data,  division, agrupa="",  campo="fold", start=1, seed=NA )
{
  if( !is.na(seed) )   set.seed( seed )

  bloque  <- unlist( mapply(  function(x,y) { rep( y, x )} ,   division,  seq( from=start, length.out=length(division) )  ) )  

  data[ ,  (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],
            by= agrupa ]
}
#-------------------------------------------------------------------------------
#------------------------------------------------------------------------------

HemiModelos  <- function( hparam )
{

  #Construyo el modelo sobre el fold=1
  dgeneracion1  <- lgb.Dataset( data=    data.matrix(  dataset[ generacion_final==1 & fold==1, campos_buenos, with=FALSE]),
                                label=   dataset[ generacion_final==1 & fold==1, clase01] )

  modelo_final1  <- lightgbm( data= dgeneracion1,
                              params= list( objective= "binary",
                                    max_bin= 5,   # https://www.youtube.com/watch?v=0mtctl8ba4g
                                    min_data_in_leaf= 100,
                                    num_leaves= 20,
                                    learning_rate= 0.013,
                                    num_iterations = 430,
                                    seed= 999983
                                   ),
                              verbose= -100 )

  rm( dgeneracion1 )  #borro y libero memoria
  gc()

  #Aplico el modelo al fold=2
  prediccion  <- predict( modelo_final1, data.matrix( dataset[ generacion_final==1 & fold==2, campos_buenos, with=FALSE]) )
  dataset[ generacion_final==1 & fold==2, prob := prediccion ]

  tb_modelitos[ dataset[ generacion_final==1 & fold==2], 
                on=c("numero_de_cliente","foto_mes"),  
                paste0( "E", experimento,"_", GLOBAL_iteracion ) := i.prob  ]


  #AHORA SOBRE LA OTRA MITAD -----------------

  #Construyo el modelo sobre el fold=2
  dgeneracion2  <- lgb.Dataset( data=    data.matrix(  dataset[ generacion_final==1 & fold==2, campos_buenos, with=FALSE]),
                                label=   dataset[ generacion_final==1 & fold==2, clase01]
                              )

  modelo_final2  <- lightgbm( data= dgeneracion2,
							  params= list( objective= "binary",
                                    max_bin= 5,   # https://www.youtube.com/watch?v=0mtctl8ba4g
                                    min_data_in_leaf= 100,
                                    num_leaves= 20,
                                    learning_rate= 0.013,
                                    num_iterations = 430,
                                    seed= 999983
                                   ),
                              verbose= -100
                            )

  rm( dgeneracion2 )  #borro y libero memoria
  gc()

  #Aplico el modelo al fold=1
  prediccion  <- predict( modelo_final2, data.matrix( dataset[ generacion_final==1 & fold==1, campos_buenos, with=FALSE]) )
  dataset[ generacion_final==1 & fold==1, prob := prediccion ]

  tb_modelitos[ dataset[ generacion_final==1 & fold==1], 
                on= c("numero_de_cliente","foto_mes"),  
                paste0( "E", experimento,"_", GLOBAL_iteracion ) := i.prob  ]

  dataset[  , prob := NULL ]

}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

FullModelo  <- function( hparam )
{
  #entreno sin undersampling
  dgeneracion  <- lgb.Dataset( data=    data.matrix(  dataset[ generacion_final==1 , campos_buenos, with=FALSE]),
                               label=   dataset[ generacion_final==1, clase01]
                             )

  modelo_final  <- lightgbm( data= dgeneracion,
                             params= list( objective= "binary",
                                    max_bin= 5,   # https://www.youtube.com/watch?v=0mtctl8ba4g
                                    min_data_in_leaf= 100,
                                    num_leaves= 20,
                                    learning_rate= 0.013,
                                    num_iterations = 430,
                                    seed= 999983
                                   ),
                             verbose= -100
                           )

  rm( dgeneracion )  #borro y libero memoria
  gc()

  #calculo la importancia de variables
  tb_importancia  <- lgb.importance( model= modelo_final )
  fwrite( tb_importancia, 
          file= paste0( kimp, "imp_", sprintf("%03d", GLOBAL_iteracion), ".txt"),
          sep="\t" )

  #Aplico sobre todo el dataset
  prediccion  <- predict( modelo_final, data.matrix( dataset[  , campos_buenos, with=FALSE]) )
  dataset[ , prob := prediccion ]
  tb_modelitos[ dataset, 
                on=c("numero_de_cliente","foto_mes"),  
                paste0( "E", experimento,"_", GLOBAL_iteracion ) := i.prob  ]

 #Fin primera pasada modelitos

  prediccion  <- predict( modelo_final, data.matrix( dapply[  , campos_buenos, with=FALSE]) )

  predsort  <- sort(prediccion, decreasing=TRUE)
  pos_corte  <- as.integer(hparam$ratio_corte*nrow(dapply))
  prob_corte <- predsort[ pos_corte ]
  Predicted  <- as.integer( prediccion > prob_corte )

  entrega  <- as.data.table( list( "numero_de_cliente"= dapply$numero_de_cliente, 
                                   "Predicted"= Predicted)  )

  #genero el archivo para Kaggle
  fwrite( entrega, 
          file= paste0(kkaggle, sprintf("%03d", GLOBAL_iteracion), ".csv" ),
          sep= "," )

  base  <- round( pos_corte / 500 ) * 500   - 3000
  evaluados  <- c( seq(from=base, to=pmax(base+6000,15000), by=500 ) , pos_corte )  
  evaluados  <- sort( evaluados )

  for(  pos  in  evaluados )
  {
    prob_corte  <-  predsort[ pos ]
    Predicted  <- as.integer( prediccion > prob_corte )

    entrega  <- as.data.table( list( "numero_de_cliente"= dapply$numero_de_cliente, 
                                     "Predicted"= Predicted)  )

    #genero el archivo para Kaggle
    fwrite( entrega, 
            file= paste0(kkagglemeseta, sprintf("%03d", GLOBAL_iteracion), 
                         "_",  sprintf( "%05d", pos) ,".csv" ),
            sep= "," )
  }

  rm( entrega, Predicted )
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

VPOS_CORTE  <- c()

fganancia_lgbm_meseta  <- function(probs, datos) 
{
  vlabels  <- getinfo(datos, "label")
  vpesos   <- getinfo(datos, "weight")

  #solo sumo 48750 si vpesos > 1, hackeo 
  tbl  <- as.data.table( list( "prob"= probs, 
                               "gan"=  ifelse( vlabels==1 & vpesos <= 1, 48750, -1250 ) *vpesos,
                               "peso"=  vpesos
                               ) )

  setorder( tbl, -prob )
  tbl[ , gan_acum :=  cumsum( gan ) ]
  tbl[ , posicion :=  cumsum( peso ) ]
  setorder( tbl, -gan_acum )   #voy por la meseta

  gan  <- mean( tbl[ 1:10,  gan_acum] )  #meseta de tamaño 10

  pos_meseta  <- tbl[ 1:10,  median(posicion)]
  VPOS_CORTE  <<- c( VPOS_CORTE, pos_meseta )

  return( list( "name"= "ganancia", 
                "value"=  gan,
                "higher_better"= TRUE ) )
}
#------------------------------------------------------------------------------

x  <- list( "learning_rate"= 0.02, 
            "feature_fraction"= 0.50,
            "min_data_in_leaf"= 4000,
            "num_leaves"= 600 )


EstimarGanancia_lightgbm  <- function( x )
{
  GLOBAL_iteracion  <<- GLOBAL_iteracion + 1

  gc()
  param_basicos  <- list( objective= "binary",
                          metric= "custom",
                          first_metric_only= TRUE,
                          boost_from_average= TRUE,
                          feature_pre_filter= FALSE,
                          verbosity= -100,
                          seed= 999983,
                          max_bin= 5,
						              min_data_in_leaf= 100,
            						  learning_rate= 0.013,
                          num_iterations= 430,   #un numero muy grande, lo limita early_stopping_rounds
                          force_row_wise= TRUE    #para que los alumnos no se atemoricen con tantos warning
  )

  #el parametro discolo, que depende de otro
  param_variable  <- list(  early_stopping_rounds= as.integer(50 + 1/x$learning_rate) )

  param_completo  <- c( param_basicos, param_variable, x )

  VPOS_CORTE  <<- c()
  kfolds  <- 5
  set.seed( 999983 )
  modelocv  <- lgb.cv( data= dtrain,
                       eval= fganancia_lgbm_meseta,
                       stratified= TRUE, #sobre el cross validation
                       nfold= kfolds,    #folds del cross validation
                       param= param_completo,
                       verbose= -100
                      )


  ganancia  <- unlist(modelocv$record_evals$valid$ganancia$eval)[ modelocv$best_iter ]
  pos_corte  <-  sum( VPOS_CORTE[  (kfolds*modelocv$best_iter+1):( kfolds*modelocv$best_iter + kfolds ) ] )

  #unlist(modelo$record_evals$valid$ganancia$eval)[ modelo$best_iter ]

  #calculo la ganancia sobre los datos de testing
  ganancia_normalizada  <- ganancia * kfolds 

  attr(ganancia_normalizada,"extras" )  <- list("num_iterations"= modelocv$best_iter)  #esta es la forma de devolver un parametro extra

  param_final  <- copy( param_completo )
  param_final["early_stopping_rounds"]  <- NULL
  param_final$num_iterations <- modelocv$best_iter  #asigno el mejor num_iterations
  param_final$ratio_corte  <- pos_corte /  sum(getinfo(dtrain, "weight"))


  #si tengo una ganancia superadora, genero el archivo para Kaggle
  if( ganancia_normalizada > GLOBAL_ganancia_max)
  {
    GLOBAL_ganancia_max  <<- ganancia_normalizada  #asigno la nueva maxima ganancia a una variable GLOBAL, por eso el <<-

    FullModelo( param_final )
    HemiModelos( param_final )
    fwrite( tb_modelitos, file= kmodelitos, sep= "," )
  }

   #logueo 
   xx  <- param_final
   xx$iteracion_bayesiana  <- GLOBAL_iteracion
   xx$ganancia  <- ganancia_normalizada  #le agrego la ganancia
   loguear( xx,  arch= klog )

   return( ganancia_normalizada )
}
#------------------------------------------------------------------------------
#Aqui empieza el programa
#en estos archivos quedan los resultados
dir.create( paste0( "./work/E",  experimento, "/" ) )     #creo carpeta del experimento dentro de work
dir.create( paste0( "./kaggle/E",  experimento, "/" ) )   #creo carpeta del experimento dentro de kaggle
dir.create( paste0( "./kaggle/E",  experimento, "/meseta/" ) )   #creo carpeta del experimento dentro de kaggle

kbayesiana    <- paste0("./work/E",  experimento, "/E",  experimento, "_", kscript, ".RDATA" )
klog          <- paste0("./work/E",  experimento, "/E",  experimento, "_", kscript, "_BOlog.txt" )
kimp          <- paste0("./work/E",  experimento, "/E",  experimento, "_", kscript, "_" )
kkaggle       <- paste0("./kaggle/E",experimento, "/E",  experimento, "_", kscript, "_" )
kkagglemeseta <- paste0("./kaggle/E",experimento, "/meseta/E",  experimento, "_", kscript, "_" )
kmodelitos    <- paste0("./modelitos/E", experimento, "_modelitos.csv.gz" )

GLOBAL_iteracion  <- 0
GLOBAL_ganancia_max  <- -Inf

#cargo el dataset que tiene los 36 meses
dataset  <- fread(karch_dataset)
tb_modelitos  <- dataset[  ,  c("numero_de_cliente","foto_mes"), with=FALSE ]
fwrite( tb_modelitos, file= kmodelitos, sep= "," )

#cargo los datos donde voy a aplicar el modelo
dapply  <- copy( dataset[  foto_mes %in% kapply_mes ] )
#creo la clase_binaria2   1={ BAJA+2,BAJA+1}  0={CONTINUA}
dataset[ , clase01:= ifelse( clase_ternaria=="CONTINUA", 0, 1 ) ]
particionar( dataset, division= c(1,1), agrupa= c("foto_mes","clase_ternaria" ), seed= ksemilla_azar )

dataset[    foto_mes>= kgen_mes_desde  &
            foto_mes<= kgen_mes_hasta & 
            !( foto_mes %in% ktrain_meses_malos ),
          generacion_final:= 1L ]  #donde entreno
#Defino los datos donde entreno, con subsampling de los CONTINUA
vector_azar  <- runif( nrow(dataset) )
dataset[    foto_mes>= ktrain_mes_desde  &
            foto_mes<= ktrain_mes_hasta & 
            !( foto_mes %in% ktrain_meses_malos ) & 
            ( clase01==1 | vector_azar < ktrain_subsampling ),  
          entrenamiento:= 1L ]  #donde entreno


#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), 
                           c("clase_ternaria","clase01", "generacion_final", "entrenamiento", "fold", campos_malos) )

#dejo los datos en el formato que necesita LightGBM
#uso el weight como un truco ESPANTOSO para saber la clase real
dtrain  <- lgb.Dataset( data=    data.matrix(  dataset[ entrenamiento==1 , campos_buenos, with=FALSE]),
                        label=   dataset[ entrenamiento==1, clase01],
                        weight=  dataset[ entrenamiento==1, ifelse(clase_ternaria=="CONTINUA", 1/ktrain_subsampling,
                                                                   ifelse( clase_ternaria=="BAJA+2", 1, 1.0000001))] ,
                        free_raw_data= TRUE
                      )
#Aqui comienza la configuracion de la Bayesian Optimization

funcion_optimizar  <- EstimarGanancia_lightgbm   #la funcion que voy a maximizar


