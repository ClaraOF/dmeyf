#Optimizacion Bayesiana de hiperparametros de  ranger
#funciona automaticamente con EXPERIMENTOS
#va generando incrementalmente salidas para kaggle

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rlist")
require("yaml")

require("ranger")
require("randomForest")  #solo se usa para imputar nulos
require("parallel")

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")


#defino la carpeta donde trabajo
setwd( "C:\\Users\\Administrator\\Documents\\Maestria\\DM_EyF" )


kexperimento  <- NA   #NA si se corre la primera vez, un valor concreto si es para continuar procesando

kscript           <- "560_ranger_BO_Clara"
karch_generacion  <- "./datasetsOri/paquete_premium_202009.csv"
karch_aplicacion  <- "./datasetsOri/paquete_premium_202011.csv"
kBO_iter    <-  150   #cantidad de iteraciones de la Optimizacion Bayesiana

param  <- list( "num.trees"=      500,  #cantidad de arboles
                "mtry"=             sqrt(ncol(dtrain)),  #cantidad de variables que evalua para hacer un split
                "min.node.size"=    1,  #hoja mas chica
                "max.depth"=        0   # 0 significa profundidad infinita
              )

hs  <- makeParamSet(
          makeIntegerParam("num.trees" ,        lower=  2L  , upper=  200L),  #la letra L al final significa ENTERO
          makeIntegerParam("max.depth",         lower=  0L  , upper=   20L),  # 0 significa profundidad infinita
          makeIntegerParam("min.node.size" ,    lower=  1L  , upper=  200L),
          makeIntegerParam("mtry" ,             lower=  2L  , upper=   20L))

ksemilla_azar  <- 999979  #Aqui poner la propia semilla
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
#funcion para particionar, es la que Andres reemplaza con caret

particionar  <- function( data, division, agrupa="", campo="fold", start=1, seed=NA )
{
  if( !is.na( seed)  )   set.seed( seed )

  bloque  <- unlist( mapply(  function(x,y) { rep( y, x ) }, division, seq( from=start, length.out=length(division) )  ) )

  data[ , (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],
           by= agrupa ]
}
#------------------------------------------------------------------------------

ranger_Simple  <- function( fold_test, pdata, param )
{
  #genero el modelo

  set.seed(ksemilla_azar)

  modelo  <- ranger( formula= "clase_binaria ~ . -internet -tpaquete1 -mcaja_ahorro_dolares -mcajeros_propios_descuentos -mtarjeta_visa_descuentos -ctarjeta_master_descuentos -cmobile_app_trx -Master_madelantodolares", 
                     data=  pdata[ fold!= fold_test], 
                     probability=   TRUE,  #para que devuelva las probabilidades
                     num.trees=     param$num.trees,
                     mtry=          param$mtry,
                     min.node.size= param$min.node.size,
                     max.depth=     param$max.depth
                 )

  prediccion  <- predict( modelo, pdata[ fold==fold_test] )

  ganancia_testing  <- pdata[ fold==fold_test,
                              sum( (prediccion$predictions[ ,"POS" ] > 0.025) *
                                    ifelse( clase_binaria=="POS", 48750, -1250)  ) ]

  return( ganancia_testing )
}
#------------------------------------------------------------------------------

ranger_CrossValidation  <- function( data, param, pcampos_buenos, qfolds, pagrupa, semilla )
{
  divi  <- rep( 1, qfolds )
  particionar( data, divi, seed=semilla, agrupa=pagrupa )

  ganancias  <- mcmapply( ranger_Simple, 
                          seq(qfolds), # 1 2 3 4 5  
                          MoreArgs= list( data, param), 
                          SIMPLIFY= FALSE,
                          mc.cores= 1 )   #dejar esto en  1, porque ranger ya corre en paralelo

  data[ , fold := NULL ]
  #devuelvo la primer ganancia y el promedio
  return( mean( unlist( ganancias )) *  qfolds )   #aqui normalizo la ganancia
}
#------------------------------------------------------------------------------
#esta funcion solo puede recibir los parametros que se estan optimizando
#el resto de los parametros se pasan como variables globales

EstimarGanancia_ranger  <- function( x )
{

   xval_folds  <- 5
   ganancia  <- ranger_CrossValidation( dataset, 
                                        param= x,
                                        qfolds= xval_folds, 
                                        pagrupa= "clase_binaria", 
                                        semilla= ksemilla_azar )

   #genero el archivo para Kaggle

     set.seed(ksemilla_azar)

     modelo  <- ranger( formula= "clase_binaria ~ . -internet -tpaquete1 -mcaja_ahorro_dolares -mcajeros_propios_descuentos -mtarjeta_visa_descuentos -ctarjeta_master_descuentos -cmobile_app_trx -Master_madelantodolares", 
                        data=  dataset, 
                        probability=   TRUE,  #para que devuelva las probabilidades
                        num.trees=     x$num.trees,
                        mtry=          x$mtry,
                        min.node.size= x$min.node.size,
                        max.depth=     x$max.depth
                      )

     prediccion  <- predict( modelo, dapply )

     Predicted  <- as.integer( prediccion$predictions[ ,"POS" ] > 0.025 )

     entrega  <- as.data.table( list( "numero_de_cliente"=dapply$numero_de_cliente, 
                                      "Predicted"= Predicted)  )

     #genero el archivo para Kaggle
     fwrite( entrega, 
             file= paste0(kkaggle, ".csv" ),
             sep=  "," )
   print(ganancia)

   return( ganancia )
}
#------------------------------------------------------------------------------
#Aqui empieza el programa


#cargo el datset donde voy a entrenar
dataset  <- fread(karch_generacion, stringsAsFactors= TRUE)   #donde entreno

dataset[ , clase_binaria := as.factor(ifelse( clase_ternaria=="BAJA+2", "POS", "NEG" )) ]
dataset[ , clase_ternaria := NULL ]  #elimino la clase_ternaria, ya no la necesito
#imputo los nulos, ya que ranger no acepta nulos
#Leo Breiman, ¿por que le temias a los nulos?
dataset  <- na.roughfix( dataset )


#cargo el dataset donde voy a aplicar el modelo, que NO tiene clase
dapply   <- fread(karch_aplicacion, stringsAsFactors= TRUE)   #donde aplico el modelo
dapply[ , clase_ternaria := NULL ]  #Elimino esta columna que esta toda en NA
dapply  <- na.roughfix( dapply )



#Aqui comienza la configuracion de la Bayesian Optimization

configureMlr( show.learner.output = FALSE)

funcion_optimizar  <- EstimarGanancia_ranger

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
#por favor, no desesperarse por lo complejo
obj.fun  <- makeSingleObjectiveFunction(
              fn=       funcion_optimizar,
              minimize= FALSE,   #estoy Maximizando la ganancia
              noisy=    TRUE,
              par.set=  hs,
              has.simple.signature = FALSE
             )

ctrl  <- makeMBOControl( save.on.disk.at.time= 600,  save.file.path= kbayesiana)
ctrl  <- setMBOControlTermination(ctrl, iters= kBO_iter )
ctrl  <- setMBOControlInfill(ctrl, crit= makeMBOInfillCritEI())

surr.km  <-  makeLearner("regr.km", predict.type= "se", covtype= "matern3_2", control= list(trace= TRUE))

#inicio la optimizacion bayesiana
if(!file.exists(kbayesiana)) {
  run  <- mbo(obj.fun, learner = surr.km, control = ctrl)
} else  run  <- mboContinue( kbayesiana )   #retomo en caso que ya exista


quit( save="no" )


