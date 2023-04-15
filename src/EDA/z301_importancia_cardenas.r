#Script dedicado a la paz espiritual de Lucas Cardenas
#construye un modelo aditivo (sin regresion lineal ni logistica)
#utilizando XGBoost con  Decision Stumps ( arboles de altura 1 )
#sale un modelo "lineal"   GLOBAL

#limpio la memoria
rm( list=ls() )
gc()

require("data.table")
require("Matrix")
require("xgboost")
require("DiagrammeR")

#Parametros del script
PARAM  <- list()
PARAM$experimento  <- "IMP3010"
# FIN Parametros del script

#------------------------------------------------------------------------------
#calculo la ganancia del ensemble a esta iteracion
#busco el corte optimo como una media movil de ancho 1001

fganancia_logistic_xgboost  <- function( probs, datos ) 
{
  vpesos  <- getinfo( datos, "weight")

  tbl  <<- as.data.table( list( "prob" = probs,
                                "gan"  = ifelse( vpesos > 1, 117000,  -3000 ) ) )

  setorder( tbl, -prob )
  tbl[ , gan_acum := cumsum( gan ) ]
  tbl[ , gan_acum_suave  := frollmean( x=gan_acum, n=1001, align="center", na.rm=TRUE, hasNA= TRUE ) ]

  mayor_ganancia  <- tbl[ , max( gan_acum_suave, na.rm=TRUE ) ]

  #rm( tbl )

  return( list(metric = "ganancia", 
               value =  ifelse(  is.na(mayor_ganancia) , 0, mayor_ganancia ) ) )
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

setwd("~/buckets/b1/")

dataset  <- fread( "./datasets/dataset_pequeno.csv")


#calculo la clase binaria
dataset[ , clase01 := ifelse( clase_ternaria %in% c("BAJA+2"), 1, 0) ]


#creo la carpeta donde va el experimento
dir.create( paste0( "./exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd(paste0( "./exp/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO

#agrego a mis fieles canaritos
# al overfitting no temo, pues  Ellos están conmigo
set.seed( 13 )
for( i in 1:20 )  dataset[ , paste0( "canarito", i) := runif( nrow(dataset) ) ]


col_buenas  <- setdiff(colnames(dataset) ,c("clase01", "clase_ternaria") )

dtrain  <- xgb.DMatrix( data=  as.matrix(dataset[ foto_mes==202107 , col_buenas, with=FALSE]),
                        label= dataset[ foto_mes==202107, clase01 ],
                        weight=  dataset[ foto_mes==202107,  ifelse( clase_ternaria=="BAJA+2", 1.000001, 1.0 ) ]
                      )

#Primero determino la cantidad optima de arboles, usando 5-fold cross validation
modelocv  <- xgb.cv(data= dtrain,
                    objective= "binary:logistic",
                    feval= fganancia_logistic_xgboost,
                    maximize= TRUE,
                    nfold= 5,
                    stratified= TRUE,
                    base_score= 0.025, #Prob de corte del PROBLEMA
                    nrounds= 99999,  #un numero muy grande
                    early_stopping_rounds= 100,
                    max_depth= 1,   #Fundamental
                    colsample_bytree= 0.2,
                    eta= 0.05,
                    tree_method= "hist",
                    max_bin= 255
                   )

# en modelocv$best_iter  quedo la cantidad optima de arboles del ensemble

#uso hiperparametros que supuestamente optimice en una bayesiana, pero con max_depth=1
modelo  <- xgb.train(data= dtrain,
                     objective= "binary:logistic",
                     base_score= 0.025, #intencional seteo
                     nrounds= modelocv$best_iter,
                     max_depth= 1,   #Fundamental
                     eta= 0.05,
                     colsample_bytree= 0.2,
                     tree_method= "hist",
                     max_bin= 255
                    )


tb_importancia  <- as.data.table( xgb.importance( model=modelo ) )


#Agregado nuevo
#tabla con estructura de arboles
m  <- xgb.model.dt.tree( model= modelo, feature_names= col_buenas )

tbl  <- m[ Node==0, ]
tbl[ m[Node==1], on="Tree", score1:= i.Quality  ]
tbl[ m[Node==2], on="Tree", score2:= i.Quality  ]

tbl[ , scoreNA := ifelse( substr(Missing, nchar(Missing), nchar(Missing))=="1", score1, score2 ) ]

score_get  <- function( pFeature, pvalor )
{
   return( sum( tbl[ Feature==pFeature & Split < pvalor,  score2]) +
           sum( tbl[ Feature==pFeature & pvalor <= Split, score1]) )
}


tbl2  <- unique(  tbl[ , c("Feature","Split") ] )
setorderv( tbl2, c("Feature","Split"), c(1,1) )
tbl2[  , delta := max(Split,na.rm=TRUE)-min(Split,na.rm=TRUE) , by=Feature ]

tbl3  <- tbl2[ , list( "Split" = min(Split) - min(ifelse( delta==0, Split, delta/10 )) ) ,  by=Feature ]
tbl4  <- tbl2[ , list( "Split" = max(Split) + max(ifelse( delta==0, Split, delta/10 )) ) ,  by=Feature ]

tbl5  <- as.data.table( rbind( tbl2[ ,c("Feature","Split"), with=FALSE], tbl3, tbl4 ))

setorderv( tbl5, c("Feature","Split"), c(1,1) )

for( i in 1:nrow(tbl5) )
{
  reg  <- tbl5[i] 
  tbl5[ i, score  := score_get( reg$Feature, reg$Split) ]
}

tbl5[ , delta :=  max(score) - min(score) , by=Feature ]
setorderv(  tbl5, c("delta", "Feature","Split"), c(-1,1,1) )

tblNA  <- tbl[ , list( "score"=sum(scoreNA) ), by=Feature ]


#determino los scores min y max para hacer todos los graficos en la misma escala vertical
score_min  <- tbl5[ , min(score, na.rm=TRUE ) ]
score_max  <- tbl5[ , max(score, na.rm=TRUE ) ]

options(scipen=5)

#Exporto el modelo aditivo
pdf( "ModeloAditivo_301.pdf" )

for( f in unique( tbl5$Feature  ))
{
  plot( tbl5[ Feature==f , Split ],
        tbl5[ Feature==f , score ],
        main= paste( f,  round( tbl5[ Feature==f , min(delta) ], 3)),
        xlab= f,
        ylab = "Score logistico",
        ylim = c( score_min, score_max),
        type="S",
        col= "blue"
      )

  grid()
}
dev.off()

