###############################
####   EN  CONSTRUCCION   #####
###############################

# Hibridacion de semillerios


#Necesita para correr en Google Cloud
# 128 GB de memoria RAM
#   8 vCPU


#limpio la memoria
rm( list= ls(all.names= TRUE) )  #remove all objects
gc( full= TRUE )                 #garbage collection

require("data.table")
require("primes")
require("yaml")

require("lightgbm")


#Parametros del script
PARAM  <- list()
PARAM$experimento  <- "ZZ8010"
PARAM$exp_input  <- c( "ZZ7792-01", "ZZ7792-02", "ZZ7792-03" )  # el inpput deben ser semillerios terminados


PARAM$kaggle$envios_desde  <-  8000L
PARAM$kaggle$envios_hasta  <- 13500L
PARAM$kaggle$envios_salto  <-   500L

PARAM$graficar$envios_hasta  <- 20000L  #para el caso que deba graficar
PARAM$graficar$ventana_suavizado  <- 2001L

PARAM$home  <- "~/buckets/b1/"
# FIN Parametros del script

OUTPUT  <- list()

#------------------------------------------------------------------------------
options(error= function() {
  traceback(20);
  options(error= NULL);
  stop("exiting after script error")
})
#------------------------------------------------------------------------------

GrabarOutput  <- function()
{
  write_yaml( OUTPUT, file= "output.yml" )   # grabo OUTPUT
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa
OUTPUT$PARAM  <- PARAM
OUTPUT$time$start  <- format(Sys.time(), "%Y%m%d %H%M%S")


#------------------------------------------------------------------------------
if( future_con_clase )
{
  OUTPUT$ganancias_suavizadas  <- vganancias_suavizadas
}

OUTPUT$time$end  <- format(Sys.time(), "%Y%m%d %H%M%S")
GrabarOutput()

#dejo la marca final
cat( format(Sys.time(), "%Y%m%d %H%M%S"),"\n",
     file= "zRend.txt",
     append= TRUE )

#------------------------------------------------------------------------------
#suicidio,  elimina la maquina virtual directamente
# para no tener que esperar a que termine una Bayesian Optimization
# sino Google me sigue facturando a pesar de no estar procesando nada
# Give them nothing, but take from them everything.

system( "sleep 10  &&
        export NAME=$(curl -X GET http://metadata.google.internal/computeMetadata/v1/instance/name -H 'Metadata-Flavor: Google') &&
        export ZONE=$(curl -X GET http://metadata.google.internal/computeMetadata/v1/instance/zone -H 'Metadata-Flavor: Google') &&
        gcloud --quiet compute instances delete $NAME --zone=$ZONE",
        wait= FALSE )
