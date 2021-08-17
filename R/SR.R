## Stock Reclutamiento (Ricker & BevertonHolt)
## **************************************************************************
## AUTOR: Pablo Martin Marin Abanto
## **************************************************************************
## Datos de archivo
##===========================================================================
  archivo = "st"
  file =  paste(archivo,".csv",sep="")
  
##===========================================================================
##Cargando la funcion SR.Fun
source("SR.Fun.R")

##===========================================================================
  data=read.table(file,header=T,sep=",")
  plot(data[,-1],pch=16)
  st=data$SSB
  rt=data$R
  yr=data$anho

  SR.Fun("Ricker")
  SR.Fun("BevertonHolt")




