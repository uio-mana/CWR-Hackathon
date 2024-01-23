######################################################################
# Script  de la herramienta rLayer - rLayer tool script 
# Autor de la herramienta - Author of the tool: Mauricio Parra Quijano 
# email: mauricio.parra@fao.org, website: http://capfitogen.net
# Universidad Nacional de Colombia (http://cienciasagrarias.bogota.unal.edu.co/)
# International Treaty on Plant Genetic Resources for Food and Agriculture (http://www.fao.org/plant-treaty/en/)  
# Farmer's pride project (http://www.farmerspride.eu/)
# 2021
######################################################################

#You can freely use and modify this script only for non-commercial purposes.Otherwise please contact to script author. Puede de manera libre usar y modificar este script s?lo con pro?sitos no comerciales. De otra forma, contacte con el autor de las herramientas.
#In any case, we encourage you include in your study publication the correspondent credits (about R software, packages and script author). En cualquier caso, le animamos a que incluya en la publicaci?n de su estudio los cr?ditos correspondientes (acerca del software R, los paquetes y el autor del script)
#To cite CAPFITOGEN in publications use: Parra-Quijano, M., Iriondo, J.M., Torres, M.E., Lopez, F., Phillips, J., and Kell, S. 2021. CAPFITOGEN3: A toolbox for the conservation and promotion of the use of agricultural biodiversity. ISBN: 978-958-505-038-9 URL:  http://www.capfitogen.net/en

#HOW TO USE THIS SCRIPT
#PLEASE SELECT ALL THE LINES IN THIS SCRIPT AND LATER CLICK ON THE "RUN" BUTTON. YOU WILL FIND THE RESULTS WHERE YOU INDICATED IT IN THE PARAMETER SCRIPT.

#COMO USAR ESTE SCRIPT
#POR FAVOR SELECCIONE TODAS LAS LINEAS DE ESTE SCRIPT Y DESPUES HAGA CLICK EN EL BOTON "RUN". LOS RESULTADOS LOS ENCONTRARA DONDE LO INDICO EN EL SCRIPT DE PARAMETROS

##########################################################################################################
##########################################################################################################
setwd(paste(ruta))
write("______NUEVO PROCESO rLayer________", file="Error/process_info.txt", append=TRUE)
write(date(), file="Error/process_info.txt", append=TRUE)

#Check e instalacion si a lugar
packages2<-vector()
if(system.file(package="sp")==""){
  packages2<-append(packages2,"sp")
}
if(system.file(package="raster")==""){
  packages2<-append(packages2,"raster")
}
if(system.file(package="maptools")==""){
  packages2<-append(packages2,"maptools")
}
if(system.file(package="rgdal")==""){
  packages2<-append(packages2,"rgdal")
}
if(system.file(package="dismo")==""){
  packages2<-append(packages2,"dismo")
}
if(system.file(package="rgeos")==""){
  packages2<-append(packages2,"rgeos")
}

#Instalar los que hagan falta
if(length(packages2)>0){
  install.packages(setdiff(packages2, rownames(installed.packages())))
}
#Carga de paquetes
library(sp)
library(raster)
library(maptools)
library(rgeos)
library(dismo)
library(rgdal)

#introducci?n tabla de lista de pa?ses y resoluciones de extracci?n a elegir y traducci?n
#Rversion
vvv<-R.Version()
vvv<-as.numeric(vvv$year)

#Condicional resol por si no lo abre v?a load.RData
loadError<-FALSE
abcd<-try(load("resol.RData"),silent=TRUE)
loadError <- (is(abcd, 'try-error')|is(abcd,'error'))
if(loadError){
  resol<-read.delim("resol.txt")
}
rm(abcd)
rm(loadError)

resol<-subset(resol,resolucion==paste(resol1))
resol<-as.character(resol[1,2])
setwd(paste(ruta,"/rdatamaps",sep=""))
dir.create(as.vector(paste(uname)))
setwd(paste(ruta,"/rdatamaps/",uname,sep=""))
dir.create(as.vector(paste(resol)))
setwd(paste(ruta))

if(cropway=="buffer"){
  #Entrar funcion para determinar si se produce (TRUE) un error
  is.error<-function (expr, tell = FALSE, force = FALSE) 
  {
    expr_name <- deparse(substitute(expr))
    test <- try(expr, silent = TRUE)
    iserror <- inherits(test, "try-error")
    if (tell) 
      if (iserror) 
        message("Note in is.error: ", test)
    if (force) 
      if (!iserror) 
        stop(expr_name, " is not returning an error.", call. = FALSE)
    iserror
  }
  
  if(is.error(read.delim(paste("Pasaporte/",pasaporte,sep=""),colClasses=c("LONGITUDE"="character","LATITUDE"="character")))){
    pasaporte<-read.delim(paste("Pasaporte/",pasaporte,sep=""),colClasses=c("LONGITUDE"="character","LATITUDE"="character"), fileEncoding="latin1")
  } else {pasaporte<-read.delim(paste("Pasaporte/",pasaporte,sep=""),colClasses=c("LONGITUDE"="character","LATITUDE"="character"))}
  
  i <- order(pasaporte$ACCENUMB)
  pasaporte <- pasaporte[i,]
  write("4.Cargados los datos de pasaporte y ordenados por ACCENUMB", file="Error/process_info.txt", append=TRUE)
  
  #filtro por GEOQUAL
  if(geoqual){
    pasaporte<-subset(pasaporte,TOTALQUAL100>=totalqual)
  }
  
  #Trifurcaci?n (sin coordenadas, con coordenadas sexagesimales, con coordenadas decimales)
  sincoord<-subset(pasaporte,(is.na(LATITUDE)|is.na(LONGITUDE))&(is.na(DECLATITUDE)|is.na(DECLONGITUDE)))
  
  sexagesimal<-subset(pasaporte,(!is.na(LATITUDE)&!is.na(LONGITUDE)))
  sexagesimal<-sexagesimal[,c(1:22,24,26:ncol(sexagesimal))]
  decimal<-subset(pasaporte,(is.na(LATITUDE)|is.na(LONGITUDE))&(!is.na(DECLATITUDE)&!is.na(DECLONGITUDE)))
  
  
  ###Sexagesimal a decimal  
  #Obtenci?n de las coordenadas en formato decimal a partir de sexagesimal codificadas tal como lo indica el formato IPGRI 2001
  if(length(sexagesimal[,1])>0){
    coordec<-as.data.frame(matrix(nrow = length(sexagesimal[,1]), ncol = 2))
    #colnames(coordec)[1]<-"DECLATITUDE"
    #colnames(coordec)[2]<-"DECLONGITUDE"
    coordec<-data.frame(sexagesimal$ACCENUMB,coordec)
    colnames(coordec)[1]<-"ACCENUMB"
    for (i in 1:length(sexagesimal[,1])) {
      coordec[i,2]<-ifelse(sexagesimal$LATITUDE[i]=='NA','NA', ((as.numeric(substr(sexagesimal$LATITUDE[i],1,2))+
                                                                   (ifelse(substr(sexagesimal$LATITUDE[i],3,4)=='--',0,(as.numeric(substr(sexagesimal$LATITUDE[i],3,4))/60)))+
                                                                   (ifelse(substr(sexagesimal$LATITUDE[i],5,6)=='--',0,(as.numeric(substr(sexagesimal$LATITUDE[i],5,6))/3600))))                                               
                                                                *(as.numeric(ifelse(substr(sexagesimal$LATITUDE[i],7,7)=='N',1,-1)))))
      coordec[i,3]<-ifelse(sexagesimal$LONGITUDE[i]=='NA','NA', ((as.numeric(substr(sexagesimal$LONGITUDE[i],1,3))+
                                                                    (ifelse(substr(sexagesimal$LONGITUDE[i],4,5)=='--',0,(as.numeric(substr(sexagesimal$LONGITUDE[i],4,5))/60)))+
                                                                    (ifelse(substr(sexagesimal$LONGITUDE[i],6,7)=='--',0,(as.numeric(substr(sexagesimal$LONGITUDE[i],6,7))/3600))))                                               
                                                                 *(as.numeric(ifelse(substr(sexagesimal$LONGITUDE[i],8,8)=='E',1,-1)))))
    }
    sexagesimal<-cbind(sexagesimal[,1:22],coordec[,2],sexagesimal[,23],coordec[,3],sexagesimal[,24:ncol(sexagesimal)])
    colnames(sexagesimal)[23]<-"DECLATITUDE"
    colnames(sexagesimal)[24]<-"LATITUDE"
    colnames(sexagesimal)[25]<-"DECLONGITUDE"
    write("5.Terminado proceso de transformaci?n sexagesimal a decimal", file="Error/process_info.txt", append=TRUE)
    ###Unificaci?n coordenadas sexagesimal a decimal y decimal original
    puntosorig<-rbind(sexagesimal,decimal)
  }
  if(length(sexagesimal[,1])==0){
    puntosorig<-decimal
  }
  
  #Entra la parte espacial
  puntos<-SpatialPoints(puntosorig[,c(25,23)], proj4string=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  #creaci?n del raster sobre los buffers desde los puntos
  influ<-circles(puntos,d=as.numeric(paste(buffer))*1000,lonlat=TRUE)
  influ<-influ@polygons
  #Creaci?n de la m?scara
  #Primero la resoluci?n o cell size
  resolucion<-raster(paste(ruta,"/rdatamaps/world/",resol,"/bio_1.tif",sep=""))
  resolucion<-xres(resolucion)
  #Creacion m?scara
  molde<-raster(influ)
  res(molde)<-resolucion
  moldextent<-extent(molde)
  projection(molde)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  values(molde)<-1
  molde<-mask(molde,influ)
  
  #Corte
  bio_1<-crop(raster(paste("rdatamaps/world/",resol,"/bio_1.tif",sep="")),molde)
  extent(bio_1)<-moldextent
  bio_1<-mask(bio_1,molde)
  writeRaster(bio_1,filename=paste("rdatamaps/",uname,"/",resol,"/bio_1.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_1)
  
  bio_2<-crop(raster(paste("rdatamaps/world/",resol,"/bio_2.tif",sep="")),molde)
  extent(bio_2)<-moldextent
  bio_2<-mask(bio_2,molde)
  writeRaster(bio_2,filename=paste("rdatamaps/",uname,"/",resol,"/bio_2.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_2)
  
  bio_3<-crop(raster(paste("rdatamaps/world/",resol,"/bio_3.tif",sep="")),molde)
  extent(bio_3)<-moldextent
  bio_3<-mask(bio_3,molde)
  writeRaster(bio_3,filename=paste("rdatamaps/",uname,"/",resol,"/bio_3.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_3)
  
  bio_4<-crop(raster(paste("rdatamaps/world/",resol,"/bio_4.tif",sep="")),molde)
  extent(bio_4)<-moldextent
  bio_4<-mask(bio_4,molde)
  writeRaster(bio_4,filename=paste("rdatamaps/",uname,"/",resol,"/bio_4.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_4)
  
  bio_5<-crop(raster(paste("rdatamaps/world/",resol,"/bio_5.tif",sep="")),molde)
  extent(bio_5)<-moldextent
  bio_5<-mask(bio_5,molde)
  writeRaster(bio_5,filename=paste("rdatamaps/",uname,"/",resol,"/bio_5.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_5)
  
  bio_6<-crop(raster(paste("rdatamaps/world/",resol,"/bio_6.tif",sep="")),molde)
  extent(bio_6)<-moldextent
  bio_6<-mask(bio_6,molde)
  writeRaster(bio_6,filename=paste("rdatamaps/",uname,"/",resol,"/bio_6.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_6)
  
  bio_7<-crop(raster(paste("rdatamaps/world/",resol,"/bio_7.tif",sep="")),molde)
  extent(bio_7)<-moldextent
  bio_7<-mask(bio_7,molde)
  writeRaster(bio_7,filename=paste("rdatamaps/",uname,"/",resol,"/bio_7.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_7)
  
  bio_8<-crop(raster(paste("rdatamaps/world/",resol,"/bio_8.tif",sep="")),molde)
  extent(bio_8)<-moldextent
  bio_8<-mask(bio_8,molde)
  writeRaster(bio_8,filename=paste("rdatamaps/",uname,"/",resol,"/bio_8.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_8)
  
  bio_9<-crop(raster(paste("rdatamaps/world/",resol,"/bio_9.tif",sep="")),molde)
  extent(bio_9)<-moldextent
  bio_9<-mask(bio_9,molde)
  writeRaster(bio_9,filename=paste("rdatamaps/",uname,"/",resol,"/bio_9.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_9)
  
  bio_10<-crop(raster(paste("rdatamaps/world/",resol,"/bio_10.tif",sep="")),molde)
  extent(bio_10)<-moldextent
  bio_10<-mask(bio_10,molde)
  writeRaster(bio_10,filename=paste("rdatamaps/",uname,"/",resol,"/bio_10.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_10)
  
  bio_11<-crop(raster(paste("rdatamaps/world/",resol,"/bio_11.tif",sep="")),molde)
  extent(bio_11)<-moldextent
  bio_11<-mask(bio_11,molde)
  writeRaster(bio_11,filename=paste("rdatamaps/",uname,"/",resol,"/bio_11.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_11)
  
  bio_12<-crop(raster(paste("rdatamaps/world/",resol,"/bio_12.tif",sep="")),molde)
  extent(bio_12)<-moldextent
  bio_12<-mask(bio_12,molde)
  writeRaster(bio_12,filename=paste("rdatamaps/",uname,"/",resol,"/bio_12.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_12)
  
  bio_13<-crop(raster(paste("rdatamaps/world/",resol,"/bio_13.tif",sep="")),molde)
  extent(bio_13)<-moldextent
  bio_13<-mask(bio_13,molde)
  writeRaster(bio_13,filename=paste("rdatamaps/",uname,"/",resol,"/bio_13.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_13)
  
  bio_14<-crop(raster(paste("rdatamaps/world/",resol,"/bio_14.tif",sep="")),molde)
  extent(bio_14)<-moldextent
  bio_14<-mask(bio_14,molde)
  writeRaster(bio_14,filename=paste("rdatamaps/",uname,"/",resol,"/bio_14.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_14)
  
  bio_15<-crop(raster(paste("rdatamaps/world/",resol,"/bio_15.tif",sep="")),molde)
  extent(bio_15)<-moldextent
  bio_15<-mask(bio_15,molde)
  writeRaster(bio_15,filename=paste("rdatamaps/",uname,"/",resol,"/bio_15.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_15)
  
  bio_16<-crop(raster(paste("rdatamaps/world/",resol,"/bio_16.tif",sep="")),molde)
  extent(bio_16)<-moldextent
  bio_16<-mask(bio_16,molde)
  writeRaster(bio_16,filename=paste("rdatamaps/",uname,"/",resol,"/bio_16.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_16)
  
  bio_17<-crop(raster(paste("rdatamaps/world/",resol,"/bio_17.tif",sep="")),molde)
  extent(bio_17)<-moldextent
  bio_17<-mask(bio_17,molde)
  writeRaster(bio_17,filename=paste("rdatamaps/",uname,"/",resol,"/bio_17.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_17)
  
  bio_18<-crop(raster(paste("rdatamaps/world/",resol,"/bio_18.tif",sep="")),molde)
  extent(bio_18)<-moldextent
  bio_18<-mask(bio_18,molde)
  writeRaster(bio_18,filename=paste("rdatamaps/",uname,"/",resol,"/bio_18.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_18)
  
  bio_19<-crop(raster(paste("rdatamaps/world/",resol,"/bio_19.tif",sep="")),molde)
  extent(bio_19)<-moldextent
  bio_19<-mask(bio_19,molde)
  writeRaster(bio_19,filename=paste("rdatamaps/",uname,"/",resol,"/bio_19.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_19)
  
  
  prec_1<-crop(raster(paste("rdatamaps/world/",resol,"/prec_1.tif",sep="")),molde)
  extent(prec_1)<-moldextent
  prec_1<-mask(prec_1,molde)
  writeRaster(prec_1,filename=paste("rdatamaps/",uname,"/",resol,"/prec_1.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(prec_1)
  
  prec_2<-crop(raster(paste("rdatamaps/world/",resol,"/prec_2.tif",sep="")),molde)
  extent(prec_2)<-moldextent
  prec_2<-mask(prec_2,molde)
  writeRaster(prec_2,filename=paste("rdatamaps/",uname,"/",resol,"/prec_2.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(prec_2)
  
  prec_3<-crop(raster(paste("rdatamaps/world/",resol,"/prec_3.tif",sep="")),molde)
  extent(prec_3)<-moldextent
  prec_3<-mask(prec_3,molde)
  writeRaster(prec_3,filename=paste("rdatamaps/",uname,"/",resol,"/prec_3.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(prec_3)
  
  prec_4<-crop(raster(paste("rdatamaps/world/",resol,"/prec_4.tif",sep="")),molde)
  extent(prec_4)<-moldextent
  prec_4<-mask(prec_4,molde)
  writeRaster(prec_4,filename=paste("rdatamaps/",uname,"/",resol,"/prec_4.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(prec_4)
  
  prec_5<-crop(raster(paste("rdatamaps/world/",resol,"/prec_5.tif",sep="")),molde)
  extent(prec_5)<-moldextent
  prec_5<-mask(prec_5,molde)
  writeRaster(prec_5,filename=paste("rdatamaps/",uname,"/",resol,"/prec_5.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(prec_5)
  
  prec_6<-crop(raster(paste("rdatamaps/world/",resol,"/prec_6.tif",sep="")),molde)
  extent(prec_6)<-moldextent
  prec_6<-mask(prec_6,molde)
  writeRaster(prec_6,filename=paste("rdatamaps/",uname,"/",resol,"/prec_6.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(prec_6)
  
  prec_7<-crop(raster(paste("rdatamaps/world/",resol,"/prec_7.tif",sep="")),molde)
  extent(prec_7)<-moldextent
  prec_7<-mask(prec_7,molde)
  writeRaster(prec_7,filename=paste("rdatamaps/",uname,"/",resol,"/prec_7.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(prec_7)
  
  prec_8<-crop(raster(paste("rdatamaps/world/",resol,"/prec_8.tif",sep="")),molde)
  extent(prec_8)<-moldextent
  prec_8<-mask(prec_8,molde)
  writeRaster(prec_8,filename=paste("rdatamaps/",uname,"/",resol,"/prec_8.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(prec_8)
  
  prec_9<-crop(raster(paste("rdatamaps/world/",resol,"/prec_9.tif",sep="")),molde)
  extent(prec_9)<-moldextent
  prec_9<-mask(prec_9,molde)
  writeRaster(prec_9,filename=paste("rdatamaps/",uname,"/",resol,"/prec_9.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(prec_9)
  
  prec_10<-crop(raster(paste("rdatamaps/world/",resol,"/prec_10.tif",sep="")),molde)
  extent(prec_10)<-moldextent
  prec_10<-mask(prec_10,molde)
  writeRaster(prec_10,filename=paste("rdatamaps/",uname,"/",resol,"/prec_10.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(prec_10)
  
  prec_11<-crop(raster(paste("rdatamaps/world/",resol,"/prec_11.tif",sep="")),molde)
  extent(prec_11)<-moldextent
  prec_11<-mask(prec_11,molde)
  writeRaster(prec_11,filename=paste("rdatamaps/",uname,"/",resol,"/prec_11.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(prec_11)
  
  prec_12<-crop(raster(paste("rdatamaps/world/",resol,"/prec_12.tif",sep="")),molde)
  extent(prec_12)<-moldextent
  prec_12<-mask(prec_12,molde)
  writeRaster(prec_12,filename=paste("rdatamaps/",uname,"/",resol,"/prec_12.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(prec_12)
  
  tmax_1<-crop(raster(paste("rdatamaps/world/",resol,"/tmax_1.tif",sep="")),molde)
  extent(tmax_1)<-moldextent
  tmax_1<-mask(tmax_1,molde)
  writeRaster(tmax_1,filename=paste("rdatamaps/",uname,"/",resol,"/tmax_1.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmax_1)
  
  tmax_2<-crop(raster(paste("rdatamaps/world/",resol,"/tmax_2.tif",sep="")),molde)
  extent(tmax_2)<-moldextent
  tmax_2<-mask(tmax_2,molde)
  writeRaster(tmax_2,filename=paste("rdatamaps/",uname,"/",resol,"/tmax_2.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmax_2)
  
  tmax_3<-crop(raster(paste("rdatamaps/world/",resol,"/tmax_3.tif",sep="")),molde)
  extent(tmax_3)<-moldextent
  tmax_3<-mask(tmax_3,molde)
  writeRaster(tmax_3,filename=paste("rdatamaps/",uname,"/",resol,"/tmax_3.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmax_3)
  
  tmax_4<-crop(raster(paste("rdatamaps/world/",resol,"/tmax_4.tif",sep="")),molde)
  extent(tmax_4)<-moldextent
  tmax_4<-mask(tmax_4,molde)
  writeRaster(tmax_4,filename=paste("rdatamaps/",uname,"/",resol,"/tmax_4.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmax_4)
  
  tmax_5<-crop(raster(paste("rdatamaps/world/",resol,"/tmax_5.tif",sep="")),molde)
  extent(tmax_5)<-moldextent
  tmax_5<-mask(tmax_5,molde)
  writeRaster(tmax_5,filename=paste("rdatamaps/",uname,"/",resol,"/tmax_5.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmax_5)
  
  tmax_6<-crop(raster(paste("rdatamaps/world/",resol,"/tmax_6.tif",sep="")),molde)
  extent(tmax_6)<-moldextent
  tmax_6<-mask(tmax_6,molde)
  writeRaster(tmax_6,filename=paste("rdatamaps/",uname,"/",resol,"/tmax_6.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmax_6)
  
  tmax_7<-crop(raster(paste("rdatamaps/world/",resol,"/tmax_7.tif",sep="")),molde)
  extent(tmax_7)<-moldextent
  tmax_7<-mask(tmax_7,molde)
  writeRaster(tmax_7,filename=paste("rdatamaps/",uname,"/",resol,"/tmax_7.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmax_7)
  
  tmax_8<-crop(raster(paste("rdatamaps/world/",resol,"/tmax_8.tif",sep="")),molde)
  extent(tmax_8)<-moldextent
  tmax_8<-mask(tmax_8,molde)
  writeRaster(tmax_8,filename=paste("rdatamaps/",uname,"/",resol,"/tmax_8.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmax_8)
  
  tmax_9<-crop(raster(paste("rdatamaps/world/",resol,"/tmax_9.tif",sep="")),molde)
  extent(tmax_9)<-moldextent
  tmax_9<-mask(tmax_9,molde)
  writeRaster(tmax_9,filename=paste("rdatamaps/",uname,"/",resol,"/tmax_9.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmax_9)
  
  tmax_10<-crop(raster(paste("rdatamaps/world/",resol,"/tmax_10.tif",sep="")),molde)
  extent(tmax_10)<-moldextent
  tmax_10<-mask(tmax_10,molde)
  writeRaster(tmax_10,filename=paste("rdatamaps/",uname,"/",resol,"/tmax_10.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmax_10)
  
  tmax_11<-crop(raster(paste("rdatamaps/world/",resol,"/tmax_11.tif",sep="")),molde)
  extent(tmax_11)<-moldextent
  tmax_11<-mask(tmax_11,molde)
  writeRaster(tmax_11,filename=paste("rdatamaps/",uname,"/",resol,"/tmax_11.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmax_11)
  
  tmax_12<-crop(raster(paste("rdatamaps/world/",resol,"/tmax_12.tif",sep="")),molde)
  extent(tmax_12)<-moldextent
  tmax_12<-mask(tmax_12,molde)
  writeRaster(tmax_12,filename=paste("rdatamaps/",uname,"/",resol,"/tmax_12.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmax_12)
  
  tmin_1<-crop(raster(paste("rdatamaps/world/",resol,"/tmin_1.tif",sep="")),molde)
  extent(tmin_1)<-moldextent
  tmin_1<-mask(tmin_1,molde)
  writeRaster(tmin_1,filename=paste("rdatamaps/",uname,"/",resol,"/tmin_1.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmin_1)
  
  tmin_2<-crop(raster(paste("rdatamaps/world/",resol,"/tmin_2.tif",sep="")),molde)
  extent(tmin_2)<-moldextent
  tmin_2<-mask(tmin_2,molde)
  writeRaster(tmin_2,filename=paste("rdatamaps/",uname,"/",resol,"/tmin_2.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmin_2)
  
  tmin_3<-crop(raster(paste("rdatamaps/world/",resol,"/tmin_3.tif",sep="")),molde)
  extent(tmin_3)<-moldextent
  tmin_3<-mask(tmin_3,molde)
  writeRaster(tmin_3,filename=paste("rdatamaps/",uname,"/",resol,"/tmin_3.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmin_3)
  
  tmin_4<-crop(raster(paste("rdatamaps/world/",resol,"/tmin_4.tif",sep="")),molde)
  extent(tmin_4)<-moldextent
  tmin_4<-mask(tmin_4,molde)
  writeRaster(tmin_4,filename=paste("rdatamaps/",uname,"/",resol,"/tmin_4.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmin_4)
  
  tmin_5<-crop(raster(paste("rdatamaps/world/",resol,"/tmin_5.tif",sep="")),molde)
  extent(tmin_5)<-moldextent
  tmin_5<-mask(tmin_5,molde)
  writeRaster(tmin_5,filename=paste("rdatamaps/",uname,"/",resol,"/tmin_5.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmin_5)
  
  tmin_6<-crop(raster(paste("rdatamaps/world/",resol,"/tmin_6.tif",sep="")),molde)
  extent(tmin_6)<-moldextent
  tmin_6<-mask(tmin_6,molde)
  writeRaster(tmin_6,filename=paste("rdatamaps/",uname,"/",resol,"/tmin_6.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmin_6)
  
  tmin_7<-crop(raster(paste("rdatamaps/world/",resol,"/tmin_7.tif",sep="")),molde)
  extent(tmin_7)<-moldextent
  tmin_7<-mask(tmin_7,molde)
  writeRaster(tmin_7,filename=paste("rdatamaps/",uname,"/",resol,"/tmin_7.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmin_7)
  
  tmin_8<-crop(raster(paste("rdatamaps/world/",resol,"/tmin_8.tif",sep="")),molde)
  extent(tmin_8)<-moldextent
  tmin_8<-mask(tmin_8,molde)
  writeRaster(tmin_8,filename=paste("rdatamaps/",uname,"/",resol,"/tmin_8.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmin_8)
  
  tmin_9<-crop(raster(paste("rdatamaps/world/",resol,"/tmin_9.tif",sep="")),molde)
  extent(tmin_9)<-moldextent
  tmin_9<-mask(tmin_9,molde)
  writeRaster(tmin_9,filename=paste("rdatamaps/",uname,"/",resol,"/tmin_9.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmin_9)
  
  tmin_10<-crop(raster(paste("rdatamaps/world/",resol,"/tmin_10.tif",sep="")),molde)
  extent(tmin_10)<-moldextent
  tmin_10<-mask(tmin_10,molde)
  writeRaster(tmin_10,filename=paste("rdatamaps/",uname,"/",resol,"/tmin_10.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmin_10)
  
  tmin_11<-crop(raster(paste("rdatamaps/world/",resol,"/tmin_11.tif",sep="")),molde)
  extent(tmin_11)<-moldextent
  tmin_11<-mask(tmin_11,molde)
  writeRaster(tmin_11,filename=paste("rdatamaps/",uname,"/",resol,"/tmin_11.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmin_11)
  
  tmin_12<-crop(raster(paste("rdatamaps/world/",resol,"/tmin_12.tif",sep="")),molde)
  extent(tmin_12)<-moldextent
  tmin_12<-mask(tmin_12,molde)
  writeRaster(tmin_12,filename=paste("rdatamaps/",uname,"/",resol,"/tmin_12.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmin_12)
  
  tmean_1<-crop(raster(paste("rdatamaps/world/",resol,"/tmean_1.tif",sep="")),molde)
  extent(tmean_1)<-moldextent
  tmean_1<-mask(tmean_1,molde)
  writeRaster(tmean_1,filename=paste("rdatamaps/",uname,"/",resol,"/tmean_1.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmean_1)
  
  tmean_2<-crop(raster(paste("rdatamaps/world/",resol,"/tmean_2.tif",sep="")),molde)
  extent(tmean_2)<-moldextent
  tmean_2<-mask(tmean_2,molde)
  writeRaster(tmean_2,filename=paste("rdatamaps/",uname,"/",resol,"/tmean_2.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmean_2)
  
  tmean_3<-crop(raster(paste("rdatamaps/world/",resol,"/tmean_3.tif",sep="")),molde)
  extent(tmean_3)<-moldextent
  tmean_3<-mask(tmean_3,molde)
  writeRaster(tmean_3,filename=paste("rdatamaps/",uname,"/",resol,"/tmean_3.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmean_3)
  
  tmean_4<-crop(raster(paste("rdatamaps/world/",resol,"/tmean_4.tif",sep="")),molde)
  extent(tmean_4)<-moldextent
  tmean_4<-mask(tmean_4,molde)
  writeRaster(tmean_4,filename=paste("rdatamaps/",uname,"/",resol,"/tmean_4.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmean_4)
  
  tmean_5<-crop(raster(paste("rdatamaps/world/",resol,"/tmean_5.tif",sep="")),molde)
  extent(tmean_5)<-moldextent
  tmean_5<-mask(tmean_5,molde)
  writeRaster(tmean_5,filename=paste("rdatamaps/",uname,"/",resol,"/tmean_5.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmean_5)
  
  tmean_6<-crop(raster(paste("rdatamaps/world/",resol,"/tmean_6.tif",sep="")),molde)
  extent(tmean_6)<-moldextent
  tmean_6<-mask(tmean_6,molde)
  writeRaster(tmean_6,filename=paste("rdatamaps/",uname,"/",resol,"/tmean_6.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmean_6)
  
  tmean_7<-crop(raster(paste("rdatamaps/world/",resol,"/tmean_7.tif",sep="")),molde)
  extent(tmean_7)<-moldextent
  tmean_7<-mask(tmean_7,molde)
  writeRaster(tmean_7,filename=paste("rdatamaps/",uname,"/",resol,"/tmean_7.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmean_7)
  
  tmean_8<-crop(raster(paste("rdatamaps/world/",resol,"/tmean_8.tif",sep="")),molde)
  extent(tmean_8)<-moldextent
  tmean_8<-mask(tmean_8,molde)
  writeRaster(tmean_8,filename=paste("rdatamaps/",uname,"/",resol,"/tmean_8.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmean_8)
  
  tmean_9<-crop(raster(paste("rdatamaps/world/",resol,"/tmean_9.tif",sep="")),molde)
  extent(tmean_9)<-moldextent
  tmean_9<-mask(tmean_9,molde)
  writeRaster(tmean_9,filename=paste("rdatamaps/",uname,"/",resol,"/tmean_9.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmean_9)
  
  tmean_10<-crop(raster(paste("rdatamaps/world/",resol,"/tmean_10.tif",sep="")),molde)
  extent(tmean_10)<-moldextent
  tmean_10<-mask(tmean_10,molde)
  writeRaster(tmean_10,filename=paste("rdatamaps/",uname,"/",resol,"/tmean_10.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmean_10)
  
  tmean_11<-crop(raster(paste("rdatamaps/world/",resol,"/tmean_11.tif",sep="")),molde)
  extent(tmean_11)<-moldextent
  tmean_11<-mask(tmean_11,molde)
  writeRaster(tmean_11,filename=paste("rdatamaps/",uname,"/",resol,"/tmean_11.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmean_11)
  
  tmean_12<-crop(raster(paste("rdatamaps/world/",resol,"/tmean_12.tif",sep="")),molde)
  extent(tmean_12)<-moldextent
  tmean_12<-mask(tmean_12,molde)
  writeRaster(tmean_12,filename=paste("rdatamaps/",uname,"/",resol,"/tmean_12.tif",sep=""),overwrite=T,datatype='FLT4S')
  
  
  ####GEOPHYS
  
  alt<-crop(raster(paste("rdatamaps/world/",resol,"/alt.tif",sep="")),molde)
  extent(alt)<-moldextent
  alt<-mask(alt,molde)
  writeRaster(alt,filename=paste("rdatamaps/",uname,"/",resol,"/alt.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(alt)
  
  aspect<-crop(raster(paste("rdatamaps/world/",resol,"/aspect.tif",sep="")),molde)
  extent(aspect)<-moldextent
  aspect<-mask(aspect,molde)
  writeRaster(aspect,filename=paste("rdatamaps/",uname,"/",resol,"/aspect.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(aspect)
  
  eastness<-crop(raster(paste("rdatamaps/world/",resol,"/eastness.tif",sep="")),molde)
  extent(eastness)<-moldextent
  eastness<-mask(eastness,molde)
  writeRaster(eastness,filename=paste("rdatamaps/",uname,"/",resol,"/eastness.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(eastness)
  
  northness<-crop(raster(paste("rdatamaps/world/",resol,"/northness.tif",sep="")),molde)
  extent(northness)<-moldextent
  northness<-mask(northness,molde)
  writeRaster(northness,filename=paste("rdatamaps/",uname,"/",resol,"/northness.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(northness)
  
  slope<-crop(raster(paste("rdatamaps/world/",resol,"/slope.tif",sep="")),molde)
  extent(slope)<-moldextent
  slope<-mask(slope,molde)
  writeRaster(slope,filename=paste("rdatamaps/",uname,"/",resol,"/slope.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(slope)
  
  ###EDAPH
  
  ref_depth<-crop(raster(paste("rdatamaps/world/",resol,"/ref_depth.tif",sep="")),molde)
  extent(ref_depth)<-moldextent
  ref_depth<-mask(ref_depth,molde)
  writeRaster(ref_depth,filename=paste("rdatamaps/",uname,"/",resol,"/ref_depth.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(ref_depth)
  
  s_bs<-crop(raster(paste("rdatamaps/world/",resol,"/s_bs.tif",sep="")),molde)
  extent(s_bs)<-moldextent
  s_bs<-mask(s_bs,molde)
  writeRaster(s_bs,filename=paste("rdatamaps/",uname,"/",resol,"/s_bs.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_bs)
  
  s_caco3<-crop(raster(paste("rdatamaps/world/",resol,"/s_caco3.tif",sep="")),molde)
  extent(s_caco3)<-moldextent
  s_caco3<-mask(s_caco3,molde)
  writeRaster(s_caco3,filename=paste("rdatamaps/",uname,"/",resol,"/s_caco3.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_caco3)
  
  s_caso4<-crop(raster(paste("rdatamaps/world/",resol,"/s_caso4.tif",sep="")),molde)
  extent(s_caso4)<-moldextent
  s_caso4<-mask(s_caso4,molde)
  writeRaster(s_caso4,filename=paste("rdatamaps/",uname,"/",resol,"/s_caso4.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_caso4)
  
  s_cec_clay<-crop(raster(paste("rdatamaps/world/",resol,"/s_cec_clay.tif",sep="")),molde)
  extent(s_cec_clay)<-moldextent
  s_cec_clay<-mask(s_cec_clay,molde)
  writeRaster(s_cec_clay,filename=paste("rdatamaps/",uname,"/",resol,"/s_cec_clay.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_cec_clay)
  
  s_cec_soil<-crop(raster(paste("rdatamaps/world/",resol,"/s_cec_soil.tif",sep="")),molde)
  extent(s_cec_soil)<-moldextent
  s_cec_soil<-mask(s_cec_soil,molde)
  writeRaster(s_cec_soil,filename=paste("rdatamaps/",uname,"/",resol,"/s_cec_soil.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_cec_soil)
  
  s_clay<-crop(raster(paste("rdatamaps/world/",resol,"/s_clay.tif",sep="")),molde)
  extent(s_clay)<-moldextent
  s_clay<-mask(s_clay,molde)
  writeRaster(s_clay,filename=paste("rdatamaps/",uname,"/",resol,"/s_clay.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_clay)
  
  s_ece<-crop(raster(paste("rdatamaps/world/",resol,"/s_ece.tif",sep="")),molde)
  extent(s_ece)<-moldextent
  s_ece<-mask(s_ece,molde)
  writeRaster(s_ece,filename=paste("rdatamaps/",uname,"/",resol,"/s_ece.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_ece)
  
  s_esp<-crop(raster(paste("rdatamaps/world/",resol,"/s_esp.tif",sep="")),molde)
  extent(s_esp)<-moldextent
  s_esp<-mask(s_esp,molde)
  writeRaster(s_esp,filename=paste("rdatamaps/",uname,"/",resol,"/s_esp.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_esp)
  
  s_gravel<-crop(raster(paste("rdatamaps/world/",resol,"/s_gravel.tif",sep="")),molde)
  extent(s_gravel)<-moldextent
  s_gravel<-mask(s_gravel,molde)
  writeRaster(s_gravel,filename=paste("rdatamaps/",uname,"/",resol,"/s_gravel.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_gravel)
  
  s_oc<-crop(raster(paste("rdatamaps/world/",resol,"/s_oc.tif",sep="")),molde)
  extent(s_oc)<-moldextent
  s_oc<-mask(s_oc,molde)
  writeRaster(s_oc,filename=paste("rdatamaps/",uname,"/",resol,"/s_oc.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_oc)
  
  s_ph_h2o<-crop(raster(paste("rdatamaps/world/",resol,"/s_ph_h2o.tif",sep="")),molde)
  extent(s_ph_h2o)<-moldextent
  s_ph_h2o<-mask(s_ph_h2o,molde)
  writeRaster(s_ph_h2o,filename=paste("rdatamaps/",uname,"/",resol,"/s_ph_h2o.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_ph_h2o)
  
  s_ref_bulk<-crop(raster(paste("rdatamaps/world/",resol,"/s_ref_bulk.tif",sep="")),molde)
  extent(s_ref_bulk)<-moldextent
  s_ref_bulk<-mask(s_ref_bulk,molde)
  writeRaster(s_ref_bulk,filename=paste("rdatamaps/",uname,"/",resol,"/s_ref_bulk.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_ref_bulk)
  
  s_sand<-crop(raster(paste("rdatamaps/world/",resol,"/s_sand.tif",sep="")),molde)
  extent(s_sand)<-moldextent
  s_sand<-mask(s_sand,molde)
  writeRaster(s_sand,filename=paste("rdatamaps/",uname,"/",resol,"/s_sand.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_sand)
  
  s_silt<-crop(raster(paste("rdatamaps/world/",resol,"/s_silt.tif",sep="")),molde)
  extent(s_silt)<-moldextent
  s_silt<-mask(s_silt,molde)
  writeRaster(s_silt,filename=paste("rdatamaps/",uname,"/",resol,"/s_silt.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_silt)
  
  s_teb<-crop(raster(paste("rdatamaps/world/",resol,"/s_teb.tif",sep="")),molde)
  extent(s_teb)<-moldextent
  s_teb<-mask(s_teb,molde)
  writeRaster(s_teb,filename=paste("rdatamaps/",uname,"/",resol,"/s_teb.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_teb)
  
  t_bs<-crop(raster(paste("rdatamaps/world/",resol,"/t_bs.tif",sep="")),molde)
  extent(t_bs)<-moldextent
  t_bs<-mask(t_bs,molde)
  writeRaster(t_bs,filename=paste("rdatamaps/",uname,"/",resol,"/t_bs.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_bs)
  
  t_caco3<-crop(raster(paste("rdatamaps/world/",resol,"/t_caco3.tif",sep="")),molde)
  extent(t_caco3)<-moldextent
  t_caco3<-mask(t_caco3,molde)
  writeRaster(t_caco3,filename=paste("rdatamaps/",uname,"/",resol,"/t_caco3.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_caco3)
  
  t_caso4<-crop(raster(paste("rdatamaps/world/",resol,"/t_caso4.tif",sep="")),molde)
  extent(t_caso4)<-moldextent
  t_caso4<-mask(t_caso4,molde)
  writeRaster(t_caso4,filename=paste("rdatamaps/",uname,"/",resol,"/t_caso4.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_caso4)
  
  t_cec_clay<-crop(raster(paste("rdatamaps/world/",resol,"/t_cec_clay.tif",sep="")),molde)
  extent(t_cec_clay)<-moldextent
  t_cec_clay<-mask(t_cec_clay,molde)
  writeRaster(t_cec_clay,filename=paste("rdatamaps/",uname,"/",resol,"/t_cec_clay.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_cec_clay)
  
  t_cec_soil<-crop(raster(paste("rdatamaps/world/",resol,"/t_cec_soil.tif",sep="")),molde)
  extent(t_cec_soil)<-moldextent
  t_cec_soil<-mask(t_cec_soil,molde)
  writeRaster(t_cec_soil,filename=paste("rdatamaps/",uname,"/",resol,"/t_cec_soil.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_cec_soil)
  
  t_clay<-crop(raster(paste("rdatamaps/world/",resol,"/t_clay.tif",sep="")),molde)
  extent(t_clay)<-moldextent
  t_clay<-mask(t_clay,molde)
  writeRaster(t_clay,filename=paste("rdatamaps/",uname,"/",resol,"/t_clay.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_clay)
  
  t_ece<-crop(raster(paste("rdatamaps/world/",resol,"/t_ece.tif",sep="")),molde)
  extent(t_ece)<-moldextent
  t_ece<-mask(t_ece,molde)
  writeRaster(t_ece,filename=paste("rdatamaps/",uname,"/",resol,"/t_ece.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_ece)
  
  t_esp<-crop(raster(paste("rdatamaps/world/",resol,"/t_esp.tif",sep="")),molde)
  extent(t_esp)<-moldextent
  t_esp<-mask(t_esp,molde)
  writeRaster(t_esp,filename=paste("rdatamaps/",uname,"/",resol,"/t_esp.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_esp)
  
  t_gravel<-crop(raster(paste("rdatamaps/world/",resol,"/t_gravel.tif",sep="")),molde)
  extent(t_gravel)<-moldextent
  t_gravel<-mask(t_gravel,molde)
  writeRaster(t_gravel,filename=paste("rdatamaps/",uname,"/",resol,"/t_gravel.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_gravel)
  
  t_oc<-crop(raster(paste("rdatamaps/world/",resol,"/t_oc.tif",sep="")),molde)
  extent(t_oc)<-moldextent
  t_oc<-mask(t_oc,molde)
  writeRaster(t_oc,filename=paste("rdatamaps/",uname,"/",resol,"/t_oc.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_oc)
  
  t_ph_h2o<-crop(raster(paste("rdatamaps/world/",resol,"/t_ph_h2o.tif",sep="")),molde)
  extent(t_ph_h2o)<-moldextent
  t_ph_h2o<-mask(t_ph_h2o,molde)
  writeRaster(t_ph_h2o,filename=paste("rdatamaps/",uname,"/",resol,"/t_ph_h2o.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_ph_h2o)
  
  t_ref_bulk<-crop(raster(paste("rdatamaps/world/",resol,"/t_ref_bulk.tif",sep="")),molde)
  extent(t_ref_bulk)<-moldextent
  t_ref_bulk<-mask(t_ref_bulk,molde)
  writeRaster(t_ref_bulk,filename=paste("rdatamaps/",uname,"/",resol,"/t_ref_bulk.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_ref_bulk)
  
  t_sand<-crop(raster(paste("rdatamaps/world/",resol,"/t_sand.tif",sep="")),molde)
  extent(t_sand)<-moldextent
  t_sand<-mask(t_sand,molde)
  writeRaster(t_sand,filename=paste("rdatamaps/",uname,"/",resol,"/t_sand.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_sand)
  
  t_silt<-crop(raster(paste("rdatamaps/world/",resol,"/t_silt.tif",sep="")),molde)
  extent(t_silt)<-moldextent
  t_silt<-mask(t_silt,molde)
  writeRaster(t_silt,filename=paste("rdatamaps/",uname,"/",resol,"/t_silt.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_silt)
  
  t_teb<-crop(raster(paste("rdatamaps/world/",resol,"/t_teb.tif",sep="")),molde)
  extent(t_teb)<-moldextent
  t_teb<-mask(t_teb,molde)
  writeRaster(t_teb,filename=paste("rdatamaps/",uname,"/",resol,"/t_teb.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_teb)
  
  #Nuevas variables
  
  srad_1<-crop(raster(paste("rdatamaps/world/",resol,"/srad_1.tif",sep="")),molde)
  extent(srad_1)<-moldextent
  srad_1<-mask(srad_1,molde)
  writeRaster(srad_1,filename=paste("rdatamaps/",uname,"/",resol,"/srad_1.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(srad_1)
  
  srad_2<-crop(raster(paste("rdatamaps/world/",resol,"/srad_2.tif",sep="")),molde)
  extent(srad_2)<-moldextent
  srad_2<-mask(srad_2,molde)
  writeRaster(srad_2,filename=paste("rdatamaps/",uname,"/",resol,"/srad_2.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(srad_2)
  
  srad_3<-crop(raster(paste("rdatamaps/world/",resol,"/srad_3.tif",sep="")),molde)
  extent(srad_3)<-moldextent
  srad_3<-mask(srad_3,molde)
  writeRaster(srad_3,filename=paste("rdatamaps/",uname,"/",resol,"/srad_3.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(srad_3)
  
  srad_4<-crop(raster(paste("rdatamaps/world/",resol,"/srad_4.tif",sep="")),molde)
  extent(srad_4)<-moldextent
  srad_4<-mask(srad_4,molde)
  writeRaster(srad_4,filename=paste("rdatamaps/",uname,"/",resol,"/srad_4.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(srad_4)
  
  srad_5<-crop(raster(paste("rdatamaps/world/",resol,"/srad_5.tif",sep="")),molde)
  extent(srad_5)<-moldextent
  srad_5<-mask(srad_5,molde)
  writeRaster(srad_5,filename=paste("rdatamaps/",uname,"/",resol,"/srad_5.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(srad_5)
  
  srad_6<-crop(raster(paste("rdatamaps/world/",resol,"/srad_6.tif",sep="")),molde)
  extent(srad_6)<-moldextent
  srad_6<-mask(srad_6,molde)
  writeRaster(srad_6,filename=paste("rdatamaps/",uname,"/",resol,"/srad_6.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(srad_6)
  
  srad_7<-crop(raster(paste("rdatamaps/world/",resol,"/srad_7.tif",sep="")),molde)
  extent(srad_7)<-moldextent
  srad_7<-mask(srad_7,molde)
  writeRaster(srad_7,filename=paste("rdatamaps/",uname,"/",resol,"/srad_7.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(srad_7)
  
  srad_8<-crop(raster(paste("rdatamaps/world/",resol,"/srad_8.tif",sep="")),molde)
  extent(srad_8)<-moldextent
  srad_8<-mask(srad_8,molde)
  writeRaster(srad_8,filename=paste("rdatamaps/",uname,"/",resol,"/srad_8.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(srad_8)
  
  srad_9<-crop(raster(paste("rdatamaps/world/",resol,"/srad_9.tif",sep="")),molde)
  extent(srad_9)<-moldextent
  srad_9<-mask(srad_9,molde)
  writeRaster(srad_9,filename=paste("rdatamaps/",uname,"/",resol,"/srad_9.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(srad_9)
  
  srad_10<-crop(raster(paste("rdatamaps/world/",resol,"/srad_10.tif",sep="")),molde)
  extent(srad_10)<-moldextent
  srad_10<-mask(srad_10,molde)
  writeRaster(srad_10,filename=paste("rdatamaps/",uname,"/",resol,"/srad_10.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(srad_10)
  
  srad_11<-crop(raster(paste("rdatamaps/world/",resol,"/srad_11.tif",sep="")),molde)
  extent(srad_11)<-moldextent
  srad_11<-mask(srad_11,molde)
  writeRaster(srad_11,filename=paste("rdatamaps/",uname,"/",resol,"/srad_11.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(srad_11)
  
  srad_12<-crop(raster(paste("rdatamaps/world/",resol,"/srad_12.tif",sep="")),molde)
  extent(srad_12)<-moldextent
  srad_12<-mask(srad_12,molde)
  writeRaster(srad_12,filename=paste("rdatamaps/",uname,"/",resol,"/srad_12.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(srad_12)
  
  srad_annual<-crop(raster(paste("rdatamaps/world/",resol,"/srad_annual.tif",sep="")),molde)
  extent(srad_annual)<-moldextent
  srad_annual<-mask(srad_annual,molde)
  writeRaster(srad_annual,filename=paste("rdatamaps/",uname,"/",resol,"/srad_annual.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(srad_annual)
  
  wind_1<-crop(raster(paste("rdatamaps/world/",resol,"/wind_1.tif",sep="")),molde)
  extent(wind_1)<-moldextent
  wind_1<-mask(wind_1,molde)
  writeRaster(wind_1,filename=paste("rdatamaps/",uname,"/",resol,"/wind_1.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(wind_1)
  
  wind_2<-crop(raster(paste("rdatamaps/world/",resol,"/wind_2.tif",sep="")),molde)
  extent(wind_2)<-moldextent
  wind_2<-mask(wind_2,molde)
  writeRaster(wind_2,filename=paste("rdatamaps/",uname,"/",resol,"/wind_2.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(wind_2)
  
  wind_3<-crop(raster(paste("rdatamaps/world/",resol,"/wind_3.tif",sep="")),molde)
  extent(wind_3)<-moldextent
  wind_3<-mask(wind_3,molde)
  writeRaster(wind_3,filename=paste("rdatamaps/",uname,"/",resol,"/wind_3.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(wind_3)
  
  wind_4<-crop(raster(paste("rdatamaps/world/",resol,"/wind_4.tif",sep="")),molde)
  extent(wind_4)<-moldextent
  wind_4<-mask(wind_4,molde)
  writeRaster(wind_4,filename=paste("rdatamaps/",uname,"/",resol,"/wind_4.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(wind_4)
  
  wind_5<-crop(raster(paste("rdatamaps/world/",resol,"/wind_5.tif",sep="")),molde)
  extent(wind_5)<-moldextent
  wind_5<-mask(wind_5,molde)
  writeRaster(wind_5,filename=paste("rdatamaps/",uname,"/",resol,"/wind_5.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(wind_5)
  
  wind_6<-crop(raster(paste("rdatamaps/world/",resol,"/wind_6.tif",sep="")),molde)
  extent(wind_6)<-moldextent
  wind_6<-mask(wind_6,molde)
  writeRaster(wind_6,filename=paste("rdatamaps/",uname,"/",resol,"/wind_6.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(wind_6)
  
  wind_7<-crop(raster(paste("rdatamaps/world/",resol,"/wind_7.tif",sep="")),molde)
  extent(wind_7)<-moldextent
  wind_7<-mask(wind_7,molde)
  writeRaster(wind_7,filename=paste("rdatamaps/",uname,"/",resol,"/wind_7.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(wind_7)
  
  wind_8<-crop(raster(paste("rdatamaps/world/",resol,"/wind_8.tif",sep="")),molde)
  extent(wind_8)<-moldextent
  wind_8<-mask(wind_8,molde)
  writeRaster(wind_8,filename=paste("rdatamaps/",uname,"/",resol,"/wind_8.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(wind_8)
  
  wind_9<-crop(raster(paste("rdatamaps/world/",resol,"/wind_9.tif",sep="")),molde)
  extent(wind_9)<-moldextent
  wind_9<-mask(wind_9,molde)
  writeRaster(wind_9,filename=paste("rdatamaps/",uname,"/",resol,"/wind_9.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(wind_9)
  
  wind_10<-crop(raster(paste("rdatamaps/world/",resol,"/wind_10.tif",sep="")),molde)
  extent(wind_10)<-moldextent
  wind_10<-mask(wind_10,molde)
  writeRaster(wind_10,filename=paste("rdatamaps/",uname,"/",resol,"/wind_10.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(wind_10)
  
  wind_11<-crop(raster(paste("rdatamaps/world/",resol,"/wind_11.tif",sep="")),molde)
  extent(wind_11)<-moldextent
  wind_11<-mask(wind_11,molde)
  writeRaster(wind_11,filename=paste("rdatamaps/",uname,"/",resol,"/wind_11.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(wind_11)
  
  wind_12<-crop(raster(paste("rdatamaps/world/",resol,"/wind_12.tif",sep="")),molde)
  extent(wind_12)<-moldextent
  wind_12<-mask(wind_12,molde)
  writeRaster(wind_12,filename=paste("rdatamaps/",uname,"/",resol,"/wind_12.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(wind_12)
  
  wind_annual<-crop(raster(paste("rdatamaps/world/",resol,"/wind_annual.tif",sep="")),molde)
  extent(wind_annual)<-moldextent
  wind_annual<-mask(wind_annual,molde)
  writeRaster(wind_annual,filename=paste("rdatamaps/",uname,"/",resol,"/wind_annual.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(wind_annual)
  
  vapr_1<-crop(raster(paste("rdatamaps/world/",resol,"/vapr_1.tif",sep="")),molde)
  extent(vapr_1)<-moldextent
  vapr_1<-mask(vapr_1,molde)
  writeRaster(vapr_1,filename=paste("rdatamaps/",uname,"/",resol,"/vapr_1.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(vapr_1)
  
  vapr_2<-crop(raster(paste("rdatamaps/world/",resol,"/vapr_2.tif",sep="")),molde)
  extent(vapr_2)<-moldextent
  vapr_2<-mask(vapr_2,molde)
  writeRaster(vapr_2,filename=paste("rdatamaps/",uname,"/",resol,"/vapr_2.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(vapr_2)
  
  vapr_3<-crop(raster(paste("rdatamaps/world/",resol,"/vapr_3.tif",sep="")),molde)
  extent(vapr_3)<-moldextent
  vapr_3<-mask(vapr_3,molde)
  writeRaster(vapr_3,filename=paste("rdatamaps/",uname,"/",resol,"/vapr_3.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(vapr_3)
  
  vapr_4<-crop(raster(paste("rdatamaps/world/",resol,"/vapr_4.tif",sep="")),molde)
  extent(vapr_4)<-moldextent
  vapr_4<-mask(vapr_4,molde)
  writeRaster(vapr_4,filename=paste("rdatamaps/",uname,"/",resol,"/vapr_4.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(vapr_4)
  
  vapr_5<-crop(raster(paste("rdatamaps/world/",resol,"/vapr_5.tif",sep="")),molde)
  extent(vapr_5)<-moldextent
  vapr_5<-mask(vapr_5,molde)
  writeRaster(vapr_5,filename=paste("rdatamaps/",uname,"/",resol,"/vapr_5.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(vapr_5)
  
  vapr_6<-crop(raster(paste("rdatamaps/world/",resol,"/vapr_6.tif",sep="")),molde)
  extent(vapr_6)<-moldextent
  vapr_6<-mask(vapr_6,molde)
  writeRaster(vapr_6,filename=paste("rdatamaps/",uname,"/",resol,"/vapr_6.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(vapr_6)
  
  vapr_7<-crop(raster(paste("rdatamaps/world/",resol,"/vapr_7.tif",sep="")),molde)
  extent(vapr_7)<-moldextent
  vapr_7<-mask(vapr_7,molde)
  writeRaster(vapr_7,filename=paste("rdatamaps/",uname,"/",resol,"/vapr_7.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(vapr_7)
  
  vapr_8<-crop(raster(paste("rdatamaps/world/",resol,"/vapr_8.tif",sep="")),molde)
  extent(vapr_8)<-moldextent
  vapr_8<-mask(vapr_8,molde)
  writeRaster(vapr_8,filename=paste("rdatamaps/",uname,"/",resol,"/vapr_8.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(vapr_8)
  
  vapr_9<-crop(raster(paste("rdatamaps/world/",resol,"/vapr_9.tif",sep="")),molde)
  extent(vapr_9)<-moldextent
  vapr_9<-mask(vapr_9,molde)
  writeRaster(vapr_9,filename=paste("rdatamaps/",uname,"/",resol,"/vapr_9.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(vapr_9)
  
  vapr_10<-crop(raster(paste("rdatamaps/world/",resol,"/vapr_10.tif",sep="")),molde)
  extent(vapr_10)<-moldextent
  vapr_10<-mask(vapr_10,molde)
  writeRaster(vapr_10,filename=paste("rdatamaps/",uname,"/",resol,"/vapr_10.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(vapr_10)
  
  vapr_11<-crop(raster(paste("rdatamaps/world/",resol,"/vapr_11.tif",sep="")),molde)
  extent(vapr_11)<-moldextent
  vapr_11<-mask(vapr_11,molde)
  writeRaster(vapr_11,filename=paste("rdatamaps/",uname,"/",resol,"/vapr_11.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(vapr_11)
  
  vapr_12<-crop(raster(paste("rdatamaps/world/",resol,"/vapr_12.tif",sep="")),molde)
  extent(vapr_12)<-moldextent
  vapr_12<-mask(vapr_12,molde)
  writeRaster(vapr_12,filename=paste("rdatamaps/",uname,"/",resol,"/vapr_12.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(vapr_12)
  
  vapr_annual<-crop(raster(paste("rdatamaps/world/",resol,"/vapr_annual.tif",sep="")),molde)
  extent(vapr_annual)<-moldextent
  vapr_annual<-mask(vapr_annual,molde)
  writeRaster(vapr_annual,filename=paste("rdatamaps/",uname,"/",resol,"/vapr_annual.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(vapr_annual)
  
  #SOILGRIDS
  
  #AWC1
  t_awc1<-crop(raster(paste("rdatamaps/world/",resol,"/t_awc1.tif",sep="")),molde)
  extent(t_awc1)<-moldextent
  t_awc1<-mask(t_awc1,molde)
  
  s_awc1<-crop(raster(paste("rdatamaps/world/",resol,"/s_awc1.tif",sep="")),molde)
  extent(s_awc1)<-moldextent
  s_awc1<-mask(s_awc1,molde)
  
  writeRaster(t_awc1,filename=paste("rdatamaps/",uname,"/",resol,"/t_awc1.tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(s_awc1,filename=paste("rdatamaps/",uname,"/",resol,"/s_awc1.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_awc1)
  rm(s_awc1)
  
  
  #AWC2
  t_awc2<-crop(raster(paste("rdatamaps/world/",resol,"/t_awc2.tif",sep="")),molde)
  extent(t_awc2)<-moldextent
  t_awc2<-mask(t_awc2,molde)
  
  s_awc2<-crop(raster(paste("rdatamaps/world/",resol,"/s_awc2.tif",sep="")),molde)
  extent(s_awc2)<-moldextent
  s_awc2<-mask(s_awc2,molde)
  
  writeRaster(t_awc2,filename=paste("rdatamaps/",uname,"/",resol,"/t_awc2.tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(s_awc2,filename=paste("rdatamaps/",uname,"/",resol,"/s_awc2.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_awc2)
  rm(t_awc2)
  
  #AWC3
  t_awc3<-crop(raster(paste("rdatamaps/world/",resol,"/t_awc3.tif",sep="")),molde)
  extent(t_awc3)<-moldextent
  t_awc3<-mask(t_awc3,molde)
  
  s_awc3<-crop(raster(paste("rdatamaps/world/",resol,"/s_awc3.tif",sep="")),molde)
  extent(s_awc3)<-moldextent
  s_awc3<-mask(s_awc3,molde)
  
  writeRaster(t_awc3,filename=paste("rdatamaps/",uname,"/",resol,"/t_awc3.tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(s_awc3,filename=paste("rdatamaps/",uname,"/",resol,"/s_awc3.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_awc3)
  rm(t_awc3)
  
  #AWCts
  t_awcts<-crop(raster(paste("rdatamaps/world/",resol,"/t_awcts.tif",sep="")),molde)
  extent(t_awcts)<-moldextent
  t_awcts<-mask(t_awcts,molde)
  
  s_awcts<-crop(raster(paste("rdatamaps/world/",resol,"/s_awcts.tif",sep="")),molde)
  extent(s_awcts)<-moldextent
  s_awcts<-mask(s_awcts,molde)
  
  writeRaster(t_awcts,filename=paste("rdatamaps/",uname,"/",resol,"/t_awcts.tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(s_awcts,filename=paste("rdatamaps/",uname,"/",resol,"/s_awcts.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_awcts)
  rm(t_awcts)
  
  #Depth rock
  depth_rock<-crop(raster(paste("rdatamaps/world/",resol,"/depth_rock.tif",sep="")),molde)
  extent(depth_rock)<-moldextent
  depth_rock<-mask(depth_rock,molde)
  writeRaster(depth_rock,filename=paste("rdatamaps/",uname,"/",resol,"/depth_rock.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(depth_rock)
  
  #R horizon
  r_horizon<-crop(raster(paste("rdatamaps/world/",resol,"/r_horizon.tif",sep="")),molde)
  extent(r_horizon)<-moldextent
  r_horizon<-mask(r_horizon,molde)
  writeRaster(r_horizon,filename=paste("rdatamaps/",uname,"/",resol,"/r_horizon.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(r_horizon)
  
  #Bulk density
  t_bulk_dens<-crop(raster(paste("rdatamaps/world/",resol,"/t_bulk_dens.tif",sep="")),molde)
  extent(t_bulk_dens)<-moldextent
  t_bulk_dens<-mask(t_bulk_dens,molde)
  
  s_bulk_dens<-crop(raster(paste("rdatamaps/world/",resol,"/s_bulk_dens.tif",sep="")),molde)
  extent(s_bulk_dens)<-moldextent
  s_bulk_dens<-mask(s_bulk_dens,molde)
  
  writeRaster(t_bulk_dens,filename=paste("rdatamaps/",uname,"/",resol,"/t_bulk_dens.tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(s_bulk_dens,filename=paste("rdatamaps/",uname,"/",resol,"/s_bulk_dens.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_bulk_dens)
  rm(t_bulk_dens)
  
  #Cec soil
  t_cecsol<-crop(raster(paste("rdatamaps/world/",resol,"/t_cecsol.tif",sep="")),molde)
  extent(t_cecsol)<-moldextent
  t_cecsol<-mask(t_cecsol,molde)
  
  s_cecsol<-crop(raster(paste("rdatamaps/world/",resol,"/s_cecsol.tif",sep="")),molde)
  extent(s_cecsol)<-moldextent
  s_cecsol<-mask(s_cecsol,molde)
  
  writeRaster(t_cecsol,filename=paste("rdatamaps/",uname,"/",resol,"/t_cecsol.tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(s_cecsol,filename=paste("rdatamaps/",uname,"/",resol,"/s_cecsol.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_cecsol)
  rm(t_cecsol)
  
  #Clay content
  t_clay_cont<-crop(raster(paste("rdatamaps/world/",resol,"/t_clay_cont.tif",sep="")),molde)
  extent(t_clay_cont)<-moldextent
  t_clay_cont<-mask(t_clay_cont,molde)
  
  s_clay_cont<-crop(raster(paste("rdatamaps/world/",resol,"/s_clay_cont.tif",sep="")),molde)
  extent(s_clay_cont)<-moldextent
  s_clay_cont<-mask(s_clay_cont,molde)
  
  writeRaster(t_clay_cont,filename=paste("rdatamaps/",uname,"/",resol,"/t_clay_cont.tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(s_clay_cont,filename=paste("rdatamaps/",uname,"/",resol,"/s_clay_cont.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_clay_cont)
  rm(t_clay_cont)
  
  #Coarse frag
  t_coarse_frag<-crop(raster(paste("rdatamaps/world/",resol,"/t_coarse_frag.tif",sep="")),molde)
  extent(t_coarse_frag)<-moldextent
  t_coarse_frag<-mask(t_coarse_frag,molde)
  
  s_coarse_frag<-crop(raster(paste("rdatamaps/world/",resol,"/s_coarse_frag.tif",sep="")),molde)
  extent(s_coarse_frag)<-moldextent
  s_coarse_frag<-mask(s_coarse_frag,molde)
  
  writeRaster(t_coarse_frag,filename=paste("rdatamaps/",uname,"/",resol,"/t_coarse_frag.tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(s_coarse_frag,filename=paste("rdatamaps/",uname,"/",resol,"/s_coarse_frag.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_coarse_frag)
  rm(t_coarse_frag)
  
  #OC density
  t_oc_dens<-crop(raster(paste("rdatamaps/world/",resol,"/t_oc_dens.tif",sep="")),molde)
  extent(t_oc_dens)<-moldextent
  t_oc_dens<-mask(t_oc_dens,molde)
  
  s_oc_dens<-crop(raster(paste("rdatamaps/world/",resol,"/s_oc_dens.tif",sep="")),molde)
  extent(s_oc_dens)<-moldextent
  s_oc_dens<-mask(s_oc_dens,molde)
  
  writeRaster(t_oc_dens,filename=paste("rdatamaps/",uname,"/",resol,"/t_oc_dens.tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(s_oc_dens,filename=paste("rdatamaps/",uname,"/",resol,"/s_oc_dens.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_oc_dens)
  rm(t_oc_dens)
  
  #OC stock
  t_oc_stock<-crop(raster(paste("rdatamaps/world/",resol,"/t_oc_stock.tif",sep="")),molde)
  extent(t_oc_stock)<-moldextent
  t_oc_stock<-mask(t_oc_stock,molde)
  writeRaster(t_oc_stock,filename=paste("rdatamaps/",uname,"/",resol,"/t_oc_stock.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_oc_stock)
  
  s_oc_stock<-crop(raster(paste("rdatamaps/world/",resol,"/s_oc_stock.tif",sep="")),molde)
  extent(s_oc_stock)<-moldextent
  s_oc_stock<-mask(s_oc_stock,molde)
  writeRaster(s_oc_stock,filename=paste("rdatamaps/",uname,"/",resol,"/s_oc_stock.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_oc_stock)
  
  #OC content
  t_oc_cont<-crop(raster(paste("rdatamaps/world/",resol,"/t_oc_cont.tif",sep="")),molde)
  extent(t_oc_cont)<-moldextent
  t_oc_cont<-mask(t_oc_cont,molde)
  
  s_oc_cont<-crop(raster(paste("rdatamaps/world/",resol,"/s_oc_cont.tif",sep="")),molde)
  extent(s_oc_cont)<-moldextent
  s_oc_cont<-mask(s_oc_cont,molde)
  
  writeRaster(t_oc_cont,filename=paste("rdatamaps/",uname,"/",resol,"/t_oc_cont.tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(s_oc_cont,filename=paste("rdatamaps/",uname,"/",resol,"/s_oc_cont.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_oc_cont)
  rm(t_oc_cont)
  
  #Ph h2O
  t_ph_hox<-crop(raster(paste("rdatamaps/world/",resol,"/t_ph_hox.tif",sep="")),molde)
  extent(t_ph_hox)<-moldextent
  t_ph_hox<-mask(t_ph_hox,molde)
  
  s_ph_hox<-crop(raster(paste("rdatamaps/world/",resol,"/s_ph_hox.tif",sep="")),molde)
  extent(s_ph_hox)<-moldextent
  s_ph_hox<-mask(s_ph_hox,molde)
  
  writeRaster(t_ph_hox,filename=paste("rdatamaps/",uname,"/",resol,"/t_ph_hox.tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(s_ph_hox,filename=paste("rdatamaps/",uname,"/",resol,"/s_ph_hox.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_ph_hox)
  rm(t_ph_hox)
  
  #Ph kcl
  t_ph_kcl<-crop(raster(paste("rdatamaps/world/",resol,"/t_ph_kcl.tif",sep="")),molde)
  extent(t_ph_kcl)<-moldextent
  t_ph_kcl<-mask(t_ph_kcl,molde)
  
  s_ph_kcl<-crop(raster(paste("rdatamaps/world/",resol,"/s_ph_kcl.tif",sep="")),molde)
  extent(s_ph_kcl)<-moldextent
  s_ph_kcl<-mask(s_ph_kcl,molde)
  
  writeRaster(t_ph_kcl,filename=paste("rdatamaps/",uname,"/",resol,"/t_ph_kcl.tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(s_ph_kcl,filename=paste("rdatamaps/",uname,"/",resol,"/s_ph_kcl.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_ph_kcl)
  rm(t_ph_kcl)
  
  #Sodicity
  sodicity<-crop(raster(paste("rdatamaps/world/",resol,"/sodicity.tif",sep="")),molde)
  extent(sodicity)<-moldextent
  sodicity<-mask(sodicity,molde)
  writeRaster(sodicity,filename=paste("rdatamaps/",uname,"/",resol,"/sodicity.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(sodicity)
  
  #Silt content
  t_silt_cont<-crop(raster(paste("rdatamaps/world/",resol,"/t_silt_cont.tif",sep="")),molde)
  extent(t_silt_cont)<-moldextent
  t_silt_cont<-mask(t_silt_cont,molde)
  
  s_silt_cont<-crop(raster(paste("rdatamaps/world/",resol,"/s_silt_cont.tif",sep="")),molde)
  extent(s_silt_cont)<-moldextent
  s_silt_cont<-mask(s_silt_cont,molde)
  
  writeRaster(t_silt_cont,filename=paste("rdatamaps/",uname,"/",resol,"/t_silt_cont.tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(s_silt_cont,filename=paste("rdatamaps/",uname,"/",resol,"/s_silt_cont.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_silt_cont)
  rm(s_silt_cont)
  
  #Sand content
  t_sand_cont<-crop(raster(paste("rdatamaps/world/",resol,"/t_sand_cont.tif",sep="")),molde)
  extent(t_sand_cont)<-moldextent
  t_sand_cont<-mask(t_sand_cont,molde)
  
  s_sand_cont<-crop(raster(paste("rdatamaps/world/",resol,"/s_sand_cont.tif",sep="")),molde)
  extent(s_sand_cont)<-moldextent
  s_sand_cont<-mask(s_sand_cont,molde)
  
  writeRaster(t_sand_cont,filename=paste("rdatamaps/",uname,"/",resol,"/t_sand_cont.tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(s_sand_cont,filename=paste("rdatamaps/",uname,"/",resol,"/s_sand_cont.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_sand_cont)
  rm(t_sand_cont)
  
  #Soil water capacity
  t_soilwater_cap<-crop(raster(paste("rdatamaps/world/",resol,"/t_soilwater_cap.tif",sep="")),molde)
  extent(t_soilwater_cap)<-moldextent
  t_soilwater_cap<-mask(t_soilwater_cap,molde)
  
  s_soilwater_cap<-crop(raster(paste("rdatamaps/world/",resol,"/s_soilwater_cap.tif",sep="")),molde)
  extent(s_soilwater_cap)<-moldextent
  s_soilwater_cap<-mask(s_soilwater_cap,molde)
  
  writeRaster(t_soilwater_cap,filename=paste("rdatamaps/",uname,"/",resol,"/t_soilwater_cap.tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(s_soilwater_cap,filename=paste("rdatamaps/",uname,"/",resol,"/s_soilwater_cap.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_soilwater_cap)
  rm(t_soilwater_cap)
  
  
  #puntos
  bioclimn<-ncell(tmean_12)
  bioclimp<-xyFromCell(tmean_12,c(1:bioclimn))
  
  bioclime<-extract(tmean_12,bioclimp)
  
  bioclim<-cbind(bioclimp,bioclime)
  bioclim<-na.omit(bioclim)
  bioclim<-bioclim[,c(1,2)]
  
  POINTID<-as.vector(1:length(bioclim[,1]))
  
  bioclimp<-data.frame(cbind(POINTID,bioclim[,c(1,2)]))
  colnames(bioclimp)[2]<-"POINT_X"
  colnames(bioclimp)[3]<-"POINT_Y"
  puntos<-bioclimp
  
  setwd(paste(ruta,"/rdatapoints",sep=""))
  dir.create(as.vector(paste(uname)))
  setwd(paste(ruta,"/rdatapoints/",uname,sep=""))
  dir.create(as.vector(paste(resol)))
  save(puntos,file=paste(resol,"/","base",resol,".RData",sep=""))
  rm(tmean_12)
  rm(molde)
}

if(cropway=="polygon"){
  #Entrada raster 
  setwd(paste(ruta))
  molde<-raster(paste("rdatamaps/world/",resol,"/alt.tif",sep=""))
  crs(molde)<-CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  #Entrada polygon
  areas<-readOGR(paste("Pasaporte/",shapefile,".shp",sep=""))
  areas<-SpatialPolygons(areas@polygons,proj4string=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") )
  molde<-crop(molde,areas)
  molde<-mask(molde,areas)
  moldextent<-extent(molde)
  
  
  #Corte
  bio_1<-crop(raster(paste("rdatamaps/world/",resol,"/bio_1.tif",sep="")),molde)
  extent(bio_1)<-moldextent
  bio_1<-mask(bio_1,molde)
  writeRaster(bio_1,filename=paste("rdatamaps/",uname,"/",resol,"/bio_1.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_1)
  
  bio_2<-crop(raster(paste("rdatamaps/world/",resol,"/bio_2.tif",sep="")),molde)
  extent(bio_2)<-moldextent
  bio_2<-mask(bio_2,molde)
  writeRaster(bio_2,filename=paste("rdatamaps/",uname,"/",resol,"/bio_2.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_2)
  
  bio_3<-crop(raster(paste("rdatamaps/world/",resol,"/bio_3.tif",sep="")),molde)
  extent(bio_3)<-moldextent
  bio_3<-mask(bio_3,molde)
  writeRaster(bio_3,filename=paste("rdatamaps/",uname,"/",resol,"/bio_3.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_3)
  
  bio_4<-crop(raster(paste("rdatamaps/world/",resol,"/bio_4.tif",sep="")),molde)
  extent(bio_4)<-moldextent
  bio_4<-mask(bio_4,molde)
  writeRaster(bio_4,filename=paste("rdatamaps/",uname,"/",resol,"/bio_4.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_4)
  
  bio_5<-crop(raster(paste("rdatamaps/world/",resol,"/bio_5.tif",sep="")),molde)
  extent(bio_5)<-moldextent
  bio_5<-mask(bio_5,molde)
  writeRaster(bio_5,filename=paste("rdatamaps/",uname,"/",resol,"/bio_5.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_5)
  
  bio_6<-crop(raster(paste("rdatamaps/world/",resol,"/bio_6.tif",sep="")),molde)
  extent(bio_6)<-moldextent
  bio_6<-mask(bio_6,molde)
  writeRaster(bio_6,filename=paste("rdatamaps/",uname,"/",resol,"/bio_6.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_6)
  
  bio_7<-crop(raster(paste("rdatamaps/world/",resol,"/bio_7.tif",sep="")),molde)
  extent(bio_7)<-moldextent
  bio_7<-mask(bio_7,molde)
  writeRaster(bio_7,filename=paste("rdatamaps/",uname,"/",resol,"/bio_7.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_7)
  
  bio_8<-crop(raster(paste("rdatamaps/world/",resol,"/bio_8.tif",sep="")),molde)
  extent(bio_8)<-moldextent
  bio_8<-mask(bio_8,molde)
  writeRaster(bio_8,filename=paste("rdatamaps/",uname,"/",resol,"/bio_8.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_8)
  
  bio_9<-crop(raster(paste("rdatamaps/world/",resol,"/bio_9.tif",sep="")),molde)
  extent(bio_9)<-moldextent
  bio_9<-mask(bio_9,molde)
  writeRaster(bio_9,filename=paste("rdatamaps/",uname,"/",resol,"/bio_9.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_9)
  
  bio_10<-crop(raster(paste("rdatamaps/world/",resol,"/bio_10.tif",sep="")),molde)
  extent(bio_10)<-moldextent
  bio_10<-mask(bio_10,molde)
  writeRaster(bio_10,filename=paste("rdatamaps/",uname,"/",resol,"/bio_10.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_10)
  
  bio_11<-crop(raster(paste("rdatamaps/world/",resol,"/bio_11.tif",sep="")),molde)
  extent(bio_11)<-moldextent
  bio_11<-mask(bio_11,molde)
  writeRaster(bio_11,filename=paste("rdatamaps/",uname,"/",resol,"/bio_11.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_11)
  
  bio_12<-crop(raster(paste("rdatamaps/world/",resol,"/bio_12.tif",sep="")),molde)
  extent(bio_12)<-moldextent
  bio_12<-mask(bio_12,molde)
  writeRaster(bio_12,filename=paste("rdatamaps/",uname,"/",resol,"/bio_12.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_12)
  
  bio_13<-crop(raster(paste("rdatamaps/world/",resol,"/bio_13.tif",sep="")),molde)
  extent(bio_13)<-moldextent
  bio_13<-mask(bio_13,molde)
  writeRaster(bio_13,filename=paste("rdatamaps/",uname,"/",resol,"/bio_13.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_13)
  
  bio_14<-crop(raster(paste("rdatamaps/world/",resol,"/bio_14.tif",sep="")),molde)
  extent(bio_14)<-moldextent
  bio_14<-mask(bio_14,molde)
  writeRaster(bio_14,filename=paste("rdatamaps/",uname,"/",resol,"/bio_14.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_14)
  
  bio_15<-crop(raster(paste("rdatamaps/world/",resol,"/bio_15.tif",sep="")),molde)
  extent(bio_15)<-moldextent
  bio_15<-mask(bio_15,molde)
  writeRaster(bio_15,filename=paste("rdatamaps/",uname,"/",resol,"/bio_15.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_15)
  
  bio_16<-crop(raster(paste("rdatamaps/world/",resol,"/bio_16.tif",sep="")),molde)
  extent(bio_16)<-moldextent
  bio_16<-mask(bio_16,molde)
  writeRaster(bio_16,filename=paste("rdatamaps/",uname,"/",resol,"/bio_16.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_16)
  
  bio_17<-crop(raster(paste("rdatamaps/world/",resol,"/bio_17.tif",sep="")),molde)
  extent(bio_17)<-moldextent
  bio_17<-mask(bio_17,molde)
  writeRaster(bio_17,filename=paste("rdatamaps/",uname,"/",resol,"/bio_17.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_17)
  
  bio_18<-crop(raster(paste("rdatamaps/world/",resol,"/bio_18.tif",sep="")),molde)
  extent(bio_18)<-moldextent
  bio_18<-mask(bio_18,molde)
  writeRaster(bio_18,filename=paste("rdatamaps/",uname,"/",resol,"/bio_18.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_18)
  
  bio_19<-crop(raster(paste("rdatamaps/world/",resol,"/bio_19.tif",sep="")),molde)
  extent(bio_19)<-moldextent
  bio_19<-mask(bio_19,molde)
  writeRaster(bio_19,filename=paste("rdatamaps/",uname,"/",resol,"/bio_19.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_19)
  
  
  prec_1<-crop(raster(paste("rdatamaps/world/",resol,"/prec_1.tif",sep="")),molde)
  extent(prec_1)<-moldextent
  prec_1<-mask(prec_1,molde)
  writeRaster(prec_1,filename=paste("rdatamaps/",uname,"/",resol,"/prec_1.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(prec_1)
  
  prec_2<-crop(raster(paste("rdatamaps/world/",resol,"/prec_2.tif",sep="")),molde)
  extent(prec_2)<-moldextent
  prec_2<-mask(prec_2,molde)
  writeRaster(prec_2,filename=paste("rdatamaps/",uname,"/",resol,"/prec_2.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(prec_2)
  
  prec_3<-crop(raster(paste("rdatamaps/world/",resol,"/prec_3.tif",sep="")),molde)
  extent(prec_3)<-moldextent
  prec_3<-mask(prec_3,molde)
  writeRaster(prec_3,filename=paste("rdatamaps/",uname,"/",resol,"/prec_3.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(prec_3)
  
  prec_4<-crop(raster(paste("rdatamaps/world/",resol,"/prec_4.tif",sep="")),molde)
  extent(prec_4)<-moldextent
  prec_4<-mask(prec_4,molde)
  writeRaster(prec_4,filename=paste("rdatamaps/",uname,"/",resol,"/prec_4.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(prec_4)
  
  prec_5<-crop(raster(paste("rdatamaps/world/",resol,"/prec_5.tif",sep="")),molde)
  extent(prec_5)<-moldextent
  prec_5<-mask(prec_5,molde)
  writeRaster(prec_5,filename=paste("rdatamaps/",uname,"/",resol,"/prec_5.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(prec_5)
  
  prec_6<-crop(raster(paste("rdatamaps/world/",resol,"/prec_6.tif",sep="")),molde)
  extent(prec_6)<-moldextent
  prec_6<-mask(prec_6,molde)
  writeRaster(prec_6,filename=paste("rdatamaps/",uname,"/",resol,"/prec_6.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(prec_6)
  
  prec_7<-crop(raster(paste("rdatamaps/world/",resol,"/prec_7.tif",sep="")),molde)
  extent(prec_7)<-moldextent
  prec_7<-mask(prec_7,molde)
  writeRaster(prec_7,filename=paste("rdatamaps/",uname,"/",resol,"/prec_7.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(prec_7)
  
  prec_8<-crop(raster(paste("rdatamaps/world/",resol,"/prec_8.tif",sep="")),molde)
  extent(prec_8)<-moldextent
  prec_8<-mask(prec_8,molde)
  writeRaster(prec_8,filename=paste("rdatamaps/",uname,"/",resol,"/prec_8.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(prec_8)
  
  prec_9<-crop(raster(paste("rdatamaps/world/",resol,"/prec_9.tif",sep="")),molde)
  extent(prec_9)<-moldextent
  prec_9<-mask(prec_9,molde)
  writeRaster(prec_9,filename=paste("rdatamaps/",uname,"/",resol,"/prec_9.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(prec_9)
  
  prec_10<-crop(raster(paste("rdatamaps/world/",resol,"/prec_10.tif",sep="")),molde)
  extent(prec_10)<-moldextent
  prec_10<-mask(prec_10,molde)
  writeRaster(prec_10,filename=paste("rdatamaps/",uname,"/",resol,"/prec_10.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(prec_10)
  
  prec_11<-crop(raster(paste("rdatamaps/world/",resol,"/prec_11.tif",sep="")),molde)
  extent(prec_11)<-moldextent
  prec_11<-mask(prec_11,molde)
  writeRaster(prec_11,filename=paste("rdatamaps/",uname,"/",resol,"/prec_11.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(prec_11)
  
  prec_12<-crop(raster(paste("rdatamaps/world/",resol,"/prec_12.tif",sep="")),molde)
  extent(prec_12)<-moldextent
  prec_12<-mask(prec_12,molde)
  writeRaster(prec_12,filename=paste("rdatamaps/",uname,"/",resol,"/prec_12.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(prec_12)
  
  tmax_1<-crop(raster(paste("rdatamaps/world/",resol,"/tmax_1.tif",sep="")),molde)
  extent(tmax_1)<-moldextent
  tmax_1<-mask(tmax_1,molde)
  writeRaster(tmax_1,filename=paste("rdatamaps/",uname,"/",resol,"/tmax_1.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmax_1)
  
  tmax_2<-crop(raster(paste("rdatamaps/world/",resol,"/tmax_2.tif",sep="")),molde)
  extent(tmax_2)<-moldextent
  tmax_2<-mask(tmax_2,molde)
  writeRaster(tmax_2,filename=paste("rdatamaps/",uname,"/",resol,"/tmax_2.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmax_2)
  
  tmax_3<-crop(raster(paste("rdatamaps/world/",resol,"/tmax_3.tif",sep="")),molde)
  extent(tmax_3)<-moldextent
  tmax_3<-mask(tmax_3,molde)
  writeRaster(tmax_3,filename=paste("rdatamaps/",uname,"/",resol,"/tmax_3.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmax_3)
  
  tmax_4<-crop(raster(paste("rdatamaps/world/",resol,"/tmax_4.tif",sep="")),molde)
  extent(tmax_4)<-moldextent
  tmax_4<-mask(tmax_4,molde)
  writeRaster(tmax_4,filename=paste("rdatamaps/",uname,"/",resol,"/tmax_4.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmax_4)
  
  tmax_5<-crop(raster(paste("rdatamaps/world/",resol,"/tmax_5.tif",sep="")),molde)
  extent(tmax_5)<-moldextent
  tmax_5<-mask(tmax_5,molde)
  writeRaster(tmax_5,filename=paste("rdatamaps/",uname,"/",resol,"/tmax_5.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmax_5)
  
  tmax_6<-crop(raster(paste("rdatamaps/world/",resol,"/tmax_6.tif",sep="")),molde)
  extent(tmax_6)<-moldextent
  tmax_6<-mask(tmax_6,molde)
  writeRaster(tmax_6,filename=paste("rdatamaps/",uname,"/",resol,"/tmax_6.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmax_6)
  
  tmax_7<-crop(raster(paste("rdatamaps/world/",resol,"/tmax_7.tif",sep="")),molde)
  extent(tmax_7)<-moldextent
  tmax_7<-mask(tmax_7,molde)
  writeRaster(tmax_7,filename=paste("rdatamaps/",uname,"/",resol,"/tmax_7.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmax_7)
  
  tmax_8<-crop(raster(paste("rdatamaps/world/",resol,"/tmax_8.tif",sep="")),molde)
  extent(tmax_8)<-moldextent
  tmax_8<-mask(tmax_8,molde)
  writeRaster(tmax_8,filename=paste("rdatamaps/",uname,"/",resol,"/tmax_8.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmax_8)
  
  tmax_9<-crop(raster(paste("rdatamaps/world/",resol,"/tmax_9.tif",sep="")),molde)
  extent(tmax_9)<-moldextent
  tmax_9<-mask(tmax_9,molde)
  writeRaster(tmax_9,filename=paste("rdatamaps/",uname,"/",resol,"/tmax_9.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmax_9)
  
  tmax_10<-crop(raster(paste("rdatamaps/world/",resol,"/tmax_10.tif",sep="")),molde)
  extent(tmax_10)<-moldextent
  tmax_10<-mask(tmax_10,molde)
  writeRaster(tmax_10,filename=paste("rdatamaps/",uname,"/",resol,"/tmax_10.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmax_10)
  
  tmax_11<-crop(raster(paste("rdatamaps/world/",resol,"/tmax_11.tif",sep="")),molde)
  extent(tmax_11)<-moldextent
  tmax_11<-mask(tmax_11,molde)
  writeRaster(tmax_11,filename=paste("rdatamaps/",uname,"/",resol,"/tmax_11.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmax_11)
  
  tmax_12<-crop(raster(paste("rdatamaps/world/",resol,"/tmax_12.tif",sep="")),molde)
  extent(tmax_12)<-moldextent
  tmax_12<-mask(tmax_12,molde)
  writeRaster(tmax_12,filename=paste("rdatamaps/",uname,"/",resol,"/tmax_12.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmax_12)
  
  tmin_1<-crop(raster(paste("rdatamaps/world/",resol,"/tmin_1.tif",sep="")),molde)
  extent(tmin_1)<-moldextent
  tmin_1<-mask(tmin_1,molde)
  writeRaster(tmin_1,filename=paste("rdatamaps/",uname,"/",resol,"/tmin_1.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmin_1)
  
  tmin_2<-crop(raster(paste("rdatamaps/world/",resol,"/tmin_2.tif",sep="")),molde)
  extent(tmin_2)<-moldextent
  tmin_2<-mask(tmin_2,molde)
  writeRaster(tmin_2,filename=paste("rdatamaps/",uname,"/",resol,"/tmin_2.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmin_2)
  
  tmin_3<-crop(raster(paste("rdatamaps/world/",resol,"/tmin_3.tif",sep="")),molde)
  extent(tmin_3)<-moldextent
  tmin_3<-mask(tmin_3,molde)
  writeRaster(tmin_3,filename=paste("rdatamaps/",uname,"/",resol,"/tmin_3.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmin_3)
  
  tmin_4<-crop(raster(paste("rdatamaps/world/",resol,"/tmin_4.tif",sep="")),molde)
  extent(tmin_4)<-moldextent
  tmin_4<-mask(tmin_4,molde)
  writeRaster(tmin_4,filename=paste("rdatamaps/",uname,"/",resol,"/tmin_4.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmin_4)
  
  tmin_5<-crop(raster(paste("rdatamaps/world/",resol,"/tmin_5.tif",sep="")),molde)
  extent(tmin_5)<-moldextent
  tmin_5<-mask(tmin_5,molde)
  writeRaster(tmin_5,filename=paste("rdatamaps/",uname,"/",resol,"/tmin_5.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmin_5)
  
  tmin_6<-crop(raster(paste("rdatamaps/world/",resol,"/tmin_6.tif",sep="")),molde)
  extent(tmin_6)<-moldextent
  tmin_6<-mask(tmin_6,molde)
  writeRaster(tmin_6,filename=paste("rdatamaps/",uname,"/",resol,"/tmin_6.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmin_6)
  
  tmin_7<-crop(raster(paste("rdatamaps/world/",resol,"/tmin_7.tif",sep="")),molde)
  extent(tmin_7)<-moldextent
  tmin_7<-mask(tmin_7,molde)
  writeRaster(tmin_7,filename=paste("rdatamaps/",uname,"/",resol,"/tmin_7.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmin_7)
  
  tmin_8<-crop(raster(paste("rdatamaps/world/",resol,"/tmin_8.tif",sep="")),molde)
  extent(tmin_8)<-moldextent
  tmin_8<-mask(tmin_8,molde)
  writeRaster(tmin_8,filename=paste("rdatamaps/",uname,"/",resol,"/tmin_8.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmin_8)
  
  tmin_9<-crop(raster(paste("rdatamaps/world/",resol,"/tmin_9.tif",sep="")),molde)
  extent(tmin_9)<-moldextent
  tmin_9<-mask(tmin_9,molde)
  writeRaster(tmin_9,filename=paste("rdatamaps/",uname,"/",resol,"/tmin_9.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmin_9)
  
  tmin_10<-crop(raster(paste("rdatamaps/world/",resol,"/tmin_10.tif",sep="")),molde)
  extent(tmin_10)<-moldextent
  tmin_10<-mask(tmin_10,molde)
  writeRaster(tmin_10,filename=paste("rdatamaps/",uname,"/",resol,"/tmin_10.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmin_10)
  
  tmin_11<-crop(raster(paste("rdatamaps/world/",resol,"/tmin_11.tif",sep="")),molde)
  extent(tmin_11)<-moldextent
  tmin_11<-mask(tmin_11,molde)
  writeRaster(tmin_11,filename=paste("rdatamaps/",uname,"/",resol,"/tmin_11.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmin_11)
  
  tmin_12<-crop(raster(paste("rdatamaps/world/",resol,"/tmin_12.tif",sep="")),molde)
  extent(tmin_12)<-moldextent
  tmin_12<-mask(tmin_12,molde)
  writeRaster(tmin_12,filename=paste("rdatamaps/",uname,"/",resol,"/tmin_12.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmin_12)
  
  tmean_1<-crop(raster(paste("rdatamaps/world/",resol,"/tmean_1.tif",sep="")),molde)
  extent(tmean_1)<-moldextent
  tmean_1<-mask(tmean_1,molde)
  writeRaster(tmean_1,filename=paste("rdatamaps/",uname,"/",resol,"/tmean_1.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmean_1)
  
  tmean_2<-crop(raster(paste("rdatamaps/world/",resol,"/tmean_2.tif",sep="")),molde)
  extent(tmean_2)<-moldextent
  tmean_2<-mask(tmean_2,molde)
  writeRaster(tmean_2,filename=paste("rdatamaps/",uname,"/",resol,"/tmean_2.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmean_2)
  
  tmean_3<-crop(raster(paste("rdatamaps/world/",resol,"/tmean_3.tif",sep="")),molde)
  extent(tmean_3)<-moldextent
  tmean_3<-mask(tmean_3,molde)
  writeRaster(tmean_3,filename=paste("rdatamaps/",uname,"/",resol,"/tmean_3.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmean_3)
  
  tmean_4<-crop(raster(paste("rdatamaps/world/",resol,"/tmean_4.tif",sep="")),molde)
  extent(tmean_4)<-moldextent
  tmean_4<-mask(tmean_4,molde)
  writeRaster(tmean_4,filename=paste("rdatamaps/",uname,"/",resol,"/tmean_4.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmean_4)
  
  tmean_5<-crop(raster(paste("rdatamaps/world/",resol,"/tmean_5.tif",sep="")),molde)
  extent(tmean_5)<-moldextent
  tmean_5<-mask(tmean_5,molde)
  writeRaster(tmean_5,filename=paste("rdatamaps/",uname,"/",resol,"/tmean_5.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmean_5)
  
  tmean_6<-crop(raster(paste("rdatamaps/world/",resol,"/tmean_6.tif",sep="")),molde)
  extent(tmean_6)<-moldextent
  tmean_6<-mask(tmean_6,molde)
  writeRaster(tmean_6,filename=paste("rdatamaps/",uname,"/",resol,"/tmean_6.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmean_6)
  
  tmean_7<-crop(raster(paste("rdatamaps/world/",resol,"/tmean_7.tif",sep="")),molde)
  extent(tmean_7)<-moldextent
  tmean_7<-mask(tmean_7,molde)
  writeRaster(tmean_7,filename=paste("rdatamaps/",uname,"/",resol,"/tmean_7.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmean_7)
  
  tmean_8<-crop(raster(paste("rdatamaps/world/",resol,"/tmean_8.tif",sep="")),molde)
  extent(tmean_8)<-moldextent
  tmean_8<-mask(tmean_8,molde)
  writeRaster(tmean_8,filename=paste("rdatamaps/",uname,"/",resol,"/tmean_8.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmean_8)
  
  tmean_9<-crop(raster(paste("rdatamaps/world/",resol,"/tmean_9.tif",sep="")),molde)
  extent(tmean_9)<-moldextent
  tmean_9<-mask(tmean_9,molde)
  writeRaster(tmean_9,filename=paste("rdatamaps/",uname,"/",resol,"/tmean_9.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmean_9)
  
  tmean_10<-crop(raster(paste("rdatamaps/world/",resol,"/tmean_10.tif",sep="")),molde)
  extent(tmean_10)<-moldextent
  tmean_10<-mask(tmean_10,molde)
  writeRaster(tmean_10,filename=paste("rdatamaps/",uname,"/",resol,"/tmean_10.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmean_10)
  
  tmean_11<-crop(raster(paste("rdatamaps/world/",resol,"/tmean_11.tif",sep="")),molde)
  extent(tmean_11)<-moldextent
  tmean_11<-mask(tmean_11,molde)
  writeRaster(tmean_11,filename=paste("rdatamaps/",uname,"/",resol,"/tmean_11.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmean_11)
  
  tmean_12<-crop(raster(paste("rdatamaps/world/",resol,"/tmean_12.tif",sep="")),molde)
  extent(tmean_12)<-moldextent
  tmean_12<-mask(tmean_12,molde)
  writeRaster(tmean_12,filename=paste("rdatamaps/",uname,"/",resol,"/tmean_12.tif",sep=""),overwrite=T,datatype='FLT4S')
  
  
  ####GEOPHYS
  
  alt<-crop(raster(paste("rdatamaps/world/",resol,"/alt.tif",sep="")),molde)
  extent(alt)<-moldextent
  alt<-mask(alt,molde)
  writeRaster(alt,filename=paste("rdatamaps/",uname,"/",resol,"/alt.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(alt)
  
  aspect<-crop(raster(paste("rdatamaps/world/",resol,"/aspect.tif",sep="")),molde)
  extent(aspect)<-moldextent
  aspect<-mask(aspect,molde)
  writeRaster(aspect,filename=paste("rdatamaps/",uname,"/",resol,"/aspect.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(aspect)
  
  eastness<-crop(raster(paste("rdatamaps/world/",resol,"/eastness.tif",sep="")),molde)
  extent(eastness)<-moldextent
  eastness<-mask(eastness,molde)
  writeRaster(eastness,filename=paste("rdatamaps/",uname,"/",resol,"/eastness.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(eastness)
  
  northness<-crop(raster(paste("rdatamaps/world/",resol,"/northness.tif",sep="")),molde)
  extent(northness)<-moldextent
  northness<-mask(northness,molde)
  writeRaster(northness,filename=paste("rdatamaps/",uname,"/",resol,"/northness.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(northness)
  
  slope<-crop(raster(paste("rdatamaps/world/",resol,"/slope.tif",sep="")),molde)
  extent(slope)<-moldextent
  slope<-mask(slope,molde)
  writeRaster(slope,filename=paste("rdatamaps/",uname,"/",resol,"/slope.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(slope)
  
  ###EDAPH
  
  ref_depth<-crop(raster(paste("rdatamaps/world/",resol,"/ref_depth.tif",sep="")),molde)
  extent(ref_depth)<-moldextent
  ref_depth<-mask(ref_depth,molde)
  writeRaster(ref_depth,filename=paste("rdatamaps/",uname,"/",resol,"/ref_depth.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(ref_depth)
  
  s_bs<-crop(raster(paste("rdatamaps/world/",resol,"/s_bs.tif",sep="")),molde)
  extent(s_bs)<-moldextent
  s_bs<-mask(s_bs,molde)
  writeRaster(s_bs,filename=paste("rdatamaps/",uname,"/",resol,"/s_bs.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_bs)
  
  s_caco3<-crop(raster(paste("rdatamaps/world/",resol,"/s_caco3.tif",sep="")),molde)
  extent(s_caco3)<-moldextent
  s_caco3<-mask(s_caco3,molde)
  writeRaster(s_caco3,filename=paste("rdatamaps/",uname,"/",resol,"/s_caco3.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_caco3)
  
  s_caso4<-crop(raster(paste("rdatamaps/world/",resol,"/s_caso4.tif",sep="")),molde)
  extent(s_caso4)<-moldextent
  s_caso4<-mask(s_caso4,molde)
  writeRaster(s_caso4,filename=paste("rdatamaps/",uname,"/",resol,"/s_caso4.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_caso4)
  
  s_cec_clay<-crop(raster(paste("rdatamaps/world/",resol,"/s_cec_clay.tif",sep="")),molde)
  extent(s_cec_clay)<-moldextent
  s_cec_clay<-mask(s_cec_clay,molde)
  writeRaster(s_cec_clay,filename=paste("rdatamaps/",uname,"/",resol,"/s_cec_clay.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_cec_clay)
  
  s_cec_soil<-crop(raster(paste("rdatamaps/world/",resol,"/s_cec_soil.tif",sep="")),molde)
  extent(s_cec_soil)<-moldextent
  s_cec_soil<-mask(s_cec_soil,molde)
  writeRaster(s_cec_soil,filename=paste("rdatamaps/",uname,"/",resol,"/s_cec_soil.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_cec_soil)
  
  s_clay<-crop(raster(paste("rdatamaps/world/",resol,"/s_clay.tif",sep="")),molde)
  extent(s_clay)<-moldextent
  s_clay<-mask(s_clay,molde)
  writeRaster(s_clay,filename=paste("rdatamaps/",uname,"/",resol,"/s_clay.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_clay)
  
  s_ece<-crop(raster(paste("rdatamaps/world/",resol,"/s_ece.tif",sep="")),molde)
  extent(s_ece)<-moldextent
  s_ece<-mask(s_ece,molde)
  writeRaster(s_ece,filename=paste("rdatamaps/",uname,"/",resol,"/s_ece.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_ece)
  
  s_esp<-crop(raster(paste("rdatamaps/world/",resol,"/s_esp.tif",sep="")),molde)
  extent(s_esp)<-moldextent
  s_esp<-mask(s_esp,molde)
  writeRaster(s_esp,filename=paste("rdatamaps/",uname,"/",resol,"/s_esp.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_esp)
  
  s_gravel<-crop(raster(paste("rdatamaps/world/",resol,"/s_gravel.tif",sep="")),molde)
  extent(s_gravel)<-moldextent
  s_gravel<-mask(s_gravel,molde)
  writeRaster(s_gravel,filename=paste("rdatamaps/",uname,"/",resol,"/s_gravel.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_gravel)
  
  s_oc<-crop(raster(paste("rdatamaps/world/",resol,"/s_oc.tif",sep="")),molde)
  extent(s_oc)<-moldextent
  s_oc<-mask(s_oc,molde)
  writeRaster(s_oc,filename=paste("rdatamaps/",uname,"/",resol,"/s_oc.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_oc)
  
  s_ph_h2o<-crop(raster(paste("rdatamaps/world/",resol,"/s_ph_h2o.tif",sep="")),molde)
  extent(s_ph_h2o)<-moldextent
  s_ph_h2o<-mask(s_ph_h2o,molde)
  writeRaster(s_ph_h2o,filename=paste("rdatamaps/",uname,"/",resol,"/s_ph_h2o.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_ph_h2o)
  
  s_ref_bulk<-crop(raster(paste("rdatamaps/world/",resol,"/s_ref_bulk.tif",sep="")),molde)
  extent(s_ref_bulk)<-moldextent
  s_ref_bulk<-mask(s_ref_bulk,molde)
  writeRaster(s_ref_bulk,filename=paste("rdatamaps/",uname,"/",resol,"/s_ref_bulk.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_ref_bulk)
  
  s_sand<-crop(raster(paste("rdatamaps/world/",resol,"/s_sand.tif",sep="")),molde)
  extent(s_sand)<-moldextent
  s_sand<-mask(s_sand,molde)
  writeRaster(s_sand,filename=paste("rdatamaps/",uname,"/",resol,"/s_sand.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_sand)
  
  s_silt<-crop(raster(paste("rdatamaps/world/",resol,"/s_silt.tif",sep="")),molde)
  extent(s_silt)<-moldextent
  s_silt<-mask(s_silt,molde)
  writeRaster(s_silt,filename=paste("rdatamaps/",uname,"/",resol,"/s_silt.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_silt)
  
  s_teb<-crop(raster(paste("rdatamaps/world/",resol,"/s_teb.tif",sep="")),molde)
  extent(s_teb)<-moldextent
  s_teb<-mask(s_teb,molde)
  writeRaster(s_teb,filename=paste("rdatamaps/",uname,"/",resol,"/s_teb.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_teb)
  
  t_bs<-crop(raster(paste("rdatamaps/world/",resol,"/t_bs.tif",sep="")),molde)
  extent(t_bs)<-moldextent
  t_bs<-mask(t_bs,molde)
  writeRaster(t_bs,filename=paste("rdatamaps/",uname,"/",resol,"/t_bs.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_bs)
  
  t_caco3<-crop(raster(paste("rdatamaps/world/",resol,"/t_caco3.tif",sep="")),molde)
  extent(t_caco3)<-moldextent
  t_caco3<-mask(t_caco3,molde)
  writeRaster(t_caco3,filename=paste("rdatamaps/",uname,"/",resol,"/t_caco3.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_caco3)
  
  t_caso4<-crop(raster(paste("rdatamaps/world/",resol,"/t_caso4.tif",sep="")),molde)
  extent(t_caso4)<-moldextent
  t_caso4<-mask(t_caso4,molde)
  writeRaster(t_caso4,filename=paste("rdatamaps/",uname,"/",resol,"/t_caso4.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_caso4)
  
  t_cec_clay<-crop(raster(paste("rdatamaps/world/",resol,"/t_cec_clay.tif",sep="")),molde)
  extent(t_cec_clay)<-moldextent
  t_cec_clay<-mask(t_cec_clay,molde)
  writeRaster(t_cec_clay,filename=paste("rdatamaps/",uname,"/",resol,"/t_cec_clay.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_cec_clay)
  
  t_cec_soil<-crop(raster(paste("rdatamaps/world/",resol,"/t_cec_soil.tif",sep="")),molde)
  extent(t_cec_soil)<-moldextent
  t_cec_soil<-mask(t_cec_soil,molde)
  writeRaster(t_cec_soil,filename=paste("rdatamaps/",uname,"/",resol,"/t_cec_soil.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_cec_soil)
  
  t_clay<-crop(raster(paste("rdatamaps/world/",resol,"/t_clay.tif",sep="")),molde)
  extent(t_clay)<-moldextent
  t_clay<-mask(t_clay,molde)
  writeRaster(t_clay,filename=paste("rdatamaps/",uname,"/",resol,"/t_clay.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_clay)
  
  t_ece<-crop(raster(paste("rdatamaps/world/",resol,"/t_ece.tif",sep="")),molde)
  extent(t_ece)<-moldextent
  t_ece<-mask(t_ece,molde)
  writeRaster(t_ece,filename=paste("rdatamaps/",uname,"/",resol,"/t_ece.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_ece)
  
  t_esp<-crop(raster(paste("rdatamaps/world/",resol,"/t_esp.tif",sep="")),molde)
  extent(t_esp)<-moldextent
  t_esp<-mask(t_esp,molde)
  writeRaster(t_esp,filename=paste("rdatamaps/",uname,"/",resol,"/t_esp.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_esp)
  
  t_gravel<-crop(raster(paste("rdatamaps/world/",resol,"/t_gravel.tif",sep="")),molde)
  extent(t_gravel)<-moldextent
  t_gravel<-mask(t_gravel,molde)
  writeRaster(t_gravel,filename=paste("rdatamaps/",uname,"/",resol,"/t_gravel.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_gravel)
  
  t_oc<-crop(raster(paste("rdatamaps/world/",resol,"/t_oc.tif",sep="")),molde)
  extent(t_oc)<-moldextent
  t_oc<-mask(t_oc,molde)
  writeRaster(t_oc,filename=paste("rdatamaps/",uname,"/",resol,"/t_oc.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_oc)
  
  t_ph_h2o<-crop(raster(paste("rdatamaps/world/",resol,"/t_ph_h2o.tif",sep="")),molde)
  extent(t_ph_h2o)<-moldextent
  t_ph_h2o<-mask(t_ph_h2o,molde)
  writeRaster(t_ph_h2o,filename=paste("rdatamaps/",uname,"/",resol,"/t_ph_h2o.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_ph_h2o)
  
  t_ref_bulk<-crop(raster(paste("rdatamaps/world/",resol,"/t_ref_bulk.tif",sep="")),molde)
  extent(t_ref_bulk)<-moldextent
  t_ref_bulk<-mask(t_ref_bulk,molde)
  writeRaster(t_ref_bulk,filename=paste("rdatamaps/",uname,"/",resol,"/t_ref_bulk.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_ref_bulk)
  
  t_sand<-crop(raster(paste("rdatamaps/world/",resol,"/t_sand.tif",sep="")),molde)
  extent(t_sand)<-moldextent
  t_sand<-mask(t_sand,molde)
  writeRaster(t_sand,filename=paste("rdatamaps/",uname,"/",resol,"/t_sand.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_sand)
  
  t_silt<-crop(raster(paste("rdatamaps/world/",resol,"/t_silt.tif",sep="")),molde)
  extent(t_silt)<-moldextent
  t_silt<-mask(t_silt,molde)
  writeRaster(t_silt,filename=paste("rdatamaps/",uname,"/",resol,"/t_silt.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_silt)
  
  t_teb<-crop(raster(paste("rdatamaps/world/",resol,"/t_teb.tif",sep="")),molde)
  extent(t_teb)<-moldextent
  t_teb<-mask(t_teb,molde)
  writeRaster(t_teb,filename=paste("rdatamaps/",uname,"/",resol,"/t_teb.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_teb)
  
  #Nuevas variables
  
  srad_1<-crop(raster(paste("rdatamaps/world/",resol,"/srad_1.tif",sep="")),molde)
  extent(srad_1)<-moldextent
  srad_1<-mask(srad_1,molde)
  writeRaster(srad_1,filename=paste("rdatamaps/",uname,"/",resol,"/srad_1.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(srad_1)
  
  srad_2<-crop(raster(paste("rdatamaps/world/",resol,"/srad_2.tif",sep="")),molde)
  extent(srad_2)<-moldextent
  srad_2<-mask(srad_2,molde)
  writeRaster(srad_2,filename=paste("rdatamaps/",uname,"/",resol,"/srad_2.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(srad_2)
  
  srad_3<-crop(raster(paste("rdatamaps/world/",resol,"/srad_3.tif",sep="")),molde)
  extent(srad_3)<-moldextent
  srad_3<-mask(srad_3,molde)
  writeRaster(srad_3,filename=paste("rdatamaps/",uname,"/",resol,"/srad_3.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(srad_3)
  
  srad_4<-crop(raster(paste("rdatamaps/world/",resol,"/srad_4.tif",sep="")),molde)
  extent(srad_4)<-moldextent
  srad_4<-mask(srad_4,molde)
  writeRaster(srad_4,filename=paste("rdatamaps/",uname,"/",resol,"/srad_4.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(srad_4)
  
  srad_5<-crop(raster(paste("rdatamaps/world/",resol,"/srad_5.tif",sep="")),molde)
  extent(srad_5)<-moldextent
  srad_5<-mask(srad_5,molde)
  writeRaster(srad_5,filename=paste("rdatamaps/",uname,"/",resol,"/srad_5.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(srad_5)
  
  srad_6<-crop(raster(paste("rdatamaps/world/",resol,"/srad_6.tif",sep="")),molde)
  extent(srad_6)<-moldextent
  srad_6<-mask(srad_6,molde)
  writeRaster(srad_6,filename=paste("rdatamaps/",uname,"/",resol,"/srad_6.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(srad_6)
  
  srad_7<-crop(raster(paste("rdatamaps/world/",resol,"/srad_7.tif",sep="")),molde)
  extent(srad_7)<-moldextent
  srad_7<-mask(srad_7,molde)
  writeRaster(srad_7,filename=paste("rdatamaps/",uname,"/",resol,"/srad_7.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(srad_7)
  
  srad_8<-crop(raster(paste("rdatamaps/world/",resol,"/srad_8.tif",sep="")),molde)
  extent(srad_8)<-moldextent
  srad_8<-mask(srad_8,molde)
  writeRaster(srad_8,filename=paste("rdatamaps/",uname,"/",resol,"/srad_8.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(srad_8)
  
  srad_9<-crop(raster(paste("rdatamaps/world/",resol,"/srad_9.tif",sep="")),molde)
  extent(srad_9)<-moldextent
  srad_9<-mask(srad_9,molde)
  writeRaster(srad_9,filename=paste("rdatamaps/",uname,"/",resol,"/srad_9.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(srad_9)
  
  srad_10<-crop(raster(paste("rdatamaps/world/",resol,"/srad_10.tif",sep="")),molde)
  extent(srad_10)<-moldextent
  srad_10<-mask(srad_10,molde)
  writeRaster(srad_10,filename=paste("rdatamaps/",uname,"/",resol,"/srad_10.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(srad_10)
  
  srad_11<-crop(raster(paste("rdatamaps/world/",resol,"/srad_11.tif",sep="")),molde)
  extent(srad_11)<-moldextent
  srad_11<-mask(srad_11,molde)
  writeRaster(srad_11,filename=paste("rdatamaps/",uname,"/",resol,"/srad_11.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(srad_11)
  
  srad_12<-crop(raster(paste("rdatamaps/world/",resol,"/srad_12.tif",sep="")),molde)
  extent(srad_12)<-moldextent
  srad_12<-mask(srad_12,molde)
  writeRaster(srad_12,filename=paste("rdatamaps/",uname,"/",resol,"/srad_12.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(srad_12)
  
  srad_annual<-crop(raster(paste("rdatamaps/world/",resol,"/srad_annual.tif",sep="")),molde)
  extent(srad_annual)<-moldextent
  srad_annual<-mask(srad_annual,molde)
  writeRaster(srad_annual,filename=paste("rdatamaps/",uname,"/",resol,"/srad_annual.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(srad_annual)
  
  wind_1<-crop(raster(paste("rdatamaps/world/",resol,"/wind_1.tif",sep="")),molde)
  extent(wind_1)<-moldextent
  wind_1<-mask(wind_1,molde)
  writeRaster(wind_1,filename=paste("rdatamaps/",uname,"/",resol,"/wind_1.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(wind_1)
  
  wind_2<-crop(raster(paste("rdatamaps/world/",resol,"/wind_2.tif",sep="")),molde)
  extent(wind_2)<-moldextent
  wind_2<-mask(wind_2,molde)
  writeRaster(wind_2,filename=paste("rdatamaps/",uname,"/",resol,"/wind_2.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(wind_2)
  
  wind_3<-crop(raster(paste("rdatamaps/world/",resol,"/wind_3.tif",sep="")),molde)
  extent(wind_3)<-moldextent
  wind_3<-mask(wind_3,molde)
  writeRaster(wind_3,filename=paste("rdatamaps/",uname,"/",resol,"/wind_3.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(wind_3)
  
  wind_4<-crop(raster(paste("rdatamaps/world/",resol,"/wind_4.tif",sep="")),molde)
  extent(wind_4)<-moldextent
  wind_4<-mask(wind_4,molde)
  writeRaster(wind_4,filename=paste("rdatamaps/",uname,"/",resol,"/wind_4.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(wind_4)
  
  wind_5<-crop(raster(paste("rdatamaps/world/",resol,"/wind_5.tif",sep="")),molde)
  extent(wind_5)<-moldextent
  wind_5<-mask(wind_5,molde)
  writeRaster(wind_5,filename=paste("rdatamaps/",uname,"/",resol,"/wind_5.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(wind_5)
  
  wind_6<-crop(raster(paste("rdatamaps/world/",resol,"/wind_6.tif",sep="")),molde)
  extent(wind_6)<-moldextent
  wind_6<-mask(wind_6,molde)
  writeRaster(wind_6,filename=paste("rdatamaps/",uname,"/",resol,"/wind_6.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(wind_6)
  
  wind_7<-crop(raster(paste("rdatamaps/world/",resol,"/wind_7.tif",sep="")),molde)
  extent(wind_7)<-moldextent
  wind_7<-mask(wind_7,molde)
  writeRaster(wind_7,filename=paste("rdatamaps/",uname,"/",resol,"/wind_7.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(wind_7)
  
  wind_8<-crop(raster(paste("rdatamaps/world/",resol,"/wind_8.tif",sep="")),molde)
  extent(wind_8)<-moldextent
  wind_8<-mask(wind_8,molde)
  writeRaster(wind_8,filename=paste("rdatamaps/",uname,"/",resol,"/wind_8.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(wind_8)
  
  wind_9<-crop(raster(paste("rdatamaps/world/",resol,"/wind_9.tif",sep="")),molde)
  extent(wind_9)<-moldextent
  wind_9<-mask(wind_9,molde)
  writeRaster(wind_9,filename=paste("rdatamaps/",uname,"/",resol,"/wind_9.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(wind_9)
  
  wind_10<-crop(raster(paste("rdatamaps/world/",resol,"/wind_10.tif",sep="")),molde)
  extent(wind_10)<-moldextent
  wind_10<-mask(wind_10,molde)
  writeRaster(wind_10,filename=paste("rdatamaps/",uname,"/",resol,"/wind_10.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(wind_10)
  
  wind_11<-crop(raster(paste("rdatamaps/world/",resol,"/wind_11.tif",sep="")),molde)
  extent(wind_11)<-moldextent
  wind_11<-mask(wind_11,molde)
  writeRaster(wind_11,filename=paste("rdatamaps/",uname,"/",resol,"/wind_11.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(wind_11)
  
  wind_12<-crop(raster(paste("rdatamaps/world/",resol,"/wind_12.tif",sep="")),molde)
  extent(wind_12)<-moldextent
  wind_12<-mask(wind_12,molde)
  writeRaster(wind_12,filename=paste("rdatamaps/",uname,"/",resol,"/wind_12.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(wind_12)
  
  wind_annual<-crop(raster(paste("rdatamaps/world/",resol,"/wind_annual.tif",sep="")),molde)
  extent(wind_annual)<-moldextent
  wind_annual<-mask(wind_annual,molde)
  writeRaster(wind_annual,filename=paste("rdatamaps/",uname,"/",resol,"/wind_annual.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(wind_annual)
  
  vapr_1<-crop(raster(paste("rdatamaps/world/",resol,"/vapr_1.tif",sep="")),molde)
  extent(vapr_1)<-moldextent
  vapr_1<-mask(vapr_1,molde)
  writeRaster(vapr_1,filename=paste("rdatamaps/",uname,"/",resol,"/vapr_1.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(vapr_1)
  
  vapr_2<-crop(raster(paste("rdatamaps/world/",resol,"/vapr_2.tif",sep="")),molde)
  extent(vapr_2)<-moldextent
  vapr_2<-mask(vapr_2,molde)
  writeRaster(vapr_2,filename=paste("rdatamaps/",uname,"/",resol,"/vapr_2.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(vapr_2)
  
  vapr_3<-crop(raster(paste("rdatamaps/world/",resol,"/vapr_3.tif",sep="")),molde)
  extent(vapr_3)<-moldextent
  vapr_3<-mask(vapr_3,molde)
  writeRaster(vapr_3,filename=paste("rdatamaps/",uname,"/",resol,"/vapr_3.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(vapr_3)
  
  vapr_4<-crop(raster(paste("rdatamaps/world/",resol,"/vapr_4.tif",sep="")),molde)
  extent(vapr_4)<-moldextent
  vapr_4<-mask(vapr_4,molde)
  writeRaster(vapr_4,filename=paste("rdatamaps/",uname,"/",resol,"/vapr_4.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(vapr_4)
  
  vapr_5<-crop(raster(paste("rdatamaps/world/",resol,"/vapr_5.tif",sep="")),molde)
  extent(vapr_5)<-moldextent
  vapr_5<-mask(vapr_5,molde)
  writeRaster(vapr_5,filename=paste("rdatamaps/",uname,"/",resol,"/vapr_5.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(vapr_5)
  
  vapr_6<-crop(raster(paste("rdatamaps/world/",resol,"/vapr_6.tif",sep="")),molde)
  extent(vapr_6)<-moldextent
  vapr_6<-mask(vapr_6,molde)
  writeRaster(vapr_6,filename=paste("rdatamaps/",uname,"/",resol,"/vapr_6.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(vapr_6)
  
  vapr_7<-crop(raster(paste("rdatamaps/world/",resol,"/vapr_7.tif",sep="")),molde)
  extent(vapr_7)<-moldextent
  vapr_7<-mask(vapr_7,molde)
  writeRaster(vapr_7,filename=paste("rdatamaps/",uname,"/",resol,"/vapr_7.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(vapr_7)
  
  vapr_8<-crop(raster(paste("rdatamaps/world/",resol,"/vapr_8.tif",sep="")),molde)
  extent(vapr_8)<-moldextent
  vapr_8<-mask(vapr_8,molde)
  writeRaster(vapr_8,filename=paste("rdatamaps/",uname,"/",resol,"/vapr_8.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(vapr_8)
  
  vapr_9<-crop(raster(paste("rdatamaps/world/",resol,"/vapr_9.tif",sep="")),molde)
  extent(vapr_9)<-moldextent
  vapr_9<-mask(vapr_9,molde)
  writeRaster(vapr_9,filename=paste("rdatamaps/",uname,"/",resol,"/vapr_9.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(vapr_9)
  
  vapr_10<-crop(raster(paste("rdatamaps/world/",resol,"/vapr_10.tif",sep="")),molde)
  extent(vapr_10)<-moldextent
  vapr_10<-mask(vapr_10,molde)
  writeRaster(vapr_10,filename=paste("rdatamaps/",uname,"/",resol,"/vapr_10.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(vapr_10)
  
  vapr_11<-crop(raster(paste("rdatamaps/world/",resol,"/vapr_11.tif",sep="")),molde)
  extent(vapr_11)<-moldextent
  vapr_11<-mask(vapr_11,molde)
  writeRaster(vapr_11,filename=paste("rdatamaps/",uname,"/",resol,"/vapr_11.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(vapr_11)
  
  vapr_12<-crop(raster(paste("rdatamaps/world/",resol,"/vapr_12.tif",sep="")),molde)
  extent(vapr_12)<-moldextent
  vapr_12<-mask(vapr_12,molde)
  writeRaster(vapr_12,filename=paste("rdatamaps/",uname,"/",resol,"/vapr_12.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(vapr_12)
  
  vapr_annual<-crop(raster(paste("rdatamaps/world/",resol,"/vapr_annual.tif",sep="")),molde)
  extent(vapr_annual)<-moldextent
  vapr_annual<-mask(vapr_annual,molde)
  writeRaster(vapr_annual,filename=paste("rdatamaps/",uname,"/",resol,"/vapr_annual.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(vapr_annual)
  
  #SOILGRIDS
  
  #AWC1
  t_awc1<-crop(raster(paste("rdatamaps/world/",resol,"/t_awc1.tif",sep="")),molde)
  extent(t_awc1)<-moldextent
  t_awc1<-mask(t_awc1,molde)
  
  s_awc1<-crop(raster(paste("rdatamaps/world/",resol,"/s_awc1.tif",sep="")),molde)
  extent(s_awc1)<-moldextent
  s_awc1<-mask(s_awc1,molde)
  
  writeRaster(t_awc1,filename=paste("rdatamaps/",uname,"/",resol,"/t_awc1.tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(s_awc1,filename=paste("rdatamaps/",uname,"/",resol,"/s_awc1.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_awc1)
  rm(s_awc1)
  
  
  #AWC2
  t_awc2<-crop(raster(paste("rdatamaps/world/",resol,"/t_awc2.tif",sep="")),molde)
  extent(t_awc2)<-moldextent
  t_awc2<-mask(t_awc2,molde)
  
  s_awc2<-crop(raster(paste("rdatamaps/world/",resol,"/s_awc2.tif",sep="")),molde)
  extent(s_awc2)<-moldextent
  s_awc2<-mask(s_awc2,molde)
  
  writeRaster(t_awc2,filename=paste("rdatamaps/",uname,"/",resol,"/t_awc2.tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(s_awc2,filename=paste("rdatamaps/",uname,"/",resol,"/s_awc2.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_awc2)
  rm(t_awc2)
  
  #AWC3
  t_awc3<-crop(raster(paste("rdatamaps/world/",resol,"/t_awc3.tif",sep="")),molde)
  extent(t_awc3)<-moldextent
  t_awc3<-mask(t_awc3,molde)
  
  s_awc3<-crop(raster(paste("rdatamaps/world/",resol,"/s_awc3.tif",sep="")),molde)
  extent(s_awc3)<-moldextent
  s_awc3<-mask(s_awc3,molde)
  
  writeRaster(t_awc3,filename=paste("rdatamaps/",uname,"/",resol,"/t_awc3.tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(s_awc3,filename=paste("rdatamaps/",uname,"/",resol,"/s_awc3.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_awc3)
  rm(t_awc3)
  
  #AWCts
  t_awcts<-crop(raster(paste("rdatamaps/world/",resol,"/t_awcts.tif",sep="")),molde)
  extent(t_awcts)<-moldextent
  t_awcts<-mask(t_awcts,molde)
  
  s_awcts<-crop(raster(paste("rdatamaps/world/",resol,"/s_awcts.tif",sep="")),molde)
  extent(s_awcts)<-moldextent
  s_awcts<-mask(s_awcts,molde)
  
  writeRaster(t_awcts,filename=paste("rdatamaps/",uname,"/",resol,"/t_awcts.tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(s_awcts,filename=paste("rdatamaps/",uname,"/",resol,"/s_awcts.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_awcts)
  rm(t_awcts)
  
  #Depth rock
  depth_rock<-crop(raster(paste("rdatamaps/world/",resol,"/depth_rock.tif",sep="")),molde)
  extent(depth_rock)<-moldextent
  depth_rock<-mask(depth_rock,molde)
  writeRaster(depth_rock,filename=paste("rdatamaps/",uname,"/",resol,"/depth_rock.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(depth_rock)
  
  #R horizon
  r_horizon<-crop(raster(paste("rdatamaps/world/",resol,"/r_horizon.tif",sep="")),molde)
  extent(r_horizon)<-moldextent
  r_horizon<-mask(r_horizon,molde)
  writeRaster(r_horizon,filename=paste("rdatamaps/",uname,"/",resol,"/r_horizon.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(r_horizon)
  
  #Bulk density
  t_bulk_dens<-crop(raster(paste("rdatamaps/world/",resol,"/t_bulk_dens.tif",sep="")),molde)
  extent(t_bulk_dens)<-moldextent
  t_bulk_dens<-mask(t_bulk_dens,molde)
  
  s_bulk_dens<-crop(raster(paste("rdatamaps/world/",resol,"/s_bulk_dens.tif",sep="")),molde)
  extent(s_bulk_dens)<-moldextent
  s_bulk_dens<-mask(s_bulk_dens,molde)
  
  writeRaster(t_bulk_dens,filename=paste("rdatamaps/",uname,"/",resol,"/t_bulk_dens.tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(s_bulk_dens,filename=paste("rdatamaps/",uname,"/",resol,"/s_bulk_dens.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_bulk_dens)
  rm(t_bulk_dens)
  
  #Cec soil
  t_cecsol<-crop(raster(paste("rdatamaps/world/",resol,"/t_cecsol.tif",sep="")),molde)
  extent(t_cecsol)<-moldextent
  t_cecsol<-mask(t_cecsol,molde)
  
  s_cecsol<-crop(raster(paste("rdatamaps/world/",resol,"/s_cecsol.tif",sep="")),molde)
  extent(s_cecsol)<-moldextent
  s_cecsol<-mask(s_cecsol,molde)
  
  writeRaster(t_cecsol,filename=paste("rdatamaps/",uname,"/",resol,"/t_cecsol.tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(s_cecsol,filename=paste("rdatamaps/",uname,"/",resol,"/s_cecsol.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_cecsol)
  rm(t_cecsol)
  
  #Clay content
  t_clay_cont<-crop(raster(paste("rdatamaps/world/",resol,"/t_clay_cont.tif",sep="")),molde)
  extent(t_clay_cont)<-moldextent
  t_clay_cont<-mask(t_clay_cont,molde)
  
  s_clay_cont<-crop(raster(paste("rdatamaps/world/",resol,"/s_clay_cont.tif",sep="")),molde)
  extent(s_clay_cont)<-moldextent
  s_clay_cont<-mask(s_clay_cont,molde)
  
  writeRaster(t_clay_cont,filename=paste("rdatamaps/",uname,"/",resol,"/t_clay_cont.tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(s_clay_cont,filename=paste("rdatamaps/",uname,"/",resol,"/s_clay_cont.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_clay_cont)
  rm(t_clay_cont)
  
  #Coarse frag
  t_coarse_frag<-crop(raster(paste("rdatamaps/world/",resol,"/t_coarse_frag.tif",sep="")),molde)
  extent(t_coarse_frag)<-moldextent
  t_coarse_frag<-mask(t_coarse_frag,molde)
  
  s_coarse_frag<-crop(raster(paste("rdatamaps/world/",resol,"/s_coarse_frag.tif",sep="")),molde)
  extent(s_coarse_frag)<-moldextent
  s_coarse_frag<-mask(s_coarse_frag,molde)
  
  writeRaster(t_coarse_frag,filename=paste("rdatamaps/",uname,"/",resol,"/t_coarse_frag.tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(s_coarse_frag,filename=paste("rdatamaps/",uname,"/",resol,"/s_coarse_frag.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_coarse_frag)
  rm(t_coarse_frag)
  
  #OC density
  t_oc_dens<-crop(raster(paste("rdatamaps/world/",resol,"/t_oc_dens.tif",sep="")),molde)
  extent(t_oc_dens)<-moldextent
  t_oc_dens<-mask(t_oc_dens,molde)
  
  s_oc_dens<-crop(raster(paste("rdatamaps/world/",resol,"/s_oc_dens.tif",sep="")),molde)
  extent(s_oc_dens)<-moldextent
  s_oc_dens<-mask(s_oc_dens,molde)
  
  writeRaster(t_oc_dens,filename=paste("rdatamaps/",uname,"/",resol,"/t_oc_dens.tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(s_oc_dens,filename=paste("rdatamaps/",uname,"/",resol,"/s_oc_dens.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_oc_dens)
  rm(t_oc_dens)
  
  #OC stock
  t_oc_stock<-crop(raster(paste("rdatamaps/world/",resol,"/t_oc_stock.tif",sep="")),molde)
  extent(t_oc_stock)<-moldextent
  t_oc_stock<-mask(t_oc_stock,molde)
  writeRaster(t_oc_stock,filename=paste("rdatamaps/",uname,"/",resol,"/t_oc_stock.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_oc_stock)
  
  s_oc_stock<-crop(raster(paste("rdatamaps/world/",resol,"/s_oc_stock.tif",sep="")),molde)
  extent(s_oc_stock)<-moldextent
  s_oc_stock<-mask(s_oc_stock,molde)
  writeRaster(s_oc_stock,filename=paste("rdatamaps/",uname,"/",resol,"/s_oc_stock.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_oc_stock)
  
  #OC content
  t_oc_cont<-crop(raster(paste("rdatamaps/world/",resol,"/t_oc_cont.tif",sep="")),molde)
  extent(t_oc_cont)<-moldextent
  t_oc_cont<-mask(t_oc_cont,molde)
  
  s_oc_cont<-crop(raster(paste("rdatamaps/world/",resol,"/s_oc_cont.tif",sep="")),molde)
  extent(s_oc_cont)<-moldextent
  s_oc_cont<-mask(s_oc_cont,molde)
  
  writeRaster(t_oc_cont,filename=paste("rdatamaps/",uname,"/",resol,"/t_oc_cont.tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(s_oc_cont,filename=paste("rdatamaps/",uname,"/",resol,"/s_oc_cont.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_oc_cont)
  rm(t_oc_cont)
  
  #Ph h2O
  t_ph_hox<-crop(raster(paste("rdatamaps/world/",resol,"/t_ph_hox.tif",sep="")),molde)
  extent(t_ph_hox)<-moldextent
  t_ph_hox<-mask(t_ph_hox,molde)
  
  s_ph_hox<-crop(raster(paste("rdatamaps/world/",resol,"/s_ph_hox.tif",sep="")),molde)
  extent(s_ph_hox)<-moldextent
  s_ph_hox<-mask(s_ph_hox,molde)
  
  writeRaster(t_ph_hox,filename=paste("rdatamaps/",uname,"/",resol,"/t_ph_hox.tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(s_ph_hox,filename=paste("rdatamaps/",uname,"/",resol,"/s_ph_hox.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_ph_hox)
  rm(t_ph_hox)
  
  #Ph kcl
  t_ph_kcl<-crop(raster(paste("rdatamaps/world/",resol,"/t_ph_kcl.tif",sep="")),molde)
  extent(t_ph_kcl)<-moldextent
  t_ph_kcl<-mask(t_ph_kcl,molde)
  
  s_ph_kcl<-crop(raster(paste("rdatamaps/world/",resol,"/s_ph_kcl.tif",sep="")),molde)
  extent(s_ph_kcl)<-moldextent
  s_ph_kcl<-mask(s_ph_kcl,molde)
  
  writeRaster(t_ph_kcl,filename=paste("rdatamaps/",uname,"/",resol,"/t_ph_kcl.tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(s_ph_kcl,filename=paste("rdatamaps/",uname,"/",resol,"/s_ph_kcl.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_ph_kcl)
  rm(t_ph_kcl)
  
  #Sodicity
  sodicity<-crop(raster(paste("rdatamaps/world/",resol,"/sodicity.tif",sep="")),molde)
  extent(sodicity)<-moldextent
  sodicity<-mask(sodicity,molde)
  writeRaster(sodicity,filename=paste("rdatamaps/",uname,"/",resol,"/sodicity.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(sodicity)
  
  #Silt content
  t_silt_cont<-crop(raster(paste("rdatamaps/world/",resol,"/t_silt_cont.tif",sep="")),molde)
  extent(t_silt_cont)<-moldextent
  t_silt_cont<-mask(t_silt_cont,molde)
  
  s_silt_cont<-crop(raster(paste("rdatamaps/world/",resol,"/s_silt_cont.tif",sep="")),molde)
  extent(s_silt_cont)<-moldextent
  s_silt_cont<-mask(s_silt_cont,molde)
  
  writeRaster(t_silt_cont,filename=paste("rdatamaps/",uname,"/",resol,"/t_silt_cont.tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(s_silt_cont,filename=paste("rdatamaps/",uname,"/",resol,"/s_silt_cont.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_silt_cont)
  rm(s_silt_cont)
  
  #Sand content
  t_sand_cont<-crop(raster(paste("rdatamaps/world/",resol,"/t_sand_cont.tif",sep="")),molde)
  extent(t_sand_cont)<-moldextent
  t_sand_cont<-mask(t_sand_cont,molde)
  
  s_sand_cont<-crop(raster(paste("rdatamaps/world/",resol,"/s_sand_cont.tif",sep="")),molde)
  extent(s_sand_cont)<-moldextent
  s_sand_cont<-mask(s_sand_cont,molde)
  
  writeRaster(t_sand_cont,filename=paste("rdatamaps/",uname,"/",resol,"/t_sand_cont.tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(s_sand_cont,filename=paste("rdatamaps/",uname,"/",resol,"/s_sand_cont.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_sand_cont)
  rm(t_sand_cont)
  
  #Soil water capacity
  t_soilwater_cap<-crop(raster(paste("rdatamaps/world/",resol,"/t_soilwater_cap.tif",sep="")),molde)
  extent(t_soilwater_cap)<-moldextent
  t_soilwater_cap<-mask(t_soilwater_cap,molde)
  
  s_soilwater_cap<-crop(raster(paste("rdatamaps/world/",resol,"/s_soilwater_cap.tif",sep="")),molde)
  extent(s_soilwater_cap)<-moldextent
  s_soilwater_cap<-mask(s_soilwater_cap,molde)
  
  writeRaster(t_soilwater_cap,filename=paste("rdatamaps/",uname,"/",resol,"/t_soilwater_cap.tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(s_soilwater_cap,filename=paste("rdatamaps/",uname,"/",resol,"/s_soilwater_cap.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_soilwater_cap)
  rm(t_soilwater_cap)
  
  
  #puntos
  bioclimn<-ncell(tmean_12)
  bioclimp<-xyFromCell(tmean_12,c(1:bioclimn))
  
  bioclime<-extract(tmean_12,bioclimp)
  
  bioclim<-cbind(bioclimp,bioclime)
  bioclim<-na.omit(bioclim)
  bioclim<-bioclim[,c(1,2)]
  
  POINTID<-as.vector(1:length(bioclim[,1]))
  
  bioclimp<-data.frame(cbind(POINTID,bioclim[,c(1,2)]))
  colnames(bioclimp)[2]<-"POINT_X"
  colnames(bioclimp)[3]<-"POINT_Y"
  puntos<-bioclimp
  
  setwd(paste(ruta,"/rdatapoints",sep=""))
  dir.create(as.vector(paste(uname)))
  setwd(paste(ruta,"/rdatapoints/",uname,sep=""))
  dir.create(as.vector(paste(resol)))
  save(puntos,file=paste(resol,"/","base",resol,".RData",sep=""))
  rm(tmean_12)
  rm(molde)
}

if(cropway=="square"){
  #Entrar funcion para determinar si se produce (TRUE) un error
  is.error<-function (expr, tell = FALSE, force = FALSE) 
  {
    expr_name <- deparse(substitute(expr))
    test <- try(expr, silent = TRUE)
    iserror <- inherits(test, "try-error")
    if (tell) 
      if (iserror) 
        message("Note in is.error: ", test)
    if (force) 
      if (!iserror) 
        stop(expr_name, " is not returning an error.", call. = FALSE)
    iserror
  }
  
  if(is.error(read.delim(paste("Pasaporte/",pasaporte,sep=""),colClasses=c("LONGITUDE"="character","LATITUDE"="character")))){
    pasaporte<-read.delim(paste("Pasaporte/",pasaporte,sep=""),colClasses=c("LONGITUDE"="character","LATITUDE"="character"), fileEncoding="latin1")
  } else {pasaporte<-read.delim(paste("Pasaporte/",pasaporte,sep=""),colClasses=c("LONGITUDE"="character","LATITUDE"="character"))}
  
  i <- order(pasaporte$ACCENUMB)
  pasaporte <- pasaporte[i,]
  write("4.Cargados los datos de pasaporte y ordenados por ACCENUMB", file="Error/process_info.txt", append=TRUE)
  
  #filtro por GEOQUAL
  if(geoqual){
    pasaporte<-subset(pasaporte,TOTALQUAL100>=totalqual)
  }
  
  #Trifurcaci?n (sin coordenadas, con coordenadas sexagesimales, con coordenadas decimales)
  sincoord<-subset(pasaporte,(is.na(LATITUDE)|is.na(LONGITUDE))&(is.na(DECLATITUDE)|is.na(DECLONGITUDE)))
  
  sexagesimal<-subset(pasaporte,(!is.na(LATITUDE)&!is.na(LONGITUDE)))
  sexagesimal<-sexagesimal[,c(1:22,24,26:ncol(sexagesimal))]
  decimal<-subset(pasaporte,(is.na(LATITUDE)|is.na(LONGITUDE))&(!is.na(DECLATITUDE)&!is.na(DECLONGITUDE)))
  
  
  ###Sexagesimal a decimal  
  #Obtenci?n de las coordenadas en formato decimal a partir de sexagesimal codificadas tal como lo indica el formato IPGRI 2001
  if(length(sexagesimal[,1])>0){
    coordec<-as.data.frame(matrix(nrow = length(sexagesimal[,1]), ncol = 2))
    #colnames(coordec)[1]<-"DECLATITUDE"
    #colnames(coordec)[2]<-"DECLONGITUDE"
    coordec<-data.frame(sexagesimal$ACCENUMB,coordec)
    colnames(coordec)[1]<-"ACCENUMB"
    for (i in 1:length(sexagesimal[,1])) {
      coordec[i,2]<-ifelse(sexagesimal$LATITUDE[i]=='NA','NA', ((as.numeric(substr(sexagesimal$LATITUDE[i],1,2))+
                                                                   (ifelse(substr(sexagesimal$LATITUDE[i],3,4)=='--',0,(as.numeric(substr(sexagesimal$LATITUDE[i],3,4))/60)))+
                                                                   (ifelse(substr(sexagesimal$LATITUDE[i],5,6)=='--',0,(as.numeric(substr(sexagesimal$LATITUDE[i],5,6))/3600))))                                               
                                                                *(as.numeric(ifelse(substr(sexagesimal$LATITUDE[i],7,7)=='N',1,-1)))))
      coordec[i,3]<-ifelse(sexagesimal$LONGITUDE[i]=='NA','NA', ((as.numeric(substr(sexagesimal$LONGITUDE[i],1,3))+
                                                                    (ifelse(substr(sexagesimal$LONGITUDE[i],4,5)=='--',0,(as.numeric(substr(sexagesimal$LONGITUDE[i],4,5))/60)))+
                                                                    (ifelse(substr(sexagesimal$LONGITUDE[i],6,7)=='--',0,(as.numeric(substr(sexagesimal$LONGITUDE[i],6,7))/3600))))                                               
                                                                 *(as.numeric(ifelse(substr(sexagesimal$LONGITUDE[i],8,8)=='E',1,-1)))))
    }
    sexagesimal<-cbind(sexagesimal[,1:22],coordec[,2],sexagesimal[,23],coordec[,3],sexagesimal[,24:ncol(sexagesimal)])
    colnames(sexagesimal)[23]<-"DECLATITUDE"
    colnames(sexagesimal)[24]<-"LATITUDE"
    colnames(sexagesimal)[25]<-"DECLONGITUDE"
    write("5.Terminado proceso de transformaci?n sexagesimal a decimal", file="Error/process_info.txt", append=TRUE)
    ###Unificaci?n coordenadas sexagesimal a decimal y decimal original
    puntosorig<-rbind(sexagesimal,decimal)
  }
  if(length(sexagesimal[,1])==0){
    puntosorig<-decimal
  }
  
  #Creaci?n de un archivo espacial de puntos
  puntos<-SpatialPoints(puntosorig[,c(25,23)],proj4string=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  
  #Fijar extent
  if(resol=="1x1"){
    expand<-0.1
  }
  if(resol=="5x5"){
    expand<-0.15
  }
  if(resol=="10x10"){
    expand<-0.15
  }
  if(resol=="20x20"){
    expand<-0.25
  }
  
  xmin<-min(puntosorig$DECLONGITUDE)-(expand)
  if(xmin< -180){
    xmin<- -180
  }
  xmax<-max(puntosorig$DECLONGITUDE)+(expand)
  if(xmin>180){
    xmin<- 180
  }
  ymin<-min(puntosorig$DECLATITUDE)-(expand)
  if(ymin< -90){
    ymin<- -90
  }
  ymax<-max(puntosorig$DECLATITUDE)+(expand)
  if(ymax>90){
    ymax<-90
  }
  Extent<-extent(xmin,xmax,ymin,ymax)
  
  #Evitar error por load de geophys, edaph y bioclim.RData
  loadError<-FALSE
  abcd<-try(load("edaph.RData"),silent=TRUE)
  loadError <- (is(abcd, 'try-error')|is(abcd,'error'))
  if(loadError){
    edaph<-read.delim("edaph.txt")
  }
  rm(abcd)
  rm(loadError)
  loadError<-FALSE
  abcd<-try(load("bioclim.RData"),silent=TRUE)
  loadError <- (is(abcd, 'try-error')|is(abcd,'error'))
  if(loadError){
    bioclim<-read.delim("bioclim.txt")
  }
  rm(abcd)
  rm(loadError)
  loadError<-FALSE
  abcd<-try(load("geophys.RData"),silent=TRUE)
  loadError <- (is(abcd, 'try-error')|is(abcd,'error'))
  if(loadError){
    geophys<-read.delim("geophys.txt")
  }
  rm(abcd)
  rm(loadError)
  
  #####BIOCLIM
  
  bioclimv<-as.vector(bioclim$VARCODE)
  
  bio_1<-crop(raster(paste("rdatamaps/world/",resol,"/bio_1.tif",sep="")),Extent)
  writeRaster(bio_1,filename=paste("rdatamaps/",uname,"/",resol,"/bio_1.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_1)
  
  bio_2<-crop(raster(paste("rdatamaps/world/",resol,"/bio_2.tif",sep="")),Extent)
  writeRaster(bio_2,filename=paste("rdatamaps/",uname,"/",resol,"/bio_2.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_2)
  
  bio_3<-crop(raster(paste("rdatamaps/world/",resol,"/bio_3.tif",sep="")),Extent)
  writeRaster(bio_3,filename=paste("rdatamaps/",uname,"/",resol,"/bio_3.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_3)
  
  bio_4<-crop(raster(paste("rdatamaps/world/",resol,"/bio_4.tif",sep="")),Extent)
  writeRaster(bio_4,filename=paste("rdatamaps/",uname,"/",resol,"/bio_4.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_4)
  
  bio_5<-crop(raster(paste("rdatamaps/world/",resol,"/bio_5.tif",sep="")),Extent)
  writeRaster(bio_5,filename=paste("rdatamaps/",uname,"/",resol,"/bio_5.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_5)
  
  bio_6<-crop(raster(paste("rdatamaps/world/",resol,"/bio_6.tif",sep="")),Extent)
  writeRaster(bio_6,filename=paste("rdatamaps/",uname,"/",resol,"/bio_6.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_6)
  
  bio_7<-crop(raster(paste("rdatamaps/world/",resol,"/bio_7.tif",sep="")),Extent)
  writeRaster(bio_7,filename=paste("rdatamaps/",uname,"/",resol,"/bio_7.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_7)
  
  bio_8<-crop(raster(paste("rdatamaps/world/",resol,"/bio_8.tif",sep="")),Extent)
  writeRaster(bio_8,filename=paste("rdatamaps/",uname,"/",resol,"/bio_8.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_8)
  
  bio_9<-crop(raster(paste("rdatamaps/world/",resol,"/bio_9.tif",sep="")),Extent)
  writeRaster(bio_9,filename=paste("rdatamaps/",uname,"/",resol,"/bio_9.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_9)
  
  bio_10<-crop(raster(paste("rdatamaps/world/",resol,"/bio_10.tif",sep="")),Extent)
  writeRaster(bio_10,filename=paste("rdatamaps/",uname,"/",resol,"/bio_10.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_10)
  
  bio_11<-crop(raster(paste("rdatamaps/world/",resol,"/bio_11.tif",sep="")),Extent)
  writeRaster(bio_11,filename=paste("rdatamaps/",uname,"/",resol,"/bio_11.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_11)
  
  bio_12<-crop(raster(paste("rdatamaps/world/",resol,"/bio_12.tif",sep="")),Extent)
  writeRaster(bio_12,filename=paste("rdatamaps/",uname,"/",resol,"/bio_12.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_12)
  
  bio_13<-crop(raster(paste("rdatamaps/world/",resol,"/bio_13.tif",sep="")),Extent)
  writeRaster(bio_13,filename=paste("rdatamaps/",uname,"/",resol,"/bio_13.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_13)
  
  bio_14<-crop(raster(paste("rdatamaps/world/",resol,"/bio_14.tif",sep="")),Extent)
  writeRaster(bio_14,filename=paste("rdatamaps/",uname,"/",resol,"/bio_14.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_14)
  
  bio_15<-crop(raster(paste("rdatamaps/world/",resol,"/bio_15.tif",sep="")),Extent)
  writeRaster(bio_15,filename=paste("rdatamaps/",uname,"/",resol,"/bio_15.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_15)
  
  bio_16<-crop(raster(paste("rdatamaps/world/",resol,"/bio_16.tif",sep="")),Extent)
  writeRaster(bio_16,filename=paste("rdatamaps/",uname,"/",resol,"/bio_16.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_16)
  
  bio_17<-crop(raster(paste("rdatamaps/world/",resol,"/bio_17.tif",sep="")),Extent)
  writeRaster(bio_17,filename=paste("rdatamaps/",uname,"/",resol,"/bio_17.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_17)
  
  bio_18<-crop(raster(paste("rdatamaps/world/",resol,"/bio_18.tif",sep="")),Extent)
  writeRaster(bio_18,filename=paste("rdatamaps/",uname,"/",resol,"/bio_18.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_18)
  
  bio_19<-crop(raster(paste("rdatamaps/world/",resol,"/bio_19.tif",sep="")),Extent)
  writeRaster(bio_19,filename=paste("rdatamaps/",uname,"/",resol,"/bio_19.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(bio_19)
  
  
  prec_1<-crop(raster(paste("rdatamaps/world/",resol,"/prec_1.tif",sep="")),Extent)
  writeRaster(prec_1,filename=paste("rdatamaps/",uname,"/",resol,"/prec_1.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(prec_1)
  
  prec_2<-crop(raster(paste("rdatamaps/world/",resol,"/prec_2.tif",sep="")),Extent)
  writeRaster(prec_2,filename=paste("rdatamaps/",uname,"/",resol,"/prec_2.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(prec_2)
  
  prec_3<-crop(raster(paste("rdatamaps/world/",resol,"/prec_3.tif",sep="")),Extent)
  writeRaster(prec_3,filename=paste("rdatamaps/",uname,"/",resol,"/prec_3.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(prec_3)
  
  prec_4<-crop(raster(paste("rdatamaps/world/",resol,"/prec_4.tif",sep="")),Extent)
  writeRaster(prec_4,filename=paste("rdatamaps/",uname,"/",resol,"/prec_4.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(prec_4)
  
  prec_5<-crop(raster(paste("rdatamaps/world/",resol,"/prec_5.tif",sep="")),Extent)
  writeRaster(prec_5,filename=paste("rdatamaps/",uname,"/",resol,"/prec_5.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(prec_5)
  
  prec_6<-crop(raster(paste("rdatamaps/world/",resol,"/prec_6.tif",sep="")),Extent)
  writeRaster(prec_6,filename=paste("rdatamaps/",uname,"/",resol,"/prec_6.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(prec_6)
  
  prec_7<-crop(raster(paste("rdatamaps/world/",resol,"/prec_7.tif",sep="")),Extent)
  writeRaster(prec_7,filename=paste("rdatamaps/",uname,"/",resol,"/prec_7.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(prec_7)
  
  prec_8<-crop(raster(paste("rdatamaps/world/",resol,"/prec_8.tif",sep="")),Extent)
  writeRaster(prec_8,filename=paste("rdatamaps/",uname,"/",resol,"/prec_8.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(prec_8)
  
  prec_9<-crop(raster(paste("rdatamaps/world/",resol,"/prec_9.tif",sep="")),Extent)
  writeRaster(prec_9,filename=paste("rdatamaps/",uname,"/",resol,"/prec_9.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(prec_9)
  
  prec_10<-crop(raster(paste("rdatamaps/world/",resol,"/prec_10.tif",sep="")),Extent)
  writeRaster(prec_10,filename=paste("rdatamaps/",uname,"/",resol,"/prec_10.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(prec_10)
  
  prec_11<-crop(raster(paste("rdatamaps/world/",resol,"/prec_11.tif",sep="")),Extent)
  writeRaster(prec_11,filename=paste("rdatamaps/",uname,"/",resol,"/prec_11.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(prec_11)
  
  prec_12<-crop(raster(paste("rdatamaps/world/",resol,"/prec_12.tif",sep="")),Extent)
  writeRaster(prec_12,filename=paste("rdatamaps/",uname,"/",resol,"/prec_12.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(prec_12)
  
  tmax_1<-crop(raster(paste("rdatamaps/world/",resol,"/tmax_1.tif",sep="")),Extent)
  writeRaster(tmax_1,filename=paste("rdatamaps/",uname,"/",resol,"/tmax_1.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmax_1)
  
  tmax_2<-crop(raster(paste("rdatamaps/world/",resol,"/tmax_2.tif",sep="")),Extent)
  writeRaster(tmax_2,filename=paste("rdatamaps/",uname,"/",resol,"/tmax_2.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmax_2)
  
  tmax_3<-crop(raster(paste("rdatamaps/world/",resol,"/tmax_3.tif",sep="")),Extent)
  writeRaster(tmax_3,filename=paste("rdatamaps/",uname,"/",resol,"/tmax_3.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmax_3)
  
  tmax_4<-crop(raster(paste("rdatamaps/world/",resol,"/tmax_4.tif",sep="")),Extent)
  writeRaster(tmax_4,filename=paste("rdatamaps/",uname,"/",resol,"/tmax_4.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmax_4)
  
  tmax_5<-crop(raster(paste("rdatamaps/world/",resol,"/tmax_5.tif",sep="")),Extent)
  writeRaster(tmax_5,filename=paste("rdatamaps/",uname,"/",resol,"/tmax_5.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmax_5)
  
  tmax_6<-crop(raster(paste("rdatamaps/world/",resol,"/tmax_6.tif",sep="")),Extent)
  writeRaster(tmax_6,filename=paste("rdatamaps/",uname,"/",resol,"/tmax_6.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmax_6)
  
  tmax_7<-crop(raster(paste("rdatamaps/world/",resol,"/tmax_7.tif",sep="")),Extent)
  writeRaster(tmax_7,filename=paste("rdatamaps/",uname,"/",resol,"/tmax_7.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmax_7)
  
  tmax_8<-crop(raster(paste("rdatamaps/world/",resol,"/tmax_8.tif",sep="")),Extent)
  writeRaster(tmax_8,filename=paste("rdatamaps/",uname,"/",resol,"/tmax_8.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmax_8)
  
  tmax_9<-crop(raster(paste("rdatamaps/world/",resol,"/tmax_9.tif",sep="")),Extent)
  writeRaster(tmax_9,filename=paste("rdatamaps/",uname,"/",resol,"/tmax_9.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmax_9)
  
  tmax_10<-crop(raster(paste("rdatamaps/world/",resol,"/tmax_10.tif",sep="")),Extent)
  writeRaster(tmax_10,filename=paste("rdatamaps/",uname,"/",resol,"/tmax_10.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmax_10)
  
  tmax_11<-crop(raster(paste("rdatamaps/world/",resol,"/tmax_11.tif",sep="")),Extent)
  writeRaster(tmax_11,filename=paste("rdatamaps/",uname,"/",resol,"/tmax_11.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmax_11)
  
  tmax_12<-crop(raster(paste("rdatamaps/world/",resol,"/tmax_12.tif",sep="")),Extent)
  writeRaster(tmax_12,filename=paste("rdatamaps/",uname,"/",resol,"/tmax_12.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmax_12)
  
  tmin_1<-crop(raster(paste("rdatamaps/world/",resol,"/tmin_1.tif",sep="")),Extent)
  writeRaster(tmin_1,filename=paste("rdatamaps/",uname,"/",resol,"/tmin_1.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmin_1)
  
  tmin_2<-crop(raster(paste("rdatamaps/world/",resol,"/tmin_2.tif",sep="")),Extent)
  writeRaster(tmin_2,filename=paste("rdatamaps/",uname,"/",resol,"/tmin_2.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmin_2)
  
  tmin_3<-crop(raster(paste("rdatamaps/world/",resol,"/tmin_3.tif",sep="")),Extent)
  writeRaster(tmin_3,filename=paste("rdatamaps/",uname,"/",resol,"/tmin_3.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmin_3)
  
  tmin_4<-crop(raster(paste("rdatamaps/world/",resol,"/tmin_4.tif",sep="")),Extent)
  writeRaster(tmin_4,filename=paste("rdatamaps/",uname,"/",resol,"/tmin_4.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmin_4)
  
  tmin_5<-crop(raster(paste("rdatamaps/world/",resol,"/tmin_5.tif",sep="")),Extent)
  writeRaster(tmin_5,filename=paste("rdatamaps/",uname,"/",resol,"/tmin_5.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmin_5)
  
  tmin_6<-crop(raster(paste("rdatamaps/world/",resol,"/tmin_6.tif",sep="")),Extent)
  writeRaster(tmin_6,filename=paste("rdatamaps/",uname,"/",resol,"/tmin_6.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmin_6)
  
  tmin_7<-crop(raster(paste("rdatamaps/world/",resol,"/tmin_7.tif",sep="")),Extent)
  writeRaster(tmin_7,filename=paste("rdatamaps/",uname,"/",resol,"/tmin_7.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmin_7)
  
  tmin_8<-crop(raster(paste("rdatamaps/world/",resol,"/tmin_8.tif",sep="")),Extent)
  writeRaster(tmin_8,filename=paste("rdatamaps/",uname,"/",resol,"/tmin_8.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmin_8)
  
  tmin_9<-crop(raster(paste("rdatamaps/world/",resol,"/tmin_9.tif",sep="")),Extent)
  writeRaster(tmin_9,filename=paste("rdatamaps/",uname,"/",resol,"/tmin_9.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmin_9)
  
  tmin_10<-crop(raster(paste("rdatamaps/world/",resol,"/tmin_10.tif",sep="")),Extent)
  writeRaster(tmin_10,filename=paste("rdatamaps/",uname,"/",resol,"/tmin_10.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmin_10)
  
  tmin_11<-crop(raster(paste("rdatamaps/world/",resol,"/tmin_11.tif",sep="")),Extent)
  writeRaster(tmin_11,filename=paste("rdatamaps/",uname,"/",resol,"/tmin_11.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmin_11)
  
  tmin_12<-crop(raster(paste("rdatamaps/world/",resol,"/tmin_12.tif",sep="")),Extent)
  writeRaster(tmin_12,filename=paste("rdatamaps/",uname,"/",resol,"/tmin_12.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmin_12)
  
  tmean_1<-crop(raster(paste("rdatamaps/world/",resol,"/tmean_1.tif",sep="")),Extent)
  writeRaster(tmean_1,filename=paste("rdatamaps/",uname,"/",resol,"/tmean_1.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmean_1)
  
  tmean_2<-crop(raster(paste("rdatamaps/world/",resol,"/tmean_2.tif",sep="")),Extent)
  writeRaster(tmean_2,filename=paste("rdatamaps/",uname,"/",resol,"/tmean_2.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmean_2)
  
  tmean_3<-crop(raster(paste("rdatamaps/world/",resol,"/tmean_3.tif",sep="")),Extent)
  writeRaster(tmean_3,filename=paste("rdatamaps/",uname,"/",resol,"/tmean_3.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmean_3)
  
  tmean_4<-crop(raster(paste("rdatamaps/world/",resol,"/tmean_4.tif",sep="")),Extent)
  writeRaster(tmean_4,filename=paste("rdatamaps/",uname,"/",resol,"/tmean_4.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmean_4)
  
  tmean_5<-crop(raster(paste("rdatamaps/world/",resol,"/tmean_5.tif",sep="")),Extent)
  writeRaster(tmean_5,filename=paste("rdatamaps/",uname,"/",resol,"/tmean_5.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmean_5)
  
  tmean_6<-crop(raster(paste("rdatamaps/world/",resol,"/tmean_6.tif",sep="")),Extent)
  writeRaster(tmean_6,filename=paste("rdatamaps/",uname,"/",resol,"/tmean_6.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmean_6)
  
  tmean_7<-crop(raster(paste("rdatamaps/world/",resol,"/tmean_7.tif",sep="")),Extent)
  writeRaster(tmean_7,filename=paste("rdatamaps/",uname,"/",resol,"/tmean_7.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmean_7)
  
  tmean_8<-crop(raster(paste("rdatamaps/world/",resol,"/tmean_8.tif",sep="")),Extent)
  writeRaster(tmean_8,filename=paste("rdatamaps/",uname,"/",resol,"/tmean_8.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmean_8)
  
  tmean_9<-crop(raster(paste("rdatamaps/world/",resol,"/tmean_9.tif",sep="")),Extent)
  writeRaster(tmean_9,filename=paste("rdatamaps/",uname,"/",resol,"/tmean_9.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmean_9)
  
  tmean_10<-crop(raster(paste("rdatamaps/world/",resol,"/tmean_10.tif",sep="")),Extent)
  writeRaster(tmean_10,filename=paste("rdatamaps/",uname,"/",resol,"/tmean_10.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmean_10)
  
  tmean_11<-crop(raster(paste("rdatamaps/world/",resol,"/tmean_11.tif",sep="")),Extent)
  writeRaster(tmean_11,filename=paste("rdatamaps/",uname,"/",resol,"/tmean_11.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(tmean_11)
  
  tmean_12<-crop(raster(paste("rdatamaps/world/",resol,"/tmean_12.tif",sep="")),Extent)
  writeRaster(tmean_12,filename=paste("rdatamaps/",uname,"/",resol,"/tmean_12.tif",sep=""),overwrite=T,datatype='FLT4S')
  
  ####GEOPHYS
  
  alt<-crop(raster(paste("rdatamaps/world/",resol,"/alt.tif",sep="")),Extent)
  writeRaster(alt,filename=paste("rdatamaps/",uname,"/",resol,"/alt.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(alt)
  
  aspect<-crop(raster(paste("rdatamaps/world/",resol,"/aspect.tif",sep="")),Extent)
  writeRaster(aspect,filename=paste("rdatamaps/",uname,"/",resol,"/aspect.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(aspect)
  
  eastness<-crop(raster(paste("rdatamaps/world/",resol,"/eastness.tif",sep="")),Extent)
  writeRaster(eastness,filename=paste("rdatamaps/",uname,"/",resol,"/eastness.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(eastness)
  
  northness<-crop(raster(paste("rdatamaps/world/",resol,"/northness.tif",sep="")),Extent)
  writeRaster(northness,filename=paste("rdatamaps/",uname,"/",resol,"/northness.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(northness)
  
  slope<-crop(raster(paste("rdatamaps/world/",resol,"/slope.tif",sep="")),Extent)
  writeRaster(slope,filename=paste("rdatamaps/",uname,"/",resol,"/slope.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(slope)
  
  ###EDAPH
  
  ref_depth<-crop(raster(paste("rdatamaps/world/",resol,"/ref_depth.tif",sep="")),Extent)
  writeRaster(ref_depth,filename=paste("rdatamaps/",uname,"/",resol,"/ref_depth.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(ref_depth)
  
  s_bs<-crop(raster(paste("rdatamaps/world/",resol,"/s_bs.tif",sep="")),Extent)
  writeRaster(s_bs,filename=paste("rdatamaps/",uname,"/",resol,"/s_bs.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_bs)
  
  s_caco3<-crop(raster(paste("rdatamaps/world/",resol,"/s_caco3.tif",sep="")),Extent)
  writeRaster(s_caco3,filename=paste("rdatamaps/",uname,"/",resol,"/s_caco3.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_caco3)
  
  s_caso4<-crop(raster(paste("rdatamaps/world/",resol,"/s_caso4.tif",sep="")),Extent)
  writeRaster(s_caso4,filename=paste("rdatamaps/",uname,"/",resol,"/s_caso4.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_caso4)
  
  s_cec_clay<-crop(raster(paste("rdatamaps/world/",resol,"/s_cec_clay.tif",sep="")),Extent)
  writeRaster(s_cec_clay,filename=paste("rdatamaps/",uname,"/",resol,"/s_cec_clay.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_cec_clay)
  
  s_cec_soil<-crop(raster(paste("rdatamaps/world/",resol,"/s_cec_soil.tif",sep="")),Extent)
  writeRaster(s_cec_soil,filename=paste("rdatamaps/",uname,"/",resol,"/s_cec_soil.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_cec_soil)
  
  s_clay<-crop(raster(paste("rdatamaps/world/",resol,"/s_clay.tif",sep="")),Extent)
  writeRaster(s_clay,filename=paste("rdatamaps/",uname,"/",resol,"/s_clay.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_clay)
  
  s_ece<-crop(raster(paste("rdatamaps/world/",resol,"/s_ece.tif",sep="")),Extent)
  writeRaster(s_ece,filename=paste("rdatamaps/",uname,"/",resol,"/s_ece.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_ece)
  
  s_esp<-crop(raster(paste("rdatamaps/world/",resol,"/s_esp.tif",sep="")),Extent)
  writeRaster(s_esp,filename=paste("rdatamaps/",uname,"/",resol,"/s_esp.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_esp)
  
  s_gravel<-crop(raster(paste("rdatamaps/world/",resol,"/s_gravel.tif",sep="")),Extent)
  writeRaster(s_gravel,filename=paste("rdatamaps/",uname,"/",resol,"/s_gravel.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_gravel)
  
  s_oc<-crop(raster(paste("rdatamaps/world/",resol,"/s_oc.tif",sep="")),Extent)
  writeRaster(s_oc,filename=paste("rdatamaps/",uname,"/",resol,"/s_oc.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_oc)
  
  s_ph_h2o<-crop(raster(paste("rdatamaps/world/",resol,"/s_ph_h2o.tif",sep="")),Extent)
  writeRaster(s_ph_h2o,filename=paste("rdatamaps/",uname,"/",resol,"/s_ph_h2o.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_ph_h2o)
  
  s_ref_bulk<-crop(raster(paste("rdatamaps/world/",resol,"/s_ref_bulk.tif",sep="")),Extent)
  writeRaster(s_ref_bulk,filename=paste("rdatamaps/",uname,"/",resol,"/s_ref_bulk.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_ref_bulk)
  
  s_sand<-crop(raster(paste("rdatamaps/world/",resol,"/s_sand.tif",sep="")),Extent)
  writeRaster(s_sand,filename=paste("rdatamaps/",uname,"/",resol,"/s_sand.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_sand)
  
  s_silt<-crop(raster(paste("rdatamaps/world/",resol,"/s_silt.tif",sep="")),Extent)
  writeRaster(s_silt,filename=paste("rdatamaps/",uname,"/",resol,"/s_silt.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_silt)
  
  s_teb<-crop(raster(paste("rdatamaps/world/",resol,"/s_teb.tif",sep="")),Extent)
  writeRaster(s_teb,filename=paste("rdatamaps/",uname,"/",resol,"/s_teb.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_teb)
  
  t_bs<-crop(raster(paste("rdatamaps/world/",resol,"/t_bs.tif",sep="")),Extent)
  writeRaster(t_bs,filename=paste("rdatamaps/",uname,"/",resol,"/t_bs.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_bs)
  
  t_caco3<-crop(raster(paste("rdatamaps/world/",resol,"/t_caco3.tif",sep="")),Extent)
  writeRaster(t_caco3,filename=paste("rdatamaps/",uname,"/",resol,"/t_caco3.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_caco3)
  
  t_caso4<-crop(raster(paste("rdatamaps/world/",resol,"/t_caso4.tif",sep="")),Extent)
  writeRaster(t_caso4,filename=paste("rdatamaps/",uname,"/",resol,"/t_caso4.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_caso4)
  
  t_cec_clay<-crop(raster(paste("rdatamaps/world/",resol,"/t_cec_clay.tif",sep="")),Extent)
  writeRaster(t_cec_clay,filename=paste("rdatamaps/",uname,"/",resol,"/t_cec_clay.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_cec_clay)
  
  t_cec_soil<-crop(raster(paste("rdatamaps/world/",resol,"/t_cec_soil.tif",sep="")),Extent)
  writeRaster(t_cec_soil,filename=paste("rdatamaps/",uname,"/",resol,"/t_cec_soil.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_cec_soil)
  
  t_clay<-crop(raster(paste("rdatamaps/world/",resol,"/t_clay.tif",sep="")),Extent)
  writeRaster(t_clay,filename=paste("rdatamaps/",uname,"/",resol,"/t_clay.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_clay)
  
  t_ece<-crop(raster(paste("rdatamaps/world/",resol,"/t_ece.tif",sep="")),Extent)
  writeRaster(t_ece,filename=paste("rdatamaps/",uname,"/",resol,"/t_ece.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_ece)
  
  t_esp<-crop(raster(paste("rdatamaps/world/",resol,"/t_esp.tif",sep="")),Extent)
  writeRaster(t_esp,filename=paste("rdatamaps/",uname,"/",resol,"/t_esp.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_esp)
  
  t_gravel<-crop(raster(paste("rdatamaps/world/",resol,"/t_gravel.tif",sep="")),Extent)
  writeRaster(t_gravel,filename=paste("rdatamaps/",uname,"/",resol,"/t_gravel.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_gravel)
  
  t_oc<-crop(raster(paste("rdatamaps/world/",resol,"/t_oc.tif",sep="")),Extent)
  writeRaster(t_oc,filename=paste("rdatamaps/",uname,"/",resol,"/t_oc.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_oc)
  
  t_ph_h2o<-crop(raster(paste("rdatamaps/world/",resol,"/t_ph_h2o.tif",sep="")),Extent)
  writeRaster(t_ph_h2o,filename=paste("rdatamaps/",uname,"/",resol,"/t_ph_h2o.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_ph_h2o)
  
  t_ref_bulk<-crop(raster(paste("rdatamaps/world/",resol,"/t_ref_bulk.tif",sep="")),Extent)
  writeRaster(t_ref_bulk,filename=paste("rdatamaps/",uname,"/",resol,"/t_ref_bulk.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_ref_bulk)
  
  t_sand<-crop(raster(paste("rdatamaps/world/",resol,"/t_sand.tif",sep="")),Extent)
  writeRaster(t_sand,filename=paste("rdatamaps/",uname,"/",resol,"/t_sand.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_sand)
  
  t_silt<-crop(raster(paste("rdatamaps/world/",resol,"/t_silt.tif",sep="")),Extent)
  writeRaster(t_silt,filename=paste("rdatamaps/",uname,"/",resol,"/t_silt.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_silt)
  
  t_teb<-crop(raster(paste("rdatamaps/world/",resol,"/t_teb.tif",sep="")),Extent)
  writeRaster(t_teb,filename=paste("rdatamaps/",uname,"/",resol,"/t_teb.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_teb)
  
  #Nuevas variables
  
  srad_1<-crop(raster(paste("rdatamaps/world/",resol,"/srad_1.tif",sep="")),Extent)
  writeRaster(srad_1,filename=paste("rdatamaps/",uname,"/",resol,"/srad_1.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(srad_1)
  
  srad_2<-crop(raster(paste("rdatamaps/world/",resol,"/srad_2.tif",sep="")),Extent)
  writeRaster(srad_2,filename=paste("rdatamaps/",uname,"/",resol,"/srad_2.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(srad_2)
  
  srad_3<-crop(raster(paste("rdatamaps/world/",resol,"/srad_3.tif",sep="")),Extent)
  writeRaster(srad_3,filename=paste("rdatamaps/",uname,"/",resol,"/srad_3.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(srad_3)
  
  srad_4<-crop(raster(paste("rdatamaps/world/",resol,"/srad_4.tif",sep="")),Extent)
  writeRaster(srad_4,filename=paste("rdatamaps/",uname,"/",resol,"/srad_4.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(srad_4)
  
  srad_5<-crop(raster(paste("rdatamaps/world/",resol,"/srad_5.tif",sep="")),Extent)
  writeRaster(srad_5,filename=paste("rdatamaps/",uname,"/",resol,"/srad_5.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(srad_5)
  
  srad_6<-crop(raster(paste("rdatamaps/world/",resol,"/srad_6.tif",sep="")),Extent)
  writeRaster(srad_6,filename=paste("rdatamaps/",uname,"/",resol,"/srad_6.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(srad_6)
  
  srad_7<-crop(raster(paste("rdatamaps/world/",resol,"/srad_7.tif",sep="")),Extent)
  writeRaster(srad_7,filename=paste("rdatamaps/",uname,"/",resol,"/srad_7.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(srad_7)
  
  srad_8<-crop(raster(paste("rdatamaps/world/",resol,"/srad_8.tif",sep="")),Extent)
  writeRaster(srad_8,filename=paste("rdatamaps/",uname,"/",resol,"/srad_8.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(srad_8)
  
  srad_9<-crop(raster(paste("rdatamaps/world/",resol,"/srad_9.tif",sep="")),Extent)
  writeRaster(srad_9,filename=paste("rdatamaps/",uname,"/",resol,"/srad_9.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(srad_9)
  
  srad_10<-crop(raster(paste("rdatamaps/world/",resol,"/srad_10.tif",sep="")),Extent)
  writeRaster(srad_10,filename=paste("rdatamaps/",uname,"/",resol,"/srad_10.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(srad_10)
  
  srad_11<-crop(raster(paste("rdatamaps/world/",resol,"/srad_11.tif",sep="")),Extent)
  writeRaster(srad_11,filename=paste("rdatamaps/",uname,"/",resol,"/srad_11.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(srad_11)
  
  srad_12<-crop(raster(paste("rdatamaps/world/",resol,"/srad_12.tif",sep="")),Extent)
  writeRaster(srad_12,filename=paste("rdatamaps/",uname,"/",resol,"/srad_12.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(srad_12)
  
  srad_annual<-crop(raster(paste("rdatamaps/world/",resol,"/srad_annual.tif",sep="")),Extent)
  writeRaster(srad_annual,filename=paste("rdatamaps/",uname,"/",resol,"/srad_annual.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(srad_annual)
  
  wind_1<-crop(raster(paste("rdatamaps/world/",resol,"/wind_1.tif",sep="")),Extent)
  writeRaster(wind_1,filename=paste("rdatamaps/",uname,"/",resol,"/wind_1.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(wind_1)
  
  wind_2<-crop(raster(paste("rdatamaps/world/",resol,"/wind_2.tif",sep="")),Extent)
  writeRaster(wind_2,filename=paste("rdatamaps/",uname,"/",resol,"/wind_2.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(wind_2)
  
  wind_3<-crop(raster(paste("rdatamaps/world/",resol,"/wind_3.tif",sep="")),Extent)
  writeRaster(wind_3,filename=paste("rdatamaps/",uname,"/",resol,"/wind_3.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(wind_3)
  
  wind_4<-crop(raster(paste("rdatamaps/world/",resol,"/wind_4.tif",sep="")),Extent)
  writeRaster(wind_4,filename=paste("rdatamaps/",uname,"/",resol,"/wind_4.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(wind_4)
  
  wind_5<-crop(raster(paste("rdatamaps/world/",resol,"/wind_5.tif",sep="")),Extent)
  writeRaster(wind_5,filename=paste("rdatamaps/",uname,"/",resol,"/wind_5.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(wind_5)
  
  wind_6<-crop(raster(paste("rdatamaps/world/",resol,"/wind_6.tif",sep="")),Extent)
  writeRaster(wind_6,filename=paste("rdatamaps/",uname,"/",resol,"/wind_6.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(wind_6)
  
  wind_7<-crop(raster(paste("rdatamaps/world/",resol,"/wind_7.tif",sep="")),Extent)
  writeRaster(wind_7,filename=paste("rdatamaps/",uname,"/",resol,"/wind_7.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(wind_7)
  
  wind_8<-crop(raster(paste("rdatamaps/world/",resol,"/wind_8.tif",sep="")),Extent)
  writeRaster(wind_8,filename=paste("rdatamaps/",uname,"/",resol,"/wind_8.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(wind_8)
  
  wind_9<-crop(raster(paste("rdatamaps/world/",resol,"/wind_9.tif",sep="")),Extent)
  writeRaster(wind_9,filename=paste("rdatamaps/",uname,"/",resol,"/wind_9.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(wind_9)
  
  wind_10<-crop(raster(paste("rdatamaps/world/",resol,"/wind_10.tif",sep="")),Extent)
  writeRaster(wind_10,filename=paste("rdatamaps/",uname,"/",resol,"/wind_10.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(wind_10)
  
  wind_11<-crop(raster(paste("rdatamaps/world/",resol,"/wind_11.tif",sep="")),Extent)
  writeRaster(wind_11,filename=paste("rdatamaps/",uname,"/",resol,"/wind_11.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(wind_11)
  
  wind_12<-crop(raster(paste("rdatamaps/world/",resol,"/wind_12.tif",sep="")),Extent)
  writeRaster(wind_12,filename=paste("rdatamaps/",uname,"/",resol,"/wind_12.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(wind_12)
  
  wind_annual<-crop(raster(paste("rdatamaps/world/",resol,"/wind_annual.tif",sep="")),Extent)
  writeRaster(wind_annual,filename=paste("rdatamaps/",uname,"/",resol,"/wind_annual.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(wind_annual)
  
  vapr_1<-crop(raster(paste("rdatamaps/world/",resol,"/vapr_1.tif",sep="")),Extent)
  writeRaster(vapr_1,filename=paste("rdatamaps/",uname,"/",resol,"/vapr_1.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(vapr_1)
  
  vapr_2<-crop(raster(paste("rdatamaps/world/",resol,"/vapr_2.tif",sep="")),Extent)
  writeRaster(vapr_2,filename=paste("rdatamaps/",uname,"/",resol,"/vapr_2.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(vapr_2)
  
  vapr_3<-crop(raster(paste("rdatamaps/world/",resol,"/vapr_3.tif",sep="")),Extent)
  writeRaster(vapr_3,filename=paste("rdatamaps/",uname,"/",resol,"/vapr_3.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(vapr_3)
  
  vapr_4<-crop(raster(paste("rdatamaps/world/",resol,"/vapr_4.tif",sep="")),Extent)
  writeRaster(vapr_4,filename=paste("rdatamaps/",uname,"/",resol,"/vapr_4.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(vapr_4)
  
  vapr_5<-crop(raster(paste("rdatamaps/world/",resol,"/vapr_5.tif",sep="")),Extent)
  writeRaster(vapr_5,filename=paste("rdatamaps/",uname,"/",resol,"/vapr_5.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(vapr_5)
  
  vapr_6<-crop(raster(paste("rdatamaps/world/",resol,"/vapr_6.tif",sep="")),Extent)
  writeRaster(vapr_6,filename=paste("rdatamaps/",uname,"/",resol,"/vapr_6.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(vapr_6)
  
  vapr_7<-crop(raster(paste("rdatamaps/world/",resol,"/vapr_7.tif",sep="")),Extent)
  writeRaster(vapr_7,filename=paste("rdatamaps/",uname,"/",resol,"/vapr_7.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(vapr_7)
  
  vapr_8<-crop(raster(paste("rdatamaps/world/",resol,"/vapr_8.tif",sep="")),Extent)
  writeRaster(vapr_8,filename=paste("rdatamaps/",uname,"/",resol,"/vapr_8.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(vapr_8)
  
  vapr_9<-crop(raster(paste("rdatamaps/world/",resol,"/vapr_9.tif",sep="")),Extent)
  writeRaster(vapr_9,filename=paste("rdatamaps/",uname,"/",resol,"/vapr_9.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(vapr_9)
  
  vapr_10<-crop(raster(paste("rdatamaps/world/",resol,"/vapr_10.tif",sep="")),Extent)
  writeRaster(vapr_10,filename=paste("rdatamaps/",uname,"/",resol,"/vapr_10.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(vapr_10)
  
  vapr_11<-crop(raster(paste("rdatamaps/world/",resol,"/vapr_11.tif",sep="")),Extent)
  writeRaster(vapr_11,filename=paste("rdatamaps/",uname,"/",resol,"/vapr_11.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(vapr_11)
  
  vapr_12<-crop(raster(paste("rdatamaps/world/",resol,"/vapr_12.tif",sep="")),Extent)
  writeRaster(vapr_12,filename=paste("rdatamaps/",uname,"/",resol,"/vapr_12.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(vapr_12)
  
  vapr_annual<-crop(raster(paste("rdatamaps/world/",resol,"/vapr_annual.tif",sep="")),Extent)
  writeRaster(vapr_annual,filename=paste("rdatamaps/",uname,"/",resol,"/vapr_annual.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(vapr_annual)
  
  #AWC1
  t_awc1<-crop(raster(paste("rdatamaps/world/",resol,"/t_awc1.tif",sep="")),Extent)
  s_awc1<-crop(raster(paste("rdatamaps/world/",resol,"/s_awc1.tif",sep="")),Extent)
  writeRaster(t_awc1,filename=paste("rdatamaps/",uname,"/",resol,"/t_awc1.tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(s_awc1,filename=paste("rdatamaps/",uname,"/",resol,"/s_awc1.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_awc1)
  rm(s_awc1)
  
  
  #AWC2
  t_awc2<-crop(raster(paste("rdatamaps/world/",resol,"/t_awc2.tif",sep="")),Extent)
  s_awc2<-crop(raster(paste("rdatamaps/world/",resol,"/s_awc2.tif",sep="")),Extent)
  writeRaster(t_awc2,filename=paste("rdatamaps/",uname,"/",resol,"/t_awc2.tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(s_awc2,filename=paste("rdatamaps/",uname,"/",resol,"/s_awc2.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_awc2)
  rm(t_awc2)
  
  #AWC3
  t_awc3<-crop(raster(paste("rdatamaps/world/",resol,"/t_awc3.tif",sep="")),Extent)
  s_awc3<-crop(raster(paste("rdatamaps/world/",resol,"/s_awc3.tif",sep="")),Extent)
  writeRaster(t_awc3,filename=paste("rdatamaps/",uname,"/",resol,"/t_awc3.tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(s_awc3,filename=paste("rdatamaps/",uname,"/",resol,"/s_awc3.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_awc3)
  rm(t_awc3)
  
  #AWCts
  t_awcts<-crop(raster(paste("rdatamaps/world/",resol,"/t_awcts.tif",sep="")),Extent)
  s_awcts<-crop(raster(paste("rdatamaps/world/",resol,"/s_awcts.tif",sep="")),Extent)
  writeRaster(t_awcts,filename=paste("rdatamaps/",uname,"/",resol,"/t_awcts.tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(s_awcts,filename=paste("rdatamaps/",uname,"/",resol,"/s_awcts.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_awcts)
  rm(t_awcts)
  
  #Depth rock
  depth_rock<-crop(raster(paste("rdatamaps/world/",resol,"/depth_rock.tif",sep="")),Extent)
  writeRaster(depth_rock,filename=paste("rdatamaps/",uname,"/",resol,"/depth_rock.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(depth_rock)
  
  #R horizon
  r_horizon<-crop(raster(paste("rdatamaps/world/",resol,"/r_horizon.tif",sep="")),Extent)
  writeRaster(r_horizon,filename=paste("rdatamaps/",uname,"/",resol,"/r_horizon.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(r_horizon)
  
  #Bulk density
  t_bulk_dens<-crop(raster(paste("rdatamaps/world/",resol,"/t_bulk_dens.tif",sep="")),Extent)
  s_bulk_dens<-crop(raster(paste("rdatamaps/world/",resol,"/s_bulk_dens.tif",sep="")),Extent)
  writeRaster(t_bulk_dens,filename=paste("rdatamaps/",uname,"/",resol,"/t_bulk_dens.tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(s_bulk_dens,filename=paste("rdatamaps/",uname,"/",resol,"/s_bulk_dens.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_bulk_dens)
  rm(t_bulk_dens)
  
  #Cec soil
  t_cecsol<-crop(raster(paste("rdatamaps/world/",resol,"/t_cecsol.tif",sep="")),Extent)
  s_cecsol<-crop(raster(paste("rdatamaps/world/",resol,"/s_cecsol.tif",sep="")),Extent)
  writeRaster(t_cecsol,filename=paste("rdatamaps/",uname,"/",resol,"/t_cecsol.tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(s_cecsol,filename=paste("rdatamaps/",uname,"/",resol,"/s_cecsol.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_cecsol)
  rm(t_cecsol)
  
  #Clay content
  t_clay_cont<-crop(raster(paste("rdatamaps/world/",resol,"/t_clay_cont.tif",sep="")),Extent)
  s_clay_cont<-crop(raster(paste("rdatamaps/world/",resol,"/s_clay_cont.tif",sep="")),Extent)
  writeRaster(t_clay_cont,filename=paste("rdatamaps/",uname,"/",resol,"/t_clay_cont.tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(s_clay_cont,filename=paste("rdatamaps/",uname,"/",resol,"/s_clay_cont.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_clay_cont)
  rm(t_clay_cont)
  
  #Coarse frag
  t_coarse_frag<-crop(raster(paste("rdatamaps/world/",resol,"/t_coarse_frag.tif",sep="")),Extent)
  s_coarse_frag<-crop(raster(paste("rdatamaps/world/",resol,"/s_coarse_frag.tif",sep="")),Extent)
  writeRaster(t_coarse_frag,filename=paste("rdatamaps/",uname,"/",resol,"/t_coarse_frag.tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(s_coarse_frag,filename=paste("rdatamaps/",uname,"/",resol,"/s_coarse_frag.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_coarse_frag)
  rm(t_coarse_frag)
  
  #OC density
  t_oc_dens<-crop(raster(paste("rdatamaps/world/",resol,"/t_oc_dens.tif",sep="")),Extent)
  s_oc_dens<-crop(raster(paste("rdatamaps/world/",resol,"/s_oc_dens.tif",sep="")),Extent)
  writeRaster(t_oc_dens,filename=paste("rdatamaps/",uname,"/",resol,"/t_oc_dens.tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(s_oc_dens,filename=paste("rdatamaps/",uname,"/",resol,"/s_oc_dens.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_oc_dens)
  rm(t_oc_dens)
  
  #OC stock
  t_oc_stock<-crop(raster(paste("rdatamaps/world/",resol,"/t_oc_stock.tif",sep="")),Extent)
  writeRaster(t_oc_stock,filename=paste("rdatamaps/",uname,"/",resol,"/t_oc_stock.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_oc_stock)
  
  s_oc_stock<-crop(raster(paste("rdatamaps/world/",resol,"/s_oc_stock.tif",sep="")),Extent)
  writeRaster(s_oc_stock,filename=paste("rdatamaps/",uname,"/",resol,"/s_oc_stock.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_oc_stock)
  
  #OC content
  t_oc_cont<-crop(raster(paste("rdatamaps/world/",resol,"/t_oc_cont.tif",sep="")),Extent)
  s_oc_cont<-crop(raster(paste("rdatamaps/world/",resol,"/s_oc_cont.tif",sep="")),Extent)
  writeRaster(t_oc_cont,filename=paste("rdatamaps/",uname,"/",resol,"/t_oc_cont.tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(s_oc_cont,filename=paste("rdatamaps/",uname,"/",resol,"/s_oc_cont.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_oc_cont)
  rm(t_oc_cont)
  
  #Ph h2O
  t_ph_hox<-crop(raster(paste("rdatamaps/world/",resol,"/t_ph_hox.tif",sep="")),Extent)
  s_ph_hox<-crop(raster(paste("rdatamaps/world/",resol,"/s_ph_hox.tif",sep="")),Extent)
  writeRaster(t_ph_hox,filename=paste("rdatamaps/",uname,"/",resol,"/t_ph_hox.tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(s_ph_hox,filename=paste("rdatamaps/",uname,"/",resol,"/s_ph_hox.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_ph_hox)
  rm(t_ph_hox)
  
  #Ph kcl
  t_ph_kcl<-crop(raster(paste("rdatamaps/world/",resol,"/t_ph_kcl.tif",sep="")),Extent)
  s_ph_kcl<-crop(raster(paste("rdatamaps/world/",resol,"/s_ph_kcl.tif",sep="")),Extent)
  writeRaster(t_ph_kcl,filename=paste("rdatamaps/",uname,"/",resol,"/t_ph_kcl.tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(s_ph_kcl,filename=paste("rdatamaps/",uname,"/",resol,"/s_ph_kcl.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_ph_kcl)
  rm(t_ph_kcl)
  
  #Sodicity
  sodicity<-crop(raster(paste("rdatamaps/world/",resol,"/sodicity.tif",sep="")),Extent)
  writeRaster(sodicity,filename=paste("rdatamaps/",uname,"/",resol,"/sodicity.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(sodicity)
  
  #Silt content
  t_silt_cont<-crop(raster(paste("rdatamaps/world/",resol,"/t_silt_cont.tif",sep="")),Extent)
  s_silt_cont<-crop(raster(paste("rdatamaps/world/",resol,"/s_silt_cont.tif",sep="")),Extent)
  writeRaster(t_silt_cont,filename=paste("rdatamaps/",uname,"/",resol,"/t_silt_cont.tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(s_silt_cont,filename=paste("rdatamaps/",uname,"/",resol,"/s_silt_cont.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(t_silt_cont)
  rm(s_silt_cont)
  
  #Sand content
  t_sand_cont<-crop(raster(paste("rdatamaps/world/",resol,"/t_sand_cont.tif",sep="")),Extent)
  s_sand_cont<-crop(raster(paste("rdatamaps/world/",resol,"/s_sand_cont.tif",sep="")),Extent)
  writeRaster(t_sand_cont,filename=paste("rdatamaps/",uname,"/",resol,"/t_sand_cont.tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(s_sand_cont,filename=paste("rdatamaps/",uname,"/",resol,"/s_sand_cont.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_sand_cont)
  rm(t_sand_cont)
  
  #Soil water capacity
  t_soilwater_cap<-crop(raster(paste("rdatamaps/world/",resol,"/t_soilwater_cap.tif",sep="")),Extent)
  s_soilwater_cap<-crop(raster(paste("rdatamaps/world/",resol,"/s_soilwater_cap.tif",sep="")),Extent)
  writeRaster(t_soilwater_cap,filename=paste("rdatamaps/",uname,"/",resol,"/t_soilwater_cap.tif",sep=""),overwrite=T,datatype='FLT4S')
  writeRaster(s_soilwater_cap,filename=paste("rdatamaps/",uname,"/",resol,"/s_soilwater_cap.tif",sep=""),overwrite=T,datatype='FLT4S')
  rm(s_soilwater_cap)
  rm(t_soilwater_cap)
  
  #puntos
  bioclimn<-ncell(tmean_12)
  bioclimp<-xyFromCell(tmean_12,c(1:bioclimn))
  
  bioclime<-extract(tmean_12,bioclimp)
  
  bioclim<-cbind(bioclimp,bioclime)
  bioclim<-na.omit(bioclim)
  bioclim<-bioclim[,c(1,2)]
  
  POINTID<-as.vector(1:length(bioclim[,1]))
  
  bioclimp<-data.frame(cbind(POINTID,bioclim[,c(1,2)]))
  colnames(bioclimp)[2]<-"POINT_X"
  colnames(bioclimp)[3]<-"POINT_Y"
  puntos<-bioclimp
  
  setwd(paste(ruta,"/rdatapoints",sep=""))
  dir.create(as.vector(paste(uname)))
  setwd(paste(ruta,"/rdatapoints/",uname,sep=""))
  dir.create(as.vector(paste(resol)))
  save(puntos,file=paste(resol,"/","base",resol,".RData",sep=""))
  rm(tmean_12)
}
