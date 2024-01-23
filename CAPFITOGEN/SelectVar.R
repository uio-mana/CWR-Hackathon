######################################################################
# Script  de la herramienta SelecVar - SelecVar tool script 
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

#1.Creating a report of parameters used 

{
  #Parametros
  setwd(paste(resultados))
  write(paste(), file="Parametros.Parameters.SelecVar.txt", append=TRUE)
  write("_________________________________________", file="Parametros.Parameters.SelecVar.txt", append=TRUE)
  write("______Herramienta/Tool SelecVar________", file="Parametros.Parameters.SelecVar.txt", append=TRUE)
  write("_________________________________________", file="Parametros.Parameters.SelecVar.txt", append=TRUE)
  write(paste("Lista de parametros usados en: ", date(),sep=""), file="Parametros.Parameters.SelecVar.txt", append=TRUE)
  write(paste("List of parameters used on: ", date(),sep=""), file="Parametros.Parameters.SelecVar.txt", append=TRUE)
  write(paste(), file="Parametros.Parameters.SelecVar.txt", append=TRUE)
  write(paste("ruta:", ruta,sep=""), file="Parametros.Parameters.SelecVar.txt", append=TRUE)
  write(paste("pais:", pais,sep=""), file="Parametros.Parameters.SelecVar.txt", append=TRUE)
  write(paste("pasaporte:", pasaporte,sep=""), file="Parametros.Parameters.SelecVar.txt", append=TRUE)
  write(paste("distdup:", distdup,sep=""), file="Parametros.Parameters.SelecVar.txt", append=TRUE)
  write(paste("geoqual:", geoqual,sep=""), file="Parametros.Parameters.SelecVar.txt", append=TRUE)
  write(paste("totalqual:", totalqual,sep=""), file="Parametros.Parameters.SelecVar.txt", append=TRUE)
  write(paste("buffy:", buffy,sep=""), file="Parametros.Parameters.SelecVar.txt", append=TRUE)
  write(paste("tamp:", tamp,sep=""), file="Parametros.Parameters.SelecVar.txt", append=TRUE)
  write(paste("resol1:", resol1,sep=""), file="Parametros.Parameters.SelecVar.txt", append=TRUE)
  write(paste("bioclimv:", bioclimv,sep=""), file="Parametros.Parameters.SelecVar.txt", append=TRUE)
  write(paste("edaphv:", edaphv,sep=""), file="Parametros.Parameters.SelecVar.txt", append=TRUE)
  write(paste("geophysv:", geophysv,sep=""), file="Parametros.Parameters.SelecVar.txt", append=TRUE)
  write(paste("latitud:", latitud,sep=""), file="Parametros.Parameters.SelecVar.txt", append=TRUE)
  write(paste("longitud:", longitud,sep=""), file="Parametros.Parameters.SelecVar.txt", append=TRUE)
  write(paste("percenRF:", percenRF,sep=""), file="Parametros.Parameters.SelecVar.txt", append=TRUE)
  write(paste("percenCorr:", percenCorr,sep=""), file="Parametros.Parameters.SelecVar.txt", append=TRUE)
  write(paste("CorrValue:", CorrValue,sep=""), file="Parametros.Parameters.SelecVar.txt", append=TRUE)
  write(paste("pValue:", pValue,sep=""), file="Parametros.Parameters.SelecVar.txt", append=TRUE)
  write(paste("nminvar:", nminvar,sep=""), file="Parametros.Parameters.SelecVar.txt", append=TRUE)
  write(paste("ecogeopcaxe:", ecogeopcaxe,sep=""), file="Parametros.Parameters.SelecVar.txt", append=TRUE)
  write(paste("resultados:", resultados,sep=""), file="Parametros.Parameters.SelecVar.txt", append=TRUE)
  
  #Rversion
  vvv<-R.Version()
  vvv<-as.numeric(vvv$year)
  
  #Determinar esa ruta como directorio de trabajo
  setwd(paste(ruta))
  write("______NUEVO PROCESO ECOGEO________", file="Error/process_info.txt", append=TRUE)
  write(date(), file="Error/process_info.txt", append=TRUE)
  #Ampliaci?n m?xima de la capacidad de uso de memoria ram
  #memory.size(max =TRUE)
  write("1.Terminado proceso de determinaci?n de directorio de trabajo", file="Error/process_info.txt", append=TRUE)
  
}

#2. Installing (in case of absence) and load required packages
#3. Checking the validity of introduced parameters

{
  #introducci?n tabla de lista de pa?ses y resoluciones de extracci?n a elegir y traducci?n
  #Definici?n pais
  #transformaci?n pais uppercase to lowercase
  pais<-tolower(pais)
  setwd(paste(ruta))
  #Definici?n pa?s
  #Evitar error por load de lista.paises.RData
  loadError<-FALSE
  abcd<-try(load("lista.paises.RData"),silent=TRUE)
  loadError <- (is(abcd, 'try-error')|is(abcd,'error'))
  if(loadError){
    Paises<-read.delim("lista.paises.txt")
  }
  rm(abcd)
  rm(loadError)
  
  pais<-data.frame(pais)
  colnames(pais)[1]<-"Paises"
  pais<-merge(Paises,pais,by="Paises",all.y=TRUE)
  pais<-paste(pais[1,2])
  pais<-tolower(pais)
  #Definici?n resoluci?n
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
  write("2.Terminado proceso carga de tablas de lista de pa?ses y resoluci?n", file="Error/process_info.txt", append=TRUE)
  #Uso primera vez (requiere instalar los paquetes)
  ##Elemento introducido por el usuario: primvez . Nota: Tener en cuenta que si en el mismo PC ya se ha instalado ELC mapas, no hace falta reinstalar paquetes
  #if(primvez){
  #if(!internet){
  #install.packages(c("Packages/sp_1.0-4.zip","Packages/raster_2.0-31.zip","Packages/maptools_0.8-20.zip","Packages/rgdal_0.7-22.zip","Packages/dismo_0.7-23","Packages/adegenet_1.3-8.zip","Packages/ade4_1.5-2.zip","Packages/labdsv_1.5-0.zip"), repos = NULL)
  #}
  #if(internet){
  #install.packages(c("sp","raster","maptools","rgdal","dismo","cluster","ade4","labdsv"), repos='http://cran.us.r-project.org',dep="Depends")
  #}
  #}
  write("3.Terminado proceso de instalaci?n de paquetes", file="Error/process_info.txt", append=TRUE)
  #activar paquetes ya instalados y necesarios
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
  if(system.file(package="cluster")==""){
    packages2<-append(packages2,"cluster")
  }
  if(system.file(package="ade4")==""){
    packages2<-append(packages2,"ade4")
  }
  if(system.file(package="labdsv")==""){
    packages2<-append(packages2,"labdsv")
  }
  if(system.file(package="mclust")==""){
    packages2<-append(packages2,"mclust")
  }
  if(system.file(package="clustvarsel")==""){
    packages2<-append(packages2,"clustvarsel")
  }
  if(system.file(package="randomForest")==""){
    packages2<-append(packages2,"randomForest")
  }
  
  #Instalar los que hagan falta
  if(length(packages2)>0){
    install.packages(setdiff(packages2, rownames(installed.packages())))
  }
  #Carga de paquetes
  library(sp)
  library(raster)
  library(maptools)
  library(rgdal)
  library(dismo)
  library(cluster)
  library(ade4)
  library(labdsv)
  library(mclust)
  library(clustvarsel)
  library(randomForest)
  
  write("4.Terminado proceso de carga de paquetes", file="Error/process_info.txt", append=TRUE)
  #########################
  
}

#4. Introducing and checking (including deletion of spatial duplicates) the occurrence data – Creating spatial objects with occurrences coordinates

{
  #introducci?n de pasaporte
  pasaporte<-read.delim(paste("Pasaporte/",pasaporte,sep=""))
  write("5.Terminado proceso de carga de tablas originales de pasaporte", file="Error/process_info.txt", append=TRUE)
  #Selecci?n de pasaportes sobre el umbral de geoqual
  pasaporte<-subset(pasaporte,!is.na(DECLATITUDE)&!is.na(DECLONGITUDE))
  if(geoqual==TRUE){
    pasaporte<-subset(pasaporte,TOTALQUAL100>=paste(totalqual))
  }
  write("6.Terminado proceso de fijaci?n de umbral de TOTALQUAL", file="Error/process_info.txt", append=TRUE)
  
  write("7.Iniciando proceso eliminaci?n duplicados espaciales", file="Error/process_info.txt", append=TRUE)
  
  #Eliminaci?n de duplicados espaciales
  puntosBG<-SpatialPointsDataFrame(pasaporte[,c("DECLONGITUDE","DECLATITUDE")],pasaporte,proj4string=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  write("7.1 Terminado generacion mapa de puntos BG (pasaporte) con coordenadas", file="Error/process_info.txt", append=TRUE)
  
  #Eliminando duplicados geogr?ficos
  if (mean(pasaporte$DECLATITUDE)<23){
    distdup1<-distdup*0.00833
  }
  if (mean(pasaporte$DECLATITUDE)>23&mean(pasaporte$DECLATITUDE)<45){
    distdup1<-distdup*0.00975
  }
  if (mean(pasaporte$DECLATITUDE)>45&mean(pasaporte$DECLATITUDE)<67){
    distdup1<-distdup*0.0127
  }
  if (mean(pasaporte$DECLATITUDE)>67){
    distdup1<-distdup*0.02299
  }
  puntosBG<-remove.duplicates(puntosBG,zero=distdup1)
  pasaporte<-puntosBG@data
  write("7.1 Terminado proceso de eliminaci?n de duplicados espaciales distdup", file="Error/process_info.txt", append=TRUE)
  rm(puntosBG)
  tabla<-pasaporte[,c(2,23,25)]
  
}

#5. Creating destinations (local folders) for results for each component (bioclimatic, edaphic and geophysical)

{
  #####################################################
  #Extracci?n ecogeogr?fica
  tabla1<-data.frame(tabla$ACCENUMB)
  colnames(tabla1)[1]<-"ACCENUMB"
  
  #Creaci?n de carpetas resultados
  setwd(paste(resultados))
  dir.create(as.vector(paste("BioclimaticVariables_",pais,sep="")))
  resultadosBioclim<-paste(resultados,"/BioclimaticVariables_",pais,sep="")
  dir.create(as.vector(paste("GeophysicVariables_",pais,sep="")))
  resultadosGeophysic<-paste(resultados,"/GeophysicVariables_",pais,sep="")
  dir.create(as.vector(paste("EdaphicVariables_",pais,sep="")))
  resultadosEdaphic<-paste(resultados,"/EdaphicVariables_",pais,sep="")
  setwd(paste(ruta))
  
}

#6. Creating stacks of raster layers (introducing selected ecogeographical layers)
#7. Extracting (characterizing) occurrences superimposing them to ecogeographic stacks 
#8. Analyzing the extracted information (i.e. deleting monomorphic variables or incomplete data)

{
  ###BIOCLIM
  #carga de lista de variables
  loadError<-FALSE
  abcd<-try(load("bioclim.RData"),silent=TRUE)
  loadError <- (is(abcd, 'try-error')|is(abcd,'error'))
  if(loadError){
    bioclim<-read.delim("bioclim.txt")
  }
  rm(abcd)
  rm(loadError)
  
  if(buffy){
    tamp<-tamp*0.008333
  }
  #Elemento introducido por el usuario: bioclimv
  #Traducci?n de variables desde java a lista de variables
  bioclim2<-1:length(bioclimv)
  bioclimv<-as.data.frame(cbind(bioclim2,bioclimv))
  colnames(bioclimv)[2]<-"VARDESCR"
  bioclimv<-merge(bioclim,bioclimv, by="VARDESCR", all=F)
  bioclimv<-as.character(bioclimv[,3])
  #armado de stacks
  biocliml<-list()
  for(i in 1:length(bioclimv)){
    biocliml[[i]]<-raster(paste("rdatamaps/",pais,"/",resol,"/",bioclimv[i],".tif",sep=""))
    names(biocliml[[i]])<-paste(bioclimv[i])
  }
  bioclimstack<-do.call("stack",biocliml)
  #Extracci?n de informaci?n
  #bioclim?tica  
  if(!buffy){
    bioclim<-extract(bioclimstack,tabla[,c("DECLONGITUDE","DECLATITUDE")])
  }
  if(buffy){
    bioclim<-extract(bioclimstack,tabla[,c("DECLONGITUDE","DECLATITUDE")],buffer=tamp,small=TRUE,fun=mean)
  }
  bioclim<-data.frame(tabla[,1],bioclim)
  colnames(bioclim)[1]<-"ACCENUMB"
  #consolidaci?n tabla bioclim?tica
  ecogeot1<-merge(tabla1,bioclim,by="ACCENUMB",all.x=TRUE)
  setwd(paste(resultados))
  write.table(ecogeot1,file=paste(resultadosBioclim,"/Bioclim_extractedValues.txt",sep=""), sep = "\t", row.names = FALSE, qmethod = "double")
  write.table(ecogeot1,file=paste(resultadosBioclim,"/Bioclim_extractedValues.xls",sep=""), sep = "\t", row.names = FALSE, qmethod = "double")
  setwd(paste(ruta))
  ###GEOPHYS
  loadError<-FALSE
  abcd<-try(load("geophys.RData"),silent=TRUE)
  loadError <- (is(abcd, 'try-error')|is(abcd,'error'))
  if(loadError){
    geophys<-read.delim("geophys.txt")
  }
  rm(abcd)
  rm(loadError)
  
  #Elemento introducido por el usuario: geophysv
  #Traducci?n de variables desde java a lista de variables
  geophys2<-1:length(geophysv)
  geophysv<-as.data.frame(cbind(geophys2,geophysv))
  colnames(geophysv)[2]<-"VARDESCR"
  geophysv<-merge(geophys,geophysv, by="VARDESCR", all=F,sort=FALSE)
  geophysv<-as.character(geophysv[,3])
  #armado de stacks
  geophysl<-list()
  for(i in 1:length(geophysv)){
    geophysl[[i]]<-raster(paste("rdatamaps/",pais,"/",resol,"/",geophysv[i],".tif",sep=""))
    names(geophysl[[i]])<-paste(geophysv[i])
  }
  geophysstack<-do.call("stack",geophysl)
  #Extracci?n de informaci?n
  #geof?sica    
  if(!buffy){
    geophys<-extract(geophysstack,tabla[,c("DECLONGITUDE","DECLATITUDE")])
  }
  if(buffy){
    geophys<-extract(geophysstack,tabla[,c("DECLONGITUDE","DECLATITUDE")],buffer=tamp,small=TRUE,fun=mean)
  }
  geophys<-data.frame(tabla[,1],geophys)
  colnames(geophys)[1]<-"ACCENUMB"
  #consolidaci?n tabla geof?sica
  ecogeot2<-merge(tabla1,geophys,by="ACCENUMB",all.x=TRUE,sort=FALSE)
  if(latitud){
    ecogeot2<-cbind(ecogeot2,tabla[,2])
    colnames(ecogeot2)[ncol(ecogeot2)]<-"DECLATITUDE"
  }
  if(longitud){
    ecogeot2<-cbind(ecogeot2,tabla[,3])
    colnames(ecogeot2)[ncol(ecogeot2)]<-"DECLONGITUDE"
  }
  setwd(paste(resultados))
  write.table(ecogeot2, file = paste(resultadosGeophysic,"/Geophysic_extractedValues.txt",sep=""), sep = "\t", row.names = FALSE, qmethod = "double")
  write.table(ecogeot2, file = paste(resultadosGeophysic,"/Geophysic_extractedValues.xls",sep=""), sep = "\t", row.names = FALSE, qmethod = "double")
  setwd(paste(ruta))
  ###EDAPHIC
  loadError<-FALSE
  abcd<-try(load("edaph.RData"),silent=TRUE)
  loadError <- (is(abcd, 'try-error')|is(abcd,'error'))
  if(loadError){
    edaph<-read.delim("edaph.txt")
  }
  rm(abcd)
  rm(loadError)
  
  #Elemento introducido por el usuario: edaphv
  edaph2<-1:length(edaphv)
  edaphv<-as.data.frame(cbind(edaph2,edaphv))
  colnames(edaphv)[2]<-"VARDESCR"
  edaphv<-merge(edaph,edaphv, by="VARDESCR", all=F)
  edaphv<-as.character(edaphv[,3])
  #armado de stacks
  edaphl<-list()
  for(i in 1:length(edaphv)){
    edaphl[[i]]<-raster(paste("rdatamaps/",pais,"/",resol,"/",edaphv[i],".tif",sep=""))
    names(edaphl[[i]])<-paste(edaphv[i])
  }
  edaphstack<-do.call("stack",edaphl)
  #Extracci?n de informaci?n
  #ed?fica    
  if(!buffy){
    edaph<-extract(edaphstack,tabla[,c("DECLONGITUDE","DECLATITUDE")])
  }
  if(buffy){
    edaph<-extract(edaphstack,tabla[,c("DECLONGITUDE","DECLATITUDE")],buffer=tamp,small=TRUE,fun=mean)
  }
  edaph<-data.frame(tabla[,1],edaph)
  colnames(edaph)[1]<-"ACCENUMB"
  #consolidaci?n tabla ed?fica
  ecogeot3<-merge(tabla1,edaph,by="ACCENUMB",all.x=TRUE)
  setwd(paste(resultados))
  write.table(ecogeot3, file = paste(resultadosEdaphic,"/Edaphic_extractedValues.txt",sep=""), sep = "\t", row.names = FALSE, qmethod = "double")
  write.table(ecogeot3, file = paste(resultadosEdaphic,"/Edaphic_extractedValues.xls",sep=""), sep = "\t", row.names = FALSE, qmethod = "double")
  
}

#9. Adjusting the ecogeographic information extracted for further analysis

{
  #########################################
  #Selecci?n de Variables##################
  #########################################
  #eliminaci?n accesiones con NA's - Bioclim
  borrar<-complete.cases(ecogeot1)
  ecogeot1b<-list()
  for (i in 1:nrow(ecogeot1)){
    if(borrar[i]){
      ecogeot1b[[i]]<-ecogeot1[i,]
    }
  }
  ecogeot1b<-do.call("rbind",ecogeot1b)
  
  entradasBIO<-data.frame(ecogeot1b[,1])
  colnames(entradasBIO)[1]<-"ACCENUMB_without_NA_values"
  write.table(entradasBIO, file = paste(resultadosBioclim,"/accessions_used_bioclimatic_varselection.txt",sep=""), sep = "\t", row.names = FALSE, qmethod = "double")
  write.table(entradasBIO, file = paste(resultadosBioclim,"/accessions_used_bioclimatic_varselection.xls",sep=""), sep = "\t", row.names = FALSE, qmethod = "double")
  #eliminaci?n accesiones con NA's - Geophys
  
  borrar<-complete.cases(ecogeot2)
  ecogeot2g<-list()
  for (i in 1:nrow(ecogeot2)){
    if(borrar[i]){
      ecogeot2g[[i]]<-ecogeot2[i,]
    }
  }
  ecogeot2g<-do.call("rbind",ecogeot2g)
  
  entradasGEO<-data.frame(ecogeot2g[,1])
  colnames(entradasGEO)[1]<-"ACCENUMB_without_NA_values"
  write.table(entradasGEO, file = paste(resultadosGeophysic,"/accessions_used_geophysic_varselection.txt",sep=""), sep = "\t", row.names = FALSE, qmethod = "double")
  write.table(entradasGEO, file = paste(resultadosGeophysic,"/accessions_used_geophysic_varselection.xls",sep=""), sep = "\t", row.names = FALSE, qmethod = "double")
  #eliminaci?n accesiones con NA's - Edaph
  
  borrar<-complete.cases(ecogeot3)
  ecogeot3e<-list()
  for (i in 1:nrow(ecogeot3)){
    if(borrar[i]){
      ecogeot3e[[i]]<-ecogeot3[i,]
    }
  }
  ecogeot3e<-do.call("rbind",ecogeot3e)
  
  entradasEDA<-data.frame(ecogeot3e[,1])
  colnames(entradasEDA)[1]<-"ACCENUMB_without_NA_values"
  write.table(entradasEDA, file = paste(resultadosEdaphic,"/accessions_used_edaphic_varselection.txt",sep=""), sep = "\t", row.names = FALSE, qmethod = "double")
  write.table(entradasEDA, file = paste(resultadosEdaphic,"/accessions_used_edaphic_varselection.xls",sep=""), sep = "\t", row.names = FALSE, qmethod = "double")
  setwd(paste(ruta))
  write("9.Terminado proceso de eliminaci?n entradas con NAs", file="Error/process_info.txt", append=TRUE)
  
  #Eliminacion de variables monomorficas
  elimono<-list()
  for(i in 1:ncol(ecogeot1b)){
    aaa<-unique(ecogeot1b[,i])
    if(length(aaa)==1){
      elimono[[i]]<-i*-1
    }
  }
  elimono<-do.call("c",elimono)
  if(!is.null(elimono)){
    ecogeot1b<-ecogeot1b[,elimono]
  }
  if(is.null(elimono)){
    ecogeot1b<-ecogeot1b
  }
  elimono<-list()
  for(i in 1:ncol(ecogeot2g)){
    aaa<-unique(ecogeot2g[,i])
    if(length(aaa)==1){
      elimono[[i]]<-i*-1
    }
  }
  elimono<-do.call("c",elimono)
  if(!is.null(elimono)){
    ecogeot2g<-ecogeot2g[,elimono]
  }
  if(is.null(elimono)){
    ecogeot2g<-ecogeot2g
  }
  elimono<-list()
  for(i in 1:ncol(ecogeot3e)){
    aaa<-unique(ecogeot3e[,i])
    if(length(aaa)==1){
      elimono[[i]]<-i*-1
    }
  }
  elimono<-do.call("c",elimono)
  if(!is.null(elimono)){
    ecogeot3e<-ecogeot3e[,elimono]
  }
  if(is.null(elimono)){
    ecogeot3e<-ecogeot3e
  }
  
}

#10.Applying RF classification to obtain MDA values, ranking and selecting the most important variables according to the user’s parameters 

{
  ###########################################################
  #################var selection#############################
  
  #########################RF############################
  #Bioclimatico
  bioclim1<-ecogeot1b
  geophys1<-ecogeot2g
  edaph1<-ecogeot3e
  rm(ecogeot1b)
  rm(ecogeot2g)
  rm(ecogeot3e)
  ######
  #Inicia proceso RF
  seed<-round(length(bioclim1[,1])/10,0)
  if(seed<1){
    seed<-1
  }
  set.seed(seed)
  Variable.importance.bioclim<-randomForest(bioclim1[,-1],ntree=1000,keep.forest=FALSE,importance=TRUE)
  importancia.bio<-importance(Variable.importance.bioclim)
  importancia.bio<-data.frame(rownames(importancia.bio),importancia.bio)
  colnames(importancia.bio)[1]<-"Variables"
  setwd(paste(resultadosBioclim))
  dir.create(as.vector(paste("RandomForest",sep="")))
  RFBIOCLIM<-paste(resultadosBioclim,"/RandomForest",sep="")
  setwd(paste(RFBIOCLIM))
  importancia.bio<-importancia.bio[order(importancia.bio$MeanDecreaseAccuracy,decreasing=TRUE),]
  write.table(importancia.bio, file = "VariableImportance_RandomForest_bioclim.txt", sep = "\t", row.names = FALSE, qmethod = "double")
  write.table(importancia.bio, file = "VariableImportance_RandomForest_bioclim.xls", sep = "\t", row.names = FALSE, qmethod = "double")
  jpeg(file = "VarImportanceDotChart_bio.jpeg")
  grafica.bio<-varImpPlot(Variable.importance.bioclim)
  dev.off()
  setwd(paste(ruta))
  #Geophys
  seed<-round(length(geophys1[,1])/10,0)
  if(seed<1){
    seed<-1
  }
  set.seed(seed)
  Variable.importance.geophysic<-randomForest(geophys1[,-1],ntree=1000,keep.forest=FALSE,importance=TRUE)
  importancia.geo<-importance(Variable.importance.geophysic)
  importancia.geo<-data.frame(rownames(importancia.geo),importancia.geo)
  colnames(importancia.geo)[1]<-"Variables"
  setwd(paste(resultadosGeophysic))
  dir.create(as.vector(paste("RandomForest",sep="")))
  RFGEOPHYS<-paste(resultadosGeophysic,"/RandomForest",sep="")
  setwd(paste(RFGEOPHYS))
  importancia.geo<-importancia.geo[order(importancia.geo$MeanDecreaseAccuracy,decreasing=TRUE),]
  write.table(importancia.geo, file = "VariableImportance_RandomForest_geophysic.txt", sep = "\t", row.names = FALSE, qmethod = "double")
  write.table(importancia.geo, file = "VariableImportance_RandomForest_geophysic.xls", sep = "\t", row.names = FALSE, qmethod = "double")
  jpeg(file = "VarImportanceDotChart_geo.jpeg")
  grafica.geo<-varImpPlot(Variable.importance.geophysic)
  dev.off()
  setwd(paste(ruta))
  #Edaphic
  seed<-round(length(edaph1[,1])/10,0)
  if(seed<1){
    seed<-1
  }
  set.seed(seed)
  Variable.importance.edaphic<-randomForest(edaph1[,-1],ntree=1000,keep.forest=FALSE,importance=TRUE)
  importancia.eda<-importance(Variable.importance.edaphic)
  importancia.eda<-data.frame(rownames(importancia.eda),importancia.eda)
  colnames(importancia.eda)[1]<-"Variables"
  setwd(paste(resultadosEdaphic))
  dir.create(as.vector(paste("RandomForest",sep="")))
  RFEDAPHIC<-paste(resultadosEdaphic,"/RandomForest",sep="")
  setwd(paste(RFEDAPHIC))
  importancia.eda<-importancia.eda[order(importancia.eda$MeanDecreaseAccuracy,decreasing=TRUE),]
  write.table(importancia.eda, file = "VariableImportance_RandomForest_edaphic.txt", sep = "\t", row.names = FALSE, qmethod = "double")
  write.table(importancia.eda, file = "VariableImportance_RandomForest_edaphic.xls", sep = "\t", row.names = FALSE, qmethod = "double")
  jpeg(file = "VarImportanceDotChart_eda.jpeg")
  grafica.geo<-varImpPlot(Variable.importance.edaphic)
  dev.off()
  setwd(paste(ruta))
  
  #Inicia Seleccion de las variables importantes por RF
  #Bioclim
  dividebio<-round(percenRF*(nrow(importancia.bio)))
  if(dividebio<nminvar){
    dividebio<-nminvar
  }
  importancia.bio2<-as.character(importancia.bio[1:dividebio,1])
  #Geophys
  dividegeo<-round(percenRF*(nrow(importancia.geo)))
  if(dividegeo<nminvar){
    dividegeo<-nminvar
  }
  importancia.geo2<-as.character(importancia.geo[1:dividegeo,1])
  #Edaph
  divideeda<-round(percenRF*(nrow(importancia.eda)))
  if(divideeda<nminvar){
    divideeda<-nminvar
  }
  importancia.eda2<-as.character(importancia.eda[1:divideeda,1])
  rm(dividebio)
  rm(dividegeo)
  rm(divideeda)
  
}
  
#11. Applying bivariate correlation analysis on RF most important variables

{
  
  ########################Bivariadas#####################
  setwd(paste(resultados))
  
  #BIOCLIM Se seleccionan s?lo las variables importantes por RF (porcentaje o fraccion seleccionada)
  ecogeot1<-bioclim1[,paste(importancia.bio2)]
  
  #Luego, parte normal
  corrBIO.estimate<- as.data.frame(matrix(nrow = ncol(ecogeot1), ncol = ncol(ecogeot1)))
  colnames(corrBIO.estimate)<-colnames(ecogeot1)
  rownames(corrBIO.estimate)<-colnames(ecogeot1)
  corrBIO.pvalue<- as.data.frame(matrix(nrow = ncol(ecogeot1), ncol = ncol(ecogeot1)))
  colnames(corrBIO.pvalue)<-colnames(ecogeot1)
  rownames(corrBIO.pvalue)<-colnames(ecogeot1)
  corrBIO.confint<- as.data.frame(matrix(nrow = ncol(ecogeot1), ncol = ncol(ecogeot1)))
  colnames(corrBIO.confint)<-colnames(ecogeot1)
  rownames(corrBIO.confint)<-colnames(ecogeot1)
  bbb<- as.data.frame(matrix(nrow = ncol(ecogeot1), ncol = ncol(ecogeot1)))
  ccc<- as.data.frame(matrix(nrow = ncol(ecogeot1), ncol = ncol(ecogeot1)))
  
  for(j in 1:ncol(ecogeot1)){
    for(k in 1:ncol(ecogeot1)){
      if(j!=k){
        aaa<-cor.test(ecogeot1[,j],ecogeot1[,k])
        corrBIO.estimate[j,k]<-round(aaa$estimate,3)
        corrBIO.pvalue[j,k]<-round(aaa$p.value,3)
        if(aaa$p.value<=pValue){
          bbb[j,k]<-1
        }
        if(aaa$p.value>pValue){
          bbb[j,k]<-0
        }
        if(aaa$estimate>=CorrValue|aaa$estimate<= -CorrValue){
          ccc[j,k]<-round(aaa$estimate,3)
        }
        if(aaa$estimate<CorrValue&aaa$estimate> -CorrValue){
          ccc[j,k]<-0
        }
        aaa<-paste(round(aaa$conf.int[1],3),round(aaa$conf.int[2],3),sep=" / ")
        corrBIO.confint[j,k]<-aaa
      }
      if(j==k){
        corrBIO.estimate[j,k]<-1
        corrBIO.pvalue[j,k]<-NA
        corrBIO.confint[j,k]<-NA
        bbb[j,k]<-0
        ccc[j,k]<-0
      }
    }
  }
  aaa<-bbb*ccc
  colnames(aaa)<-colnames(ecogeot1)
  rownames(aaa)<-colnames(ecogeot1)
  
  #Guardar resultados
  setwd(paste(resultadosBioclim))
  dir.create(as.vector(paste("BivariateCorrelations",sep="")))
  BivariateCorrelationsBioclim<-paste(resultadosBioclim,"/BivariateCorrelations",sep="")
  setwd(paste(BivariateCorrelationsBioclim))
  write.table(corrBIO.estimate, file = paste("Estimate_correlation_bioclim.xls",sep=""), sep = "\t", row.names = TRUE, qmethod = "double")
  write.table(corrBIO.pvalue, file = paste("Pvalue_correlation_bioclim.xls",sep=""), sep = "\t", row.names = TRUE, qmethod = "double")
  write.table(corrBIO.confint, file = paste("ConfidenceInterval_correlation_bioclim.xls",sep=""), sep = "\t", row.names = TRUE, qmethod = "double")
  write.table(aaa, file = paste("RelevantValues_correlation_bioclim.xls",sep=""), sep = "\t", row.names = TRUE, qmethod = "double")
  
  #Ahora selecci?n de variables m?s importantes por RF (de menor a mayor importancia) no correlacionadas 
  romper<-round(percenCorr*nrow(aaa))
  if(romper<nminvar){
    romper<-nminvar
  }
  #A?adir seed
  x<-nrow(aaa)
  #Funcion repeat
  repeat{
    if(nrow(aaa)<=romper|x==0){
      print("repeat loop ends");
      break
    }
    if(all(aaa[x,]==0)){
      x<-x-1
      next
    }
    if(any(abs(aaa[x,])>0)){
      aaa<-aaa[-1*x,-1*x]
      x<-nrow(aaa)
    }
  }
  
}

#12. PCA for the complete list of variables per component and position of each RF important variable on each principal component (eigen vector)

{
  #PCA-Bioclim
  ecogeopcaxebio<-ecogeopcaxe
  if(ecogeopcaxe>ncol(bioclim1)-1){
    ecogeopcaxebio<-ncol(bioclim1)-1
  }
  ecogeopcab<-dudi.pca(bioclim1[,-1],center=TRUE,scale=TRUE,scannf=FALSE,nf=ecogeopcaxebio)
  variablesb<-colnames(bioclim1)[-1]
  accenumbb<-bioclim1[,1]
  eigenvalues<-ecogeopcab$eig
  eigenvectors<-ecogeopcab$c1
  eigenvalues<-data.frame(c(1:length(eigenvalues)),eigenvalues)
  colnames(eigenvalues)[1]<-"Component"
  variancet<-sum(eigenvalues[,2])
  variance<-vector(length=length(eigenvalues[,1]))
  var.accumul<-vector(length=length(eigenvalues[,1]))
  for (j in 1:length(eigenvalues[,1])){
    variance[j]<-(eigenvalues[j,2]*100)/variancet
    if(j==1){
      var.accumul[j]<-variance[j]
    }
    if(j>1){
      var.accumul[j]<-var.accumul[j-1]+variance[j]
    }
  }
  eigenvalues<-data.frame(eigenvalues,variance,var.accumul)
  eigenvectors<-data.frame(variablesb,eigenvectors)
  pcascores<-data.frame(accenumbb,ecogeopcab$li)
  
  setwd(paste(resultadosBioclim))
  dir.create(as.vector(paste("PCA",sep="")))
  PCABIOCLIM<-paste(resultadosBioclim,"/PCA",sep="")
  setwd(paste(PCABIOCLIM))
  write.table(eigenvalues,file=paste("Bioclim_eigenvalues.xls",sep=""), sep = "\t", row.names = FALSE, qmethod = "double")
  write.table(eigenvectors,file=paste("Bioclim_eigenvectors.xls",sep=""), sep = "\t", row.names = FALSE, qmethod = "double")
  
  #Localizaci?n variables importantes en PCA eigenvectors
  importancia.bio2<-rownames(aaa)
  eigenvectors2<-eigenvectors[order(abs(eigenvectors$CS1),decreasing=TRUE),]
  eigenvectors2$CS1<-c(1:nrow(eigenvectors))
  eigenvectors2<-eigenvectors2[order(abs(eigenvectors$CS2),decreasing=TRUE),]
  eigenvectors2$CS2<-c(1:nrow(eigenvectors))
  eigenvectors2<-eigenvectors2[order(abs(eigenvectors$CS3),decreasing=TRUE),]
  eigenvectors2$CS3<-c(1:nrow(eigenvectors))
  eigenvectors3<-list()
  for(j in 1:length(importancia.bio2)){
    eigenvectors3[[j]]<-subset(eigenvectors2,eigenvectors2$variablesb==importancia.bio2[j])
  }
  eigenvectors3<-do.call("rbind",eigenvectors3)
  eigenvectors3<-eigenvectors3[,1:4]
  write.table(eigenvectors3,file=paste("Bioclim_IVposition.xls",sep=""), sep = "\t", row.names = FALSE, qmethod = "double")
  rm(eigenvalues)
  rm(eigenvectors)
  rm(eigenvectors2)
  rm(eigenvectors3) 
}

#13. Creating and exporting the list of suggested important variables for each component

{
  ################################
  #Tabla final de variables seleccionadas
  setwd(paste(ruta))
  vvv<-R.Version()
  vvv<-as.numeric(vvv$year)
  #Condicional figvartotal por si no lo abre v?a load.RData
  loadError<-FALSE
  abcd<-try(load("figvartotal.RData"),silent=TRUE)
  loadError <- (is(abcd, 'try-error')|is(abcd,'error'))
  if(loadError){
    figvartotal<-read.delim("figvartotal.txt")
  }
  rm(abcd)
  rm(loadError)
  
  aaa<-colnames(aaa)
  aaa<-figvartotal[figvartotal$VARCODE %in% aaa, ]
  setwd(paste(resultados))
  write.table(aaa, file = paste("SelectedVariables_bioclim.xls",sep=""), sep = "\t", row.names = FALSE, qmethod = "double")
  rm(aaa)
  rm(resultadosBioclim)
  rm(RFBIOCLIM)
  rm(importancia.bio)
  
  
  
  #GEOPHYS Se seleccionan s?lo las variables importantes por RF (porcentaje o fraccion seleccionada)
  ecogeot1<-geophys1[,paste(importancia.geo2)]
  #ahora parte normal de Corr de SelecVar
  corrBIO.estimate<- as.data.frame(matrix(nrow = ncol(ecogeot1), ncol = ncol(ecogeot1)))
  colnames(corrBIO.estimate)<-colnames(ecogeot1)
  rownames(corrBIO.estimate)<-colnames(ecogeot1)
  corrBIO.pvalue<- as.data.frame(matrix(nrow = ncol(ecogeot1), ncol = ncol(ecogeot1)))
  colnames(corrBIO.pvalue)<-colnames(ecogeot1)
  rownames(corrBIO.pvalue)<-colnames(ecogeot1)
  corrBIO.confint<- as.data.frame(matrix(nrow = ncol(ecogeot1), ncol = ncol(ecogeot1)))
  colnames(corrBIO.confint)<-colnames(ecogeot1)
  rownames(corrBIO.confint)<-colnames(ecogeot1)
  bbb<- as.data.frame(matrix(nrow = ncol(ecogeot1), ncol = ncol(ecogeot1)))
  ccc<- as.data.frame(matrix(nrow = ncol(ecogeot1), ncol = ncol(ecogeot1)))
  for(j in 1:ncol(ecogeot1)){
    for(k in 1:ncol(ecogeot1)){
      if(j!=k){
        aaa<-cor.test(ecogeot1[,j],ecogeot1[,k])
        corrBIO.estimate[j,k]<-round(aaa$estimate,3)
        corrBIO.pvalue[j,k]<-round(aaa$p.value,3)
        if(aaa$p.value<=pValue){
          bbb[j,k]<-1
        }
        if(aaa$p.value>pValue){
          bbb[j,k]<-0
        }
        if(aaa$estimate>=CorrValue|aaa$estimate<= -CorrValue){
          ccc[j,k]<-round(aaa$estimate,3)
        }
        if(aaa$estimate<CorrValue&aaa$estimate> -CorrValue){
          ccc[j,k]<-0
        }
        aaa<-paste(round(aaa$conf.int[1],3),round(aaa$conf.int[2],3),sep=" / ")
        corrBIO.confint[j,k]<-aaa
      }
      if(j==k){
        corrBIO.estimate[j,k]<-1
        corrBIO.pvalue[j,k]<-NA
        corrBIO.confint[j,k]<-NA
        bbb[j,k]<-0
        ccc[j,k]<-0
      }
    }
  }
  aaa<-bbb*ccc
  colnames(aaa)<-colnames(ecogeot1)
  rownames(aaa)<-colnames(ecogeot1)
  
  setwd(paste(resultadosGeophysic))
  dir.create(as.vector(paste("BivariateCorrelations",sep="")))
  BivariateCorrelationsGeophysic<-paste(resultadosGeophysic,"/BivariateCorrelations",sep="")
  setwd(paste(BivariateCorrelationsGeophysic))
  
  write.table(corrBIO.estimate, file = paste("Estimate_correlation_geophysic.xls",sep=""), sep = "\t", row.names = TRUE, qmethod = "double")
  write.table(corrBIO.pvalue, file = paste("Pvalue_correlation_geophysic.xls",sep=""), sep = "\t", row.names = TRUE, qmethod = "double")
  write.table(corrBIO.confint, file = paste("ConfidenceInterval_correlation_geophysic.xls",sep=""), sep = "\t", row.names = TRUE, qmethod = "double")
  write.table(aaa, file = paste("RelevantValues_correlation_geophysic.xls",sep=""), sep = "\t", row.names = TRUE, qmethod = "double")
  
  #Ahora selecci?n de variables m?s importantes por RF (de menor a mayor importancia) no correlacionadas 
  romper<-round(percenCorr*nrow(aaa))
  if(romper<nminvar){
    romper<-nminvar
  }
  #A?adir seed
  x<-nrow(aaa)
  #Funcion repeat
  repeat{
    if(nrow(aaa)<=romper|x==0){
      print("repeat loop ends");
      break
    }
    if(all(aaa[x,]==0)){
      x<-x-1
      next
    }
    if(any(abs(aaa[x,])>0)){
      aaa<-aaa[-1*x,-1*x]
      x<-nrow(aaa)
    }
  }
  
  #PCA-Geophys
  ecogeopcaxebio<-ecogeopcaxe
  if(ecogeopcaxe>ncol(geophys1)-1){
    ecogeopcaxebio<-ncol(geophys1)-1
  }
  ecogeopcab<-dudi.pca(geophys1[,-1],center=TRUE,scale=TRUE,scannf=FALSE,nf=ecogeopcaxebio)
  variablesb<-colnames(geophys1)[-1]
  accenumbb<-geophys1[,1]
  eigenvalues<-ecogeopcab$eig
  eigenvectors<-ecogeopcab$c1
  eigenvalues<-data.frame(c(1:length(eigenvalues)),eigenvalues)
  colnames(eigenvalues)[1]<-"Component"
  variancet<-sum(eigenvalues[,2])
  variance<-vector(length=length(eigenvalues[,1]))
  var.accumul<-vector(length=length(eigenvalues[,1]))
  for (j in 1:length(eigenvalues[,1])){
    variance[j]<-(eigenvalues[j,2]*100)/variancet
    if(j==1){
      var.accumul[j]<-variance[j]
    }
    if(j>1){
      var.accumul[j]<-var.accumul[j-1]+variance[j]
    }
  }
  eigenvalues<-data.frame(eigenvalues,variance,var.accumul)
  eigenvectors<-data.frame(variablesb,eigenvectors)
  pcascores<-data.frame(accenumbb,ecogeopcab$li)
  
  setwd(paste(resultadosGeophysic))
  dir.create(as.vector(paste("PCA",sep="")))
  PCAGEOPHYS<-paste(resultadosGeophysic,"/PCA",sep="")
  setwd(paste(PCAGEOPHYS))
  write.table(eigenvalues,file=paste("Geophysic_eigenvalues.xls",sep=""), sep = "\t", row.names = FALSE, qmethod = "double")
  write.table(eigenvectors,file=paste("Geophysic_eigenvectors.xls",sep=""), sep = "\t", row.names = FALSE, qmethod = "double")
  
  #Localizaci?n variables importantes en PCA eigenvectors
  importancia.geo2<-rownames(aaa)
  eigenvectors2<-eigenvectors[order(abs(eigenvectors$CS1),decreasing=TRUE),]
  eigenvectors2$CS1<-c(1:nrow(eigenvectors))
  eigenvectors2<-eigenvectors2[order(abs(eigenvectors$CS2),decreasing=TRUE),]
  eigenvectors2$CS2<-c(1:nrow(eigenvectors))
  eigenvectors2<-eigenvectors2[order(abs(eigenvectors$CS3),decreasing=TRUE),]
  eigenvectors2$CS3<-c(1:nrow(eigenvectors))
  eigenvectors3<-list()
  for(j in 1:length(importancia.geo2)){
    eigenvectors3[[j]]<-subset(eigenvectors2,eigenvectors2$variablesb==importancia.geo2[j])
  }
  eigenvectors3<-do.call("rbind",eigenvectors3)
  eigenvectors3<-eigenvectors3[,1:4]
  write.table(eigenvectors3,file=paste("Geophysic_IVposition.xls",sep=""), sep = "\t", row.names = FALSE, qmethod = "double")
  rm(eigenvalues)
  rm(eigenvectors)
  rm(eigenvectors2)
  rm(eigenvectors3)
  #Tabla final de variables seleccionadas
  setwd(paste(resultados))
  aaa<-colnames(aaa)
  for(i in 1:length(aaa)){
    if(aaa[i]=="DECLATITUDE"){
      aaa[i]<-"POINT_Y"
    }
    if(aaa[i]=="DECLONGITUDE"){
      aaa[i]<-"POINT_X"
    }
  }
  aaa<-figvartotal[figvartotal$VARCODE %in% aaa, ]
  write.table(aaa, file = paste("SelectedVariables_geophysic.xls",sep=""), sep = "\t", row.names = FALSE, qmethod = "double")
  rm(aaa)
  rm(resultadosGeophysic)
  rm(RFGEOPHYS)
  rm(importancia.geo)
  
  #EDAPH Se seleccionan s?lo las variables importantes por RF (porcentaje o fraccion seleccionada)
  
  ecogeot1<-edaph1[,paste(importancia.eda2)]
  
  
  #ahora parte normal de Corr de SelecVar
  corrBIO.estimate<- as.data.frame(matrix(nrow = ncol(ecogeot1), ncol = ncol(ecogeot1)))
  colnames(corrBIO.estimate)<-colnames(ecogeot1)
  rownames(corrBIO.estimate)<-colnames(ecogeot1)
  corrBIO.pvalue<- as.data.frame(matrix(nrow = ncol(ecogeot1), ncol = ncol(ecogeot1)))
  colnames(corrBIO.pvalue)<-colnames(ecogeot1)
  rownames(corrBIO.pvalue)<-colnames(ecogeot1)
  corrBIO.confint<- as.data.frame(matrix(nrow = ncol(ecogeot1), ncol = ncol(ecogeot1)))
  colnames(corrBIO.confint)<-colnames(ecogeot1)
  rownames(corrBIO.confint)<-colnames(ecogeot1)
  bbb<- as.data.frame(matrix(nrow = ncol(ecogeot1), ncol = ncol(ecogeot1)))
  ccc<- as.data.frame(matrix(nrow = ncol(ecogeot1), ncol = ncol(ecogeot1)))
  for(j in 1:ncol(ecogeot1)){
    for(k in 1:ncol(ecogeot1)){
      if(j!=k){
        aaa<-cor.test(ecogeot1[,j],ecogeot1[,k])
        corrBIO.estimate[j,k]<-round(aaa$estimate,3)
        corrBIO.pvalue[j,k]<-round(aaa$p.value,3)
        if(aaa$p.value<=pValue){
          bbb[j,k]<-1
        }
        if(aaa$p.value>pValue){
          bbb[j,k]<-0
        }
        if(aaa$estimate>=CorrValue|aaa$estimate<= -CorrValue){
          ccc[j,k]<-round(aaa$estimate,3)
        }
        if(aaa$estimate<CorrValue&aaa$estimate> -CorrValue){
          ccc[j,k]<-0
        }
        aaa<-paste(round(aaa$conf.int[1],3),round(aaa$conf.int[2],3),sep=" / ")
        corrBIO.confint[j,k]<-aaa
      }
      if(j==k){
        corrBIO.estimate[j,k]<-1
        corrBIO.pvalue[j,k]<-NA
        corrBIO.confint[j,k]<-NA
        bbb[j,k]<-0
        ccc[j,k]<-0
      }
    }
  }
  aaa<-bbb*ccc
  colnames(aaa)<-colnames(ecogeot1)
  rownames(aaa)<-colnames(ecogeot1)
  
  setwd(paste(resultadosEdaphic))
  dir.create(as.vector(paste("BivariateCorrelations",sep="")))
  BivariateCorrelationsEdaphic<-paste(resultadosEdaphic,"/BivariateCorrelations",sep="")
  setwd(paste(BivariateCorrelationsEdaphic))
  write.table(corrBIO.estimate, file = paste("Estimate_correlation_edaphic.xls",sep=""), sep = "\t", row.names = TRUE, qmethod = "double")
  write.table(corrBIO.pvalue, file = paste("Pvalue_correlation_edaphic.xls",sep=""), sep = "\t", row.names = TRUE, qmethod = "double")
  write.table(corrBIO.confint, file = paste("ConfidenceInterval_correlation_edaphic.xls",sep=""), sep = "\t", row.names = TRUE, qmethod = "double")
  write.table(aaa, file = paste("RelevantValues_correlation_edaphic.xls",sep=""), sep = "\t", row.names = TRUE, qmethod = "double")
  
  #Ahora selecci?n de variables m?s importantes por RF (de menor a mayor importancia) no correlacionadas 
  romper<-round(percenCorr*nrow(aaa))
  if(romper<nminvar){
    romper<-nminvar
  }
  #A?adir seed
  x<-nrow(aaa)
  #Funcion repeat
  repeat{
    if(nrow(aaa)<=romper|x==0){
      print("repeat loop ends");
      break
    }
    if(all(aaa[x,]==0)){
      x<-x-1
      next
    }
    if(any(abs(aaa[x,])>0)){
      aaa<-aaa[-1*x,-1*x]
      x<-nrow(aaa)
    }
  }
  
  #PCA-Edaphic
  ecogeopcaxebio<-ecogeopcaxe
  if(ecogeopcaxe>ncol(edaph1)-1){
    ecogeopcaxebio<-ncol(edaph1)-1
  }
  ecogeopcab<-dudi.pca(edaph1[,-1],center=TRUE,scale=TRUE,scannf=FALSE,nf=ecogeopcaxebio)
  variablesb<-colnames(edaph1)[-1]
  accenumbb<-edaph1[,1]
  eigenvalues<-ecogeopcab$eig
  eigenvectors<-ecogeopcab$c1
  eigenvalues<-data.frame(c(1:length(eigenvalues)),eigenvalues)
  colnames(eigenvalues)[1]<-"Component"
  variancet<-sum(eigenvalues[,2])
  variance<-vector(length=length(eigenvalues[,1]))
  var.accumul<-vector(length=length(eigenvalues[,1]))
  for (j in 1:length(eigenvalues[,1])){
    variance[j]<-(eigenvalues[j,2]*100)/variancet
    if(j==1){
      var.accumul[j]<-variance[j]
    }
    if(j>1){
      var.accumul[j]<-var.accumul[j-1]+variance[j]
    }
  }
  eigenvalues<-data.frame(eigenvalues,variance,var.accumul)
  eigenvectors<-data.frame(variablesb,eigenvectors)
  pcascores<-data.frame(accenumbb,ecogeopcab$li)
  
  setwd(paste(resultadosEdaphic))
  dir.create(as.vector(paste("PCA",sep="")))
  PCAEDAPHIC<-paste(resultadosEdaphic,"/PCA",sep="")
  setwd(paste(PCAEDAPHIC))
  write.table(eigenvalues,file=paste("Edaphic_eigenvalues.xls",sep=""), sep = "\t", row.names = FALSE, qmethod = "double")
  write.table(eigenvectors,file=paste("Edaphic_eigenvectors.xls",sep=""), sep = "\t", row.names = FALSE, qmethod = "double")
  
  #Localizaci?n variables importantes en PCA eigenvectors
  importancia.eda2<-rownames(aaa)
  eigenvectors2<-eigenvectors[order(abs(eigenvectors$CS1),decreasing=TRUE),]
  eigenvectors2$CS1<-c(1:nrow(eigenvectors))
  eigenvectors2<-eigenvectors2[order(abs(eigenvectors$CS2),decreasing=TRUE),]
  eigenvectors2$CS2<-c(1:nrow(eigenvectors))
  eigenvectors2<-eigenvectors2[order(abs(eigenvectors$CS3),decreasing=TRUE),]
  eigenvectors2$CS3<-c(1:nrow(eigenvectors))
  eigenvectors3<-list()
  for(j in 1:length(importancia.eda2)){
    eigenvectors3[[j]]<-subset(eigenvectors2,eigenvectors2$variablesb==importancia.eda2[j])
  }
  eigenvectors3<-do.call("rbind",eigenvectors3)
  eigenvectors3<-eigenvectors3[,1:4]
  write.table(eigenvectors3,file=paste("Edaphic_IVposition.xls",sep=""), sep = "\t", row.names = FALSE, qmethod = "double")
  rm(eigenvalues)
  rm(eigenvectors)
  rm(eigenvectors2)
  rm(eigenvectors3)
  ################################
  #Tabla final de variables seleccionadas
  setwd(paste(resultados))
  aaa<-colnames(aaa)
  aaa<-figvartotal[figvartotal$VARCODE %in% aaa, ]
  write.table(aaa, file = paste("SelectedVariables_edaphic.xls",sep=""), sep = "\t", row.names = FALSE, qmethod = "double")
  rm(aaa)
  rm(resultadosEdaphic)
  rm(RFEDAPHIC)
  rm(importancia.eda)
  
  setwd(paste(ruta))
  write("12.Terminado proceso de an?lisis multivariado - PCA", file="Error/process_info.txt", append=TRUE)
  
  write("13.Proceso finalizado exitosamente", file="Error/process_info.txt", append=TRUE)
  
}

