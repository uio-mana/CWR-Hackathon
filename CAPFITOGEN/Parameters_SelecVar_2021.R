#############################################
#SelecVar 2021 parameters script
#############################################
#To cite CAPFITOGEN in publications use: Parra-Quijano, M., Iriondo, J.M., Torres, M.E., López, F., Phillips, J., and Kell, S. 2021. CAPFITOGEN3: A toolbox for the conservation and promotion of the use of agricultural biodiversity. ISBN: 978-958-505-038-9 URL:  http://www.capfitogen.net/en

#### How to use this script: Please configure each parameter, select all the lines of this script and click the "Run" button. After that, please open the corresponding tool script and run the process 

#### for each parameter (word in black color) please assign a value or select an option

ruta<-"C:/CAPFITOGEN3"
#Text type parameter (text that goes between quotation marks "")
#### Note: It is the path where the structure of folders and files that are necessary to run the tools is located. It is usually a folder called CAPFITOGEN3 located in the root of the main hard drive (usually C:/)

pais<-"Ecuador"
#pais<-"Colombia"
#pais<-"Spain"
#pais<-"United States"
#pais<-"Argentina"
#pais<-"Bolivia"
#pais<-"Brasil"
#pais<-"Chile"
#pais<-"Chile continental"
#pais<-"Canada"
#pais<-"Meso America"
#pais<-"Russia"
#pais<-"South America"
#pais<-"Costa Rica"
#pais<-"World"
#pais<-"user1"
#etc
#Text type parameter (text that goes between quotation marks "")
##### Note: The selected country/region (the first letter usually goes in uppercase as in "Ecuador") must have the corresponding folder (with the name of the country but there it goes in lowercase as in "ecuador") in rdatamaps.
##### Note2: the name of the country must be written as it appears in the lista_paises.xlsx table, and as it appears in the column "Way to write country name (pais parameter)" 
##### Note3: If you have previously used the rLayer tool and generated a set of cropped layers according to the individual needs of the user, you can enter here a name defined in the "uname" parameter to use that set of layers

#############################################
#Pasaporte and GEOQUAL
#############################################

pasaporte<-"PasaporteOriginalEvaluadoGEOQUAL.txt"
#Text type parameter (text that goes between quotation marks "")
# Note1: this text file must be in the Pasaporte folder, which in turn is a folder within "ruta"
# Note2: this table has the same structure as other passport tables. It may have already been analyzed using the GEOQUAL tool

geoqual<-TRUE
#TRUE or FALSE type parameter
# Note: Does the passport have the GEOQUAL evaluation (4 additional variables on the right side of the table?)

totalqual<-60 #Only applies if geoqual=TRUE
#Number type parameter (which will appear in blue in Rstudio)
##### Note1: Threshold allowed for GEOQUAL (values equal to or greater than the one indicated for the TOTALQUAL parameter)
##### Note2: It must be a value between 0 and 100. If the value stipulated for totalqual is very high and no record in the passport table has values higher than that value in the TOTALQUAL100 column, it will surely generate an error.

#############################################
#Duplicate removal
#############################################

distdup<-1
#Number type parameter (which will appear in blue in Rstudio)
##### Note1: This numerical parameter indicates the distance threshold under which you consider that two collection sites in fact represent the same population (expressed in km)
##### Note2: A very high number in distdup can significantly reduce the number of accessions in the analysis 

#############################################
#Ecogeographic
#############################################

resol1<-"Celdas 1x1 km aprox (30 arc-seg)"
#resol1<-"Celdas 5x5 km aprox (2.5 arc-min)"
#resol1<-"celdas 20x20 km aprox (10 arc-min)"
#resol1<-"celdas 10x10 km aprox (5 arc-min)"
#Text type parameter (text that goes between quotation marks "")
###### Note1: This parameter indicates the resolution at which you want to extract ecogeographic information
###### Note2: resol1 means the resolution of the ecogeographic layers that will be used to extract information for each point. resol1 must exist as a "1x1", "5x5", "10x10" or "20x20" folder inside rdatamaps/nombre_pais

buffy<-FALSE
#TRUE or FALSE type parameter
##### Note1: Method of extraction, do you want to include contiguous cells (buffer zone) to calculate the value assigned to each collection site?
##### Note2: selecting buffy=TRUE can slow down the extraction process, since it forces R to extract values for each point (which comes from the coordinates of the records in the passport table) not only from the cell where they fall (simple extraction, when buffy=FALSE) but from adjacent cells, whose values are averaged... that slows down the operation

tamp<-1000 #Only applies when buffy=TRUE
#Number type parameter (which will appear in blue in Rstudio)
##### Note1: Radius of the buffer zone that you want to apply to calculate the value that will be assigned to each collection site (in meters)
##### Note2: a very large tamp (> 10000) combined with a very high resolution (resol1) (1x1) will generate very slow extraction processes

bioclimv<-c("Temp prom anual","Prec anual","Prec cuarto mas calido")
###Complete list of bioclimatic variables, for more information see the table of variables and their corresponding descriptions in English and Spanish:
#bioclimv<-c("Temp prom anual" , "Temp prom cuarto mas calido" , "Temp prom cuarto mas frio" , "Prec anual" , "Prec mes mas humedo" , "Prec mes mas seco" , "Estacionalidad prec" , "Prec cuarto  mas humedo" , "Prec cuarto mas seco" , "Prec cuarto mas calido" , "Prec cuarto mas frio" , "Rango prom temp diurnas" , "Isotermalidad" , "Estacionalidad temp" , "Max temp mes mas calido" , "Min temp mes mas frio" , "Rango temp anual" , "Temp prom cuarto humedo" , "Temp prom cuarto seco" , "Prec prom 1" , "Prec prom 10" , "Prec prom 11" , "Prec prom 12" , "Prec prom 2" , "Prec prom 3" , "Prec prom 4" , "Prec prom 5" , "Prec prom 6" , "Prec prom 7" , "Prec prom 8" , "Prec prom 9" , "Temp max 1" , "Temp max 10" , "Temp max 11" , "Temp max 12" , "Temp max 2" , "Temp max 3" , "Temp max 4" , "Temp max 5" , "Temp max 6" , "Temp max 7" , "Temp max 8" , "Temp max 9" , "Temp prom 1" , "Temp prom 10" , "Temp prom 11" , "Temp prom 12" , "Temp prom 2" , "Temp prom 3" , "Temp prom 4" , "Temp prom 5" , "Temp prom 6" , "Temp prom 7" , "Temp prom 8" , "Temp prom 9" , "Temp min 1" , "Temp min 10" , "Temp min 11" , "Temp min 12" , "Temp min 2" , "Temp min 3" , "Temp min 4" , "Temp min 5" , "Temp min 6" , "Temp min 7" , "Temp min 8" , "Temp min 9" , "Presion de vapor 1" , "Presion de vapor 2" , "Presion de vapor 3" , "Presion de vapor 4" , "Presion de vapor 5" , "Presion de vapor 6" , "Presion de vapor 7" , "Presion de vapor 8" , "Presion de vapor 9" , "Presion de vapor 10" , "Presion de vapor 11" , "Presion de vapor 12" , "Presion de vapor anual")
###List of the 19 BIOCLIM variables:
#bioclimv<-c("Temp prom anual" , "Temp prom cuarto mas calido" , "Temp prom cuarto mas frio" , "Prec anual" , "Prec mes mas humedo" , "Prec mes mas seco" , "Estacionalidad prec" , "Prec cuarto  mas humedo" , "Prec cuarto mas seco" , "Prec cuarto mas calido" , "Prec cuarto mas frio" , "Rango prom temp diurnas" , "Isotermalidad" , "Estacionalidad temp" , "Max temp mes mas calido" , "Min temp mes mas frio" , "Rango temp anual" , "Temp prom cuarto humedo" , "Temp prom cuarto seco")
###Text type parameter (text that goes between quotation marks ""), of multiple type (you can select more than one option, including all the options in quotation marks, separated by commas and everything within a pair of parentheses preceded by the letter c)
##### Note:The names of the variables come from a column in the figvartotal.xlsx table, called VARDESCR, which are indicated in the VARMODULO column as "Bioclimatic"

edaphv<-c("Silt content top","Sand content top","Avail soil water cap top")
###Complete list of SOILGRIDS edaphic variables (more modern), for more information see the table of variables and their corresponding descriptions in English and Spanish:
#edaphv<-c("Avail soil water cap h1 top","Avail soil water cap h2 top","Avail soil water cap h3 top","Sat water cont top","Depth to bedrock","R horizon","Bulk density top","Cation exchange cap top","Clay content top","Coarse fragments top","Organic carbon dens top","Organic carbon stock top","Organic carbon content top","Soil pH H2O top","Soil pH KCl top","Sodic soil grade","Silt content top","Sand content top","Avail soil water cap top")
###Text type parameter (text that goes between quotation marks ""), of multiple type (you can select more than one option, including all the options in quotation marks, separated by commas and everything within a pair of parentheses preceded by the letter c)
##### Note:The names of the variables come from a column in the figvartotal.xlsx table, called VARDESCR, which are indicated in the VARMODULO column as "Edaphic"

geophysv<-c("Elevacion","Pendiente grados")
###Complete list of geophysical variables, for more information see the table of variables and their corresponding descriptions in English and Spanish:
#geophys<-c("Elevacion","Orientacion","Esticidad","Norticidad","Pendiente grados","Velocidad viento 1","Velocidad viento 2","Velocidad viento 3","Velocidad viento 4","Velocidad viento 5","Velocidad viento 6","Velocidad viento 7","Velocidad viento 8","Velocidad viento 9","Velocidad viento 10","Velocidad viento 11","Velocidad viento 12","Velocidad viento anual","Radiacion solar 1","Radiacion solar 2","Radiacion solar 3","Radiacion solar 4","Radiacion solar 5","Radiacion solar 6","Radiacion solar 7","Radiacion solar 8","Radiacion solar 9","Radiacion solar 10","Radiacion solar 11","Radiacion solar 12","Radiacion solar anual")
###Text type parameter (text that goes between quotation marks ""), of multiple type (you can select more than one option, including all the options in quotation marks, separated by commas and everything within a pair of parentheses preceded by the letter c)
##### Note:The names of the variables come from a column in the figvartotal.xlsx table, called VARDESCR, which are indicated in the VARMODULO column as "Geophysical"

latitud<-FALSE #Only applies if ecogeo=TRUE
#TRUE or FALSE type parameter
##### Note:This parameter indicates whether the latitude variable (Y) that comes from the DECLATITUDE column of the passport table will be used to make the ecogeographic characterization (as a geophysical variable)

longitud<-TRUE #Only applies if ecogeo=TRUE
#TRUE or FALSE type parameter
##### Note1:This parameter indicates whether the longitude variable (X) that comes from the DECLATITUDE column of the passport table will be used to make the ecogeographic characterization (as a geophysical variable)
##### Note2:For DIVmapas latitude and longitude are not found as layers in rdatamaps since they are directly obtained from the coordinates in the passport table. However, both latitude and longitude would enter as geophysical variables in DIVmapas analyses.

#############################################
#Selection of variables
#############################################

percenRF<-0.66
#Number type parameter (which will appear in blue in Rstudio)
#### Note1: percenRF is the percentage of variables that will be selected by Random Forest (for example, if you wanted to select 2/3 of the total variables by Random Forest, percenRF would be 0.66). The parameter that RF calculates to define importance is mean decrease accuracy
#### Note2: percenRF is a value between 0 and 1. The percenRF value indicates the percentage of variables that will continue in the selection process. 1-percenRF will be the variables discarded as not important for Random Forest classification.

percenCorr<-0.33
#Number type parameter (which will appear in blue in Rstudio)
#### Note1: percenCorr is the percentage of variables that will be selected by the analysis of bivariate correlations, which is executed after the selection by Random Forest (for example, if you wanted to select 1/3 of the total of variables by bivariate correlations, percenRF would be 0.33
#### Note2: percenCorr is a value between 0 and 1. The percenCorr value indicates the percentage of variables that will end up being selected (identified as important), from the group of variables selected by Random Forest classification. 1-percenCorr will be the variables discarded for being redundant according to the bivariate correlations.

CorrValue<-0.5
#Number type parameter (which will appear in blue in Rstudio)
#### Note1: In CorrValue a correlation threshold value is defined, above (in its positive form) or below (in its negative form) of which it is assumed that there is a correlation between two variables. This value tends to vary between 0.5/-0.5 and 0.7/-0.7according to most studies.
#### Note2: Although the user only enters the positive value for CorrValue here, it will be used for its positive and negative forms (CorrValue *-1)

pValue<-0.05
#Number type parameter (which will appear in blue in Rstudio)
#### Note1: pValue defines the significance threshold value for bivariate correlations. This value usually fluctuates between 0.001 and 0.05.
#### Note2: The correlation will be assumed to be significant when the P value is less than pValue

nminvar<-3
#Number type parameter (which will appear in blue in Rstudio)
#### Note1: nminvar tells the SelectVar tool the minimum number of variables to select per component. For example, although the processes of variable selection by RF and bivariate correlation indicate that two variables will be selected, if the nminvar number is 3, the selection process by correlations will select the three least correlated variables.
#### Note2: Remember that the variable selection processes are carried out independently by component. Therefore, if the total initial set of geophysical variables being evaluated is 5 variables, and nminvar=6, an error will occur.

ecogeopcaxe<-5
#Number type parameter (which will appear in blue in Rstudio)
#### Note1: ecogeopcaxe tells SelecVar the number of axes (principal components) that will be shown in the tables of eigenvectors, eigenvalues and the PCA scores.
#### Note2: ecogeopcaxe cannot be greater than the smallest number of variables to be evaluated per component, otherwise it will produce an error. For example, if the component with the least variables to evaluate was the geophysical one (geophysv) with only three variables, ecogeopcaxe cannot be greater than two.

#############################################
#Results
#############################################
resultados<-"C:/CAPFITOGEN3/Resultados/SelecVar"
#Text type parameter (text that goes between quotation marks "")
