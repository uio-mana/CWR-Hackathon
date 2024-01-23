#############################################
#ELCmapas 2021 parameters script
#############################################
#To cite CAPFITOGEN in publications use: Parra-Quijano, M., Iriondo, J.M., Torres, M.E., Lopez, F., Phillips, J., and Kell, S. 2021. CAPFITOGEN3: A toolbox for the conservation and promotion of the use of agricultural biodiversity. ISBN: 978-958-505-038-9 URL:  http://www.capfitogen.net/en

#### How to use this script: Please configure each parameter, select all the lines of this script and click the "Run" button. After that, please open the corresponding tool script and run the process 

#### for each parameter (word in black color) please assign a value or select an option

ruta<-"C:/CAPFITOGEN3"
#Text type parameter (text that goes between quotation marks "")
#### Note: It is the path where the structure of folders and files that are necessary to run the tools is located. It is usually a folder called CAPFITOGEN3 located in the root of the main hard drive (usually c:/)

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
#...... etc.
#Text type parameter (text that goes between quotation marks "")
##### Note: The selected country/region (the first letter usually goes in uppercase as in "Ecuador") must have the corresponding folder (with the name of the country but there it goes in lowercase as in "ecuador") in rdatamaps.
##### Note2: the name of the country must be written as it appears in the lista_paises.xlsx table, and as it appears in the column "Way to write country name (pais parameter)"
##### Note3: If you have previously used the rLayer tool and generated a set of cropped layers according to the individual needs of the user, you can enter here a name defined in the "uname" parameter to use that set of layers

metodo<-"kmeansbic"
#metodo<-"medoides"
#metodo<-"elbow"
#metodo<-"calinski"
#metodo<-"ssi"
#metodo<-"bic"
#Text type parameter (text that goes between quotation marks "")
##### Note: This parameter tells the tool the type of algorithm that will be used to determine the optimal number of clusters for each component (bioclimatic, geophysical, edaphic), which affects the final number of categories of the ELC map.

iterat<-10 #Only applies if metodo="Calinski" or "ssi"
#Number type parameter (which will appear in blue in Rstudio)
##### Note: This parameter is specific to the ssi or calisnki algorithms that determine clusters (metodo parameter) and indicates the number of iterations that will be used by these methods to calculate the optimal number of clusters.

resol1<-"Celdas 1x1 km aprox (30 arc-seg)"
#resol1<-"Celdas 5x5 km aprox (2.5 arc-min)"
#resol1<-"celdas 20x20 km aprox (10 arc-min)"
#resol1<-"celdas 10x10 km aprox (5 arc-min)"
#Text type parameter (text that goes between quotation marks "")
###### Note1: This parameter indicates the resolution that will be used to make the ELC map
###### Note2: resol1 means the resolution of the ecogeographic layers that will be used to extract information for each point. resol1 must exist as a "1x1", "5x5", "10x10" or "20x20" folder inside rdatamaps/nombre_pais

bioclimv<-c("Temp prom anual","Prec anual","Prec cuarto mas calido") #Only applies if bioclimsn=TRUE
###Complete list of bioclimatic variables, for more information see the table of variables and their corresponding descriptions in English and Spanish:
#bioclimv<-c("Temp prom anual" , "Temp prom cuarto mas calido" , "Temp prom cuarto mas frio" , "Prec anual" , "Prec mes mas humedo" , "Prec mes mas seco" , "Estacionalidad prec" , "Prec cuarto  mas humedo" , "Prec cuarto mas seco" , "Prec cuarto mas calido" , "Prec cuarto mas frio" , "Rango prom temp diurnas" , "Isotermalidad" , "Estacionalidad temp" , "Max temp mes mas calido" , "Min temp mes mas frio" , "Rango temp anual" , "Temp prom cuarto humedo" , "Temp prom cuarto seco" , "Prec prom 1" , "Prec prom 10" , "Prec prom 11" , "Prec prom 12" , "Prec prom 2" , "Prec prom 3" , "Prec prom 4" , "Prec prom 5" , "Prec prom 6" , "Prec prom 7" , "Prec prom 8" , "Prec prom 9" , "Temp max 1" , "Temp max 10" , "Temp max 11" , "Temp max 12" , "Temp max 2" , "Temp max 3" , "Temp max 4" , "Temp max 5" , "Temp max 6" , "Temp max 7" , "Temp max 8" , "Temp max 9" , "Temp prom 1" , "Temp prom 10" , "Temp prom 11" , "Temp prom 12" , "Temp prom 2" , "Temp prom 3" , "Temp prom 4" , "Temp prom 5" , "Temp prom 6" , "Temp prom 7" , "Temp prom 8" , "Temp prom 9" , "Temp min 1" , "Temp min 10" , "Temp min 11" , "Temp min 12" , "Temp min 2" , "Temp min 3" , "Temp min 4" , "Temp min 5" , "Temp min 6" , "Temp min 7" , "Temp min 8" , "Temp min 9" , "Presion de vapor 1" , "Presion de vapor 2" , "Presion de vapor 3" , "Presion de vapor 4" , "Presion de vapor 5" , "Presion de vapor 6" , "Presion de vapor 7" , "Presion de vapor 8" , "Presion de vapor 9" , "Presion de vapor 10" , "Presion de vapor 11" , "Presion de vapor 12" , "Presion de vapor anual")
###List of the 19 BIOCLIM variables:
#bioclimv<-c("Temp prom anual" , "Temp prom cuarto mas calido" , "Temp prom cuarto mas frio" , "Prec anual" , "Prec mes mas humedo" , "Prec mes mas seco" , "Estacionalidad prec" , "Prec cuarto  mas humedo" , "Prec cuarto mas seco" , "Prec cuarto mas calido" , "Prec cuarto mas frio" , "Rango prom temp diurnas" , "Isotermalidad" , "Estacionalidad temp" , "Max temp mes mas calido" , "Min temp mes mas frio" , "Rango temp anual" , "Temp prom cuarto humedo" , "Temp prom cuarto seco")
###Text type parameter (text that goes between quotation marks ""), of multiple type (you can select more than one option, including all the options in quotation marks, separated by commas and everything within a pair of parentheses preceded by the letter c)
##### Note:The names of the variables come from a column in the figvartotal.xlsx table, called VARDESCR, which are indicated in the VARMODULO column as "Bioclimatic"

edaphv<-c("Silt content top","Sand content top","Avail soil water cap top") #Only applies if bioclimsn=TRUE
###Complete list of SOILGRIDS edaphic variables (more modern), for more information see the table of variables and their corresponding descriptions in English and Spanish:
#edaphv<-c("Avail soil water cap h1 top","Avail soil water cap h2 top","Avail soil water cap h3 top","Sat water cont top","Depth to bedrock","R horizon","Bulk density top","Cation exchange cap top","Clay content top","Coarse fragments top","Organic carbon dens top","Organic carbon stock top","Organic carbon content top","Soil pH H2O top","Soil pH KCl top","Sodic soil grade","Silt content top","Sand content top","Avail soil water cap top")
###Text type parameter (text that goes between quotation marks ""), of multiple type (you can select more than one option, including all the options in quotation marks, separated by commas and everything within a pair of parentheses preceded by the letter c)
##### Note:The names of the variables come from a column in the figvartotal.xlsx table, called VARDESCR, which are indicated in the VARMODULO column as "Edaphic"

geophysv<-c("Elevacion","Pendiente grados")
###Complete list of geophysical variables, for more information see the table of variables and their corresponding descriptions in English and Spanish:
#geophys<-c("Elevacion","Orientacion","Esticidad","Norticidad","Pendiente grados","Velocidad viento 1","Velocidad viento 2","Velocidad viento 3","Velocidad viento 4","Velocidad viento 5","Velocidad viento 6","Velocidad viento 7","Velocidad viento 8","Velocidad viento 9","Velocidad viento 10","Velocidad viento 11","Velocidad viento 12","Velocidad viento anual","Radiacion solar 1","Radiacion solar 2","Radiacion solar 3","Radiacion solar 4","Radiacion solar 5","Radiacion solar 6","Radiacion solar 7","Radiacion solar 8","Radiacion solar 9","Radiacion solar 10","Radiacion solar 11","Radiacion solar 12","Radiacion solar anual")
###Text type parameter (text that goes between quotation marks ""), of multiple type (you can select more than one option, including all the options in quotation marks, separated by commas and everything within a pair of parentheses preceded by the letter c)
##### Note:The names of the variables come from a column in the figvartotal.xlsx table, called VARDESCR, which are indicated in the VARMODULO column as "Geophysical"

latitud<-TRUE
#TRUE or FALSE type parameter
##### Note:This parameter indicates whether the latitude (X) variable will be taken into account as a variable in the geophysical component for the creation of the ELC map. Keep in mind that using the latitude and longitude parameters (TRUE) in the creation of an ELC map will produce more spatially aggregated ecogeographic units (something usually pursued)

longitud<-TRUE
#TRUE or FALSE type parameter
##### Note:This parameter indicates whether the longitude (Y) variable will be taken into account as a variable in the geophysical component for the creation of the ELC map. Keep in mind that using the latitude and longitude parameters (TRUE) in the creation of an ELC map will produce more spatially aggregated ecogeographic units (something usually pursued)
##### Note2:Latitude and longitude are not found as layers in rdatamaps since they are directly obtained from the centroids of the cells that make up the work area.

maxg<-8
#Number type parameter (which will appear in blue in Rstudio)
##### Note1: This numeric parameter tells the tool the maximum number of clusters per component that will be allowed. Therefore, this number is a factor that the algorithm selected in the metodo parameter will take into account to limit the optimal number of clusters per component.
##### Note2: Very high numbers (greater than 8) can allow some algorithms to determine high numbers of optimal clusters per component, which will eventually generate an ELC map with too many categories (>100). Too many categories on an ELC map can be undesirable (depending on the case).

#############################################
#Results
#############################################
resultados<-"C:/CAPFITOGEN3/Resultados/ELCmapas"
#Text type parameter (text that goes between quotation marks "")
# Note: Path of a folder (existing) where the results will be saved
