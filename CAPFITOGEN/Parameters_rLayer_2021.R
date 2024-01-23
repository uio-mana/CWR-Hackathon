#############################################
#rLayer 2021 parameters script
#############################################
#To cite CAPFITOGEN in publications use: Parra-Quijano, M., Iriondo, J.M., Torres, M.E., Lopez, F., Phillips, J., and Kell, S. 2021. CAPFITOGEN3: A toolbox for the conservation and promotion of the use of agricultural biodiversity. ISBN: 978-958-505-038-9 URL:  http://www.capfitogen.net/en

#### How to use this script: Please configure each parameter, select all the lines of this script and click the "Run" button. After that, please open the corresponding tool script and run the process 

#### for each parameter (word in black color) please assign a value or select an option

ruta<-"C:/CAPFITOGEN3"
#Text type parameter (text that goes between quotation marks "")
#### Note: It is the path where the structure of folders and files that are necessary to run the tools is located. It is usually a folder called CAPFITOGEN3 located in the root of the main hard drive (usually c:/)

#############################################
#Type of layer cropping
#############################################

cropway<-"polygon"
#cropway<-"square"
#cropway<-"buffer"
#Text type parameter (text that goes between quotation marks "")
#### Note1: This parameter tells rLayer how to crop the world layers (world) to fit certain limits.
#### Note2: If cropway="polygon", a shapefile type vector file provided by the user will be used as an outline to crop the layers.
#### Note3: If cropway="square", the collection sites (coordinates) included in the passport table will be used to crop the layers, using the maximum extension of the distribution of said points to make a square or rectangle cropping.
#### Note4: If cropway="buffer", the collection sites (coordinates) included in the passport table will be used to create a circular area around each site with a radius specified in the buffer parameter

buffer<-30 #Only applies if cropway="buffer"
#Number type parameter (which will appear in blue in Rstudio)
#### Note: Numerical parameter that expresses kilometers (km) and that indicates the radius that will be used to generate the surrounding areas around each coordinate/site. The circular areas will be merged to outline the entire area with which the world layers will be cropped.

shapefile<-"albania" #Only applies if cropway="polygon"
#Text type parameter (text that goes between quotation marks "")
#### Note1: In this parameter you must indicate the name of the shapefile, which must be in the WGS84 lat-long coordinates system and contain a single polygon by which you want to crop
#### Note2: The shapefile (The 4-7 files that make it up. Among these, the mandatory files .shp, .dbf and .shx must be included) must be found in the Pasaporte folder (path X:/CAPFITOGEN/Pasaporte)

pasaporte<-"PasaporteOriginalEvaluadoGEOQUAL.txt" #Only applies if cropway="square" or cropway="buffer"
#Text type parameter (text that goes between quotation marks "")
# Note1: this text file must be in the Pasaporte folder, which in turn is a folder within "ruta"
# Note2: this table has the same structure as other passport tables. It may have already been analyzed using the GEOQUAL tool

geoqual<-TRUE #Only applies if cropway="square" or cropway="buffer"
#TRUE or FALSE type parameter
# Note: Does the passport have the GEOQUAL evaluation (4 additional variables on the right side of the table?)

totalqual<-60 #Only applies if geoqual=TRUE
#Number type parameter (which will appear in blue in Rstudio)
##### Note1: Threshold allowed for GEOQUAL (values equal to or greater than the one indicated for the TOTALQUAL parameter)
##### Note2: It must be a value between 0 and 100. If the value stipulated for totalqual is very high and no record in the passport table has values higher than that value in the TOTALQUAL100 column, it will surely generate an error.

resol1<-"Celdas 1x1 km aprox (30 arc-seg)"
#resol1<-"Celdas 5x5 km aprox (2.5 arc-min)"
#resol1<-"celdas 20x20 km aprox (10 arc-min)"
#resol1<-"celdas 10x10 km aprox (5 arc-min)"
#Text type parameter (text that goes between quotation marks "")
###### Note1: This parameter indicates the resolution of the world ecogeographic layers from which the cropped layers will be obtained
###### Note2: To choose a resol option, you must first check that the resolution of the world layers is already available in the path x:/CAPFITOGEN/rdatamaps/world/ and that the "1x1", "5x5", "10x10" or "20x20" folders are also within this path 

uname<-"user1"
#uname<-"user2"
#uname<-"user3"
#Text type parameter (text that goes between quotation marks "")
##### Note: the uname parameter is used to define the name of the folder that will be generated within X:/CAPFITOGEN/rdatamaps and that will contain the cropped layers
##### Note2: The name determined in the uname parameter can then be used in the different CAPFITOGEN tools in the "pais" parameter.
##### Note: For the local version of CAPFITOGEN, only the options "user1", "user2" and "user3" are offered.
