



####0 librarys
# load the data.table package using library()


if(!require(data.table)) {
  install.packages("data.table")
  require(data.table)
}

if(!require(tidyverse)) {
  install.packages("tidyverse")
  require(tidyverse)
}



####0 Importing
#dir<-list.dirs()
dir_Einzelteil<-"./Data/Einzelteil"
dir_Fahrzeug<-"./Data/Fahrzeug" 
dir_Geodaten<-"./Data/Geodaten" 
dir_Komponente<-"./Data/Komponente"
dir_Logistikverzug<-"./Data/Logistikverzug"  
dir_Zulassungen<-"./Data/Zulassungen" 



files_Einzelteil<-list.files(path = dir_Einzelteil)
files_Fahrzeug<-list.files(path = dir_Fahrzeug)
files_Geodaten<-list.files(path = dir_Geodaten)
files_Komponente<-list.files(path = dir_Komponente)
files_Logistikverzug<-list.files(path = dir_Logistikverzug)
files_Zulassungen<-list.files(path = dir_Zulassungen)


files_Komponente[20]



Komponente_K1BE1 <- fread(file.path(dir_Komponente,files_Komponente[17]))

Komponente_K1DI2 <- fread(file.path(dir_Komponente,files_Komponente[20]))

Komponente_K1DI2_V2<-fread(file.path(dir_Komponente,files_Komponente[20]),sep = "\\")

Komponente_K1DI2_V5<-fread(file.path(dir_Komponente,files_Komponente[20]))

Komponente_K1DI2_text<-read_file(file.path(dir_Komponente,files_Komponente[20]))
