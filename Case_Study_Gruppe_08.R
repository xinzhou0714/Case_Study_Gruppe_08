



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



# ####0 Importing
# #dir<-list.dirs()
# dir_Einzelteil<-"./Data/Einzelteil"
# dir_Fahrzeug<-"./Data/Fahrzeug" 
# dir_Geodaten<-"./Data/Geodaten" 
# dir_Komponente<-"./Data/Komponente"
# dir_Logistikverzug<-"./Data/Logistikverzug"  
# dir_Zulassungen<-"./Data/Zulassungen" 
# 
# 
# 
# files_Einzelteil<-list.files(path = dir_Einzelteil)
# files_Fahrzeug<-list.files(path = dir_Fahrzeug)
# files_Geodaten<-list.files(path = dir_Geodaten)
# files_Komponente<-list.files(path = dir_Komponente)
# files_Logistikverzug<-list.files(path = dir_Logistikverzug)
# files_Zulassungen<-list.files(path = dir_Zulassungen)
# 
# 
# files_Komponente[20]
# 
# 
# 
# Komponente_K1BE1 <- fread(file.path(dir_Komponente,files_Komponente[17]))
# 
# Komponente_K1DI2 <- fread(file.path(dir_Komponente,files_Komponente[20]))
# 
# Komponente_K1DI2_V2<-fread(file.path(dir_Komponente,files_Komponente[20]),sep = "\\")
# 
# Komponente_K1DI2_V5<-fread(file.path(dir_Komponente,files_Komponente[20]))
# 
# Komponente_K1DI2_text<-read_file(file.path(dir_Komponente,files_Komponente[20]))
# 
# 

read_unstructured_txt<-function(fName,r_sep,c_sep,type){
  #to make unstructured one-line txt-file readable 
  #
  #Args:
  #   fName:  file name of the txt file
  #   r_sep:  separator between rows
  #   c_sep:  separator between columns
  #   type:
  #Returns:
  #   the structured plain text that can be read as dataframe 
  if(type==1){
    #Structure Typ ist einfachst Fall
    read_file(fName)%>%
      str_replace_all(r_sep,"\n")%>%
      str_replace_all(c_sep,";")
  }else if(type==2){
    read_file(fName)%>%
      str_replace_all(r_sep,"\n")%>%
      str_replace_all(c_sep,";")
  }else{
    stop("unknown structure")
  }
  

  
}

#text<-read_lines("./Data/Einzelteil/Einzelteil_T01.txt")
text<-read_lines("./aaa.txt")
str_replace_all(string=text, pattern="| |", replacement=";")
