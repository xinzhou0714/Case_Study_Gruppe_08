# Hier schreibe ich  meine Gedanke 



## 1.Einlesen der Dateien

### unstrukturierte txt-Dateien:

| File Name          | Trennzeichen_Spalten-ASCII(hex) | Pattern | Ersatz | Trennzeichen_Zeilen-ASCII(hex) | Pattern | Ersatz |
| ------------------ | :--------: | :-----: | ---------- | :-----: | :-----: | :-----: |
| Einzelteil_T01.txt | 207c207c20 | " \| \| " | ";" | 20         |         | "\n" |
| Einzelteil_T02.txt |    2020    | "  " | ";" | 09         | "\t" | "\n" |
| Einzelteil_T03.txt |     7C     | "\|" | ";" | 0b      | "\v" | "\n" |
| Einzelteil_T07.txt | 09 | "\t" | ";" | 22{n}2209{n} |         | 0a22{n}22 |
| Einzelteil_T09.txt | 5c | "\\\\" | ";" | 0b |         | "\n" |
| Komponente_K1DI2.txt | 5c | "\\\\" | ";" | 0922{n}225c{n} |         | 0d0a22{n}22 |



[Regular expressions](https://stringr.tidyverse.org/articles/regular-expressions.html )





`````R
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


Einzelteil_T01<-read_file("./Data/Einzelteil/Einzelteil_T01.txt")%>%
  str_replace_all("\x22 \x22","\x22\n\x22")%>%
  str_replace_all(" | | ",";")%>%
  fread()

`````

