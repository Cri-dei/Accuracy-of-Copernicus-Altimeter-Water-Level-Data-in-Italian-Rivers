#########################################
####### Code in situ comparison #########
#########################################
####  Author: Cristina Deidda    ########
#########################################
rm(list = ls())


library("readxl")
library(tidyr)
library(ggpubr)
library(reshape)
library("dplyr")
library("gmt")

####################### PROCESSING UNFYING SENTINEL DATA  ###########################


#LOAD Sentinel 3 Processed data

load("C:/Users/39349/Documents/Remote Sensing/1. Dati/Sentinel 3 data/Sentinel3Data_processed.RData")
Sentinel3_data_Info<-data.frame(Sentinel3_data_Info)


############### UPDATE PROCESSING Sentinel Data  ######################

Sentinel3_data_managed<-list()


for (xx in 1:length(Sentinel3_data))
{
  Hour_format00<-as.POSIXct(Sentinel3_data[[xx]][,5], format="%d-%m-%Y-%H:%M",tz="UTC")
  
  Data_Time_CET<-format(Hour_format00,tz="CET", format="%d-%m-%Y-%H:%M")
  
  
  Data_format<-paste0(substring(Data_Time_CET,7,10), substring(Data_Time_CET,4,5),
                                                    substring(Data_Time_CET,1,2))
  
  Hour_format_CET<-as.POSIXct(format(Hour_format00,tz="CET"))
  
  Hour_format_ora<-lubridate::round_date(Hour_format_CET, "1 hour")   
  
  Hour_format_15min0<-lubridate::round_date(Hour_format_CET, "15 minutes") 
  
  Hour_format_30min0<-lubridate::round_date(Hour_format_CET, "30 minutes")   
  
  Hour_format<-as.numeric(paste0(substring(Hour_format_ora,12,13),substring(Hour_format_ora,15,16)))
    
  Hour_format_15min<-as.numeric(paste0(substring(Hour_format_15min0,12,13),substring(Hour_format_15min0,15,16)))
  
  Hour_format_30min<-as.numeric(paste0(substring(Hour_format_30min0,12,13),substring(Hour_format_30min0,15,16)))
  
  Sentinel3_data_managed[[xx]]<-cbind(Sentinel3_data[[xx]], Data_Time_CET, Data_format, Hour_format,Hour_format_15min,Hour_format_30min)
}  


# Save data
#save(Sentinel3_data_managed, file="Sentinel3_data_managed.RData")

setwd("C:/Users/39349/Documents/Remote Sensing/1. Dati/Sentinel 3 data/")
save(Sentinel3_data_managed, file="Sentinel3_data_managed_v2.RData")


############################### LOAD INFO FOR ALL THE STATIONS ###############################

setwd("C:/Users/39349/Documents/Remote Sensing/1. Dati/Info data")

STAZIONI_ALL_INFO<-data.frame(read_excel("STAZIONI_ALL_INFO.xlsx"))

  
Merged_STAZIONI_ALL_INFO<-merge(STAZIONI_ALL_INFO, Sentinel3_data_Info, by.x = "SENTINEL", by.y = "Identifier") 
Merged_STAZIONI_ALL_INFO$Latitude<-as.numeric(as.character(Merged_STAZIONI_ALL_INFO$Latitude))
Merged_STAZIONI_ALL_INFO$Longitude<-as.numeric(as.character(Merged_STAZIONI_ALL_INFO$Longitude))

Merged_STAZIONI_ALL_INFO$Distance<-sapply(1:nrow(Merged_STAZIONI_ALL_INFO),function(x){
  geodist(Merged_STAZIONI_ALL_INFO$Lat[x], Merged_STAZIONI_ALL_INFO$Long[x], Merged_STAZIONI_ALL_INFO$Latitude[x], Merged_STAZIONI_ALL_INFO$Longitude[x], units="km")})

save(Merged_STAZIONI_ALL_INFO, file="Merged_STAZIONI_ALL_INFO.RData")

            #######################  PO RIVER STATIONS ###########################
###################################################################################

#Load Idrometer data

#computer portatile
setwd("C:/Users/39349/Documents/Remote Sensing/1. Dati/In-situ data/Po")

Coordinate_stazioni<-read_excel("CoordinateStazioni.xlsx")
Hydrometric_file <-  readLines("ElencoFile.txt")



#Readexcelfile

HYDRO_data_PO<-lapply(1:length(Hydrometric_file),function(x){ read_excel(Hydrometric_file[x])})

names(HYDRO_data_PO)<-sapply(1:length(HYDRO_data_PO),function(x){ HYDRO_data_PO[[x]]$Stazione[1]})

####################### CREATION MERGED INFO MATRIX #########################


Merged_info<-merge(Coordinate_stazioni, Sentinel3_data_Info, by.x = "SENTINEL", by.y = "Identifier") 

Merged_info$DataStart<-lapply(1:nrow(Merged_info),function(x){ paste0(substring(Merged_info$Time_start[[x]],1,4),
                                                                      substring(Merged_info$Time_start[[x]],6,7),
                                                                      substring(Merged_info$Time_start[[x]],9,10))})

Merged_info$DataEnd<-lapply(1:nrow(Merged_info),function(x){ paste0(substring(Merged_info$Time_end[[x]],1,4),
                                                                      substring(Merged_info$Time_end[[x]],6,7),
                                                                      substring(Merged_info$Time_end[[x]],9,10))})
Merged_info<-data.frame(Merged_info)



lapply(1:nrow(Merged_info),function(x){ paste0(substring(Merged_info$Time_start[[x]],1,4),
                                               substring(Merged_info$Time_start[[x]],6,7),
                                               substring(Merged_info$Time_start[[x]],9,10))})



############### Merging Part: Matching Data for Po RIVER ##################################################

PO_COUPLES<-which(STAZIONI_ALL_INFO$Corso.d.acqua=="Po" & STAZIONI_ALL_INFO$Regione!="VENETO"  )

xxx<-1
SENT_HYDRO_data_PO<-list()
check_ok<-c(0)

for (xx in PO_COUPLES)
{ 
  Pos_stat<-which(names(HYDRO_data_PO)== STAZIONI_ALL_INFO$NomeStazione[xx])
  Pos_sent<-which(Sentinel3_data_Info$Identifier==STAZIONI_ALL_INFO$SENTINEL[xx])
  
  SENT_HYDRO_data_PO[[xxx]]<-data.frame( merge(Sentinel3_data_managed[[Pos_sent]],HYDRO_data_PO[[Pos_stat]],
                                              by.x = c("Data_format","Hour_format"), by.y = c( "data","ora"))) 
  
  
  # Calculating the difference 
  SENT_HYDRO_data_PO[[xxx]]$Water.level<-as.numeric(as.character(SENT_HYDRO_data_PO[[xxx]]$Water.level))
  SENT_HYDRO_data_PO[[xxx]]$H.cm<-as.numeric(as.character(SENT_HYDRO_data_PO[[xxx]]$H.cm))
  
  SENT_HYDRO_data_PO[[xxx]]<-SENT_HYDRO_data_PO[[xxx]] %>%mutate(Hydro_diff = H.cm - lag(H.cm, default = first(H.cm))) 
  SENT_HYDRO_data_PO[[xxx]]<-SENT_HYDRO_data_PO[[xxx]] %>%mutate(Sentinel_diff = Water.level - lag(Water.level, default = first(Water.level)))
  
  xxx<-xxx+1
}

#names(SENT_HYDRO_data_PO)<-names(HYDRO_data_PO)
names(SENT_HYDRO_data_PO)<-STAZIONI_ALL_INFO$NomeStazione2[PO_COUPLES]


# Save data
#save(HYDRO_data_PO,SENT_HYDRO_data_PO, file="SENT_HYDRO_data_PO.RData")
save(SENT_HYDRO_data_PO, file="SENT_HYDRO_data_PO_v2.RData")

              ###################################################################################
########################################### LOMBARDIA #####################################################################
              ###################################################################################

setwd("C:/Users/39349/Documents/Remote Sensing/1. Dati/In-situ data/Lombardia/Pizzighettone")


Pizzighettone_2019<-data.frame(read.table("Pizzighettone_2019.csv", skip=1,sep=","))

Pizzighettone_2020<-data.frame(read.table("Pizzighettone_2020.csv", skip=1,sep=","))

Pizzighettone<- rbind(Pizzighettone_2019,Pizzighettone_2020)
colnames(Pizzighettone)<-c("ID","yyyy.mm.dd.hh.mm","Livelli")

Pizzighettone$Livelli<-Pizzighettone$Livelli/100

Data_format<-paste0(substring(Pizzighettone$yyyy.mm.dd.hh.mm,1,4),
                    substring(Pizzighettone$yyyy.mm.dd.hh.mm,6,7),
                    substring(Pizzighettone$yyyy.mm.dd.hh.mm,9,10))

Hour_format<-as.numeric(paste0(substring(Pizzighettone$yyyy.mm.dd.hh.mm,12,13),
                               substring(Pizzighettone$yyyy.mm.dd.hh.mm,15,16)))

Pizzighettone_managed<-cbind(Pizzighettone, Data_format, Hour_format)


Sent_lomb<- Merged_STAZIONI_ALL_INFO$SENTINEL[which(Merged_STAZIONI_ALL_INFO$Stazione=="Pizzighettone")]
Pos_lomb<-which(Sentinel3_data_Info$Identifier==Sent_lomb)

SENT_HYDRO_Data_Lombardy<-data.frame( merge(Sentinel3_data_managed[[Pos_lomb]],Pizzighettone_managed,
                                            by.x = c("Data_format","Hour_format_15min"), by.y = c("Data_format","Hour_format")) )


SENT_HYDRO_Data_Lombardy$`Water.level`<-as.numeric(as.character(SENT_HYDRO_Data_Lombardy$`Water.level`))
SENT_HYDRO_Data_Lombardy$`Livelli`<-as.numeric(as.character(SENT_HYDRO_Data_Lombardy$`Livelli`))

SENT_HYDRO_Data_Lombardy<-SENT_HYDRO_Data_Lombardy%>%mutate(Hydro_diff = `Livelli` - lag(`Livelli`, default = first(`Livelli`))) 
SENT_HYDRO_Data_Lombardy<-SENT_HYDRO_Data_Lombardy%>%mutate(Sentinel_diff = `Water.level` - lag(`Water.level`, default = first(`Water.level`)))

# Calculating the difference 


# Save data
setwd("C:/Users/39349/Documents/Remote Sensing/1. Dati/In-situ data/Lombardia")

save(Pizzighettone,Pizzighettone_managed, file="HYDRO_data_Lombardia.RData")
save(SENT_HYDRO_Data_Lombardy, file="SENT_HYDRO_data_Lombardia_v2.RData")

###############################################################################################################################



                        #######################  UMBRIA STATIONS ###########################

setwd("C:/Users/39349/Documents/Remote Sensing/1. Dati/In-situ data/Umbria")

Ponte_Nuovo_Umbria<-read_excel("Ponte Nuovo - Umbria.xlsx")
Umbria_stazioni<-read_excel("Umbria_stazioni.xlsx")


Ponte_Nuovo_Umbria$Ora_format<-seq(0,23)*100
Ponte_Nuovo_Umbria$Data_format<-c(0)

Ponte_Nuovo_Umbria$Data_format<-sapply(1:nrow(Ponte_Nuovo_Umbria),function(x){ paste0(substring(Ponte_Nuovo_Umbria$Data[x],1,4),
                                                                                         substring(Ponte_Nuovo_Umbria$Data[x],6,7),
                                                                                         substring(Ponte_Nuovo_Umbria$Data[x],9,10))})

#Merging part

Sentinel_UMBRIA<-Merged_info$SENTINEL[which(Merged_info$HYDROMETER=="PONTE NUOVO")]

for( i in 1:length(Sentinel3_data_managed)){
                                    if(Sentinel3_data_managed[[i]][1,1]==Sentinel_UMBRIA){
                                      SENT_HYDRO_data_UMBRIA<- merge(Sentinel3_data_managed[[i]],Ponte_Nuovo_Umbria,
                                      by.x = c("Data_format","Hour_format"), by.y = c( "Data_format","Ora_format"))} 
                                        }   


SENT_HYDRO_data_UMBRIA$Livelli<-as.numeric(as.character(SENT_HYDRO_data_UMBRIA$Livelli))
SENT_HYDRO_data_UMBRIA$`Water level`<-as.numeric(as.character(SENT_HYDRO_data_UMBRIA$`Water level`))

SENT_HYDRO_data_UMBRIA<-SENT_HYDRO_data_UMBRIA %>%mutate(Hydro_diff = `Livelli` - 
                                                           lag(`Livelli`, default = first(`Livelli`))) 

SENT_HYDRO_data_UMBRIA<-SENT_HYDRO_data_UMBRIA %>%mutate(Sentinel_diff = `Water level` - lag(`Water level`, default = first(`Water level`)))

# Save data
#save(Ponte_Nuovo_Umbria,SENT_HYDRO_data_UMBRIA, file="SENT_HYDRO_data_UMBRIA.RData")
save(Ponte_Nuovo_Umbria,SENT_HYDRO_data_UMBRIA, file="SENT_HYDRO_data_UMBRIA_v2.RData")

###############################################################LAZIO STATIONS #############################################
                                                  ############################################################

setwd("C:/Users/39349/Documents/Remote Sensing/1. Dati/In-situ data/Lazio")

Stazioni_Lazio<-data.frame(read_excel("Elenco anagrafica e coordinate stazioni.xlsx"))

setwd("C:/Users/39349/Documents/Remote Sensing/1. Dati/In-situ data/Lazio/Tevere a Ponte Felice portate e livelli 2005_2019")

##Sentinel data managed for LAZIO

Laz_pos<-which(Sentinel3_data_Info$Identifier%in%Stazioni_Lazio$SENTINEL)
Sentinel_LAZIO<-Sentinel3_data_managed[Laz_pos]
Sentinel_LAZIO_managed<-list()

# 
# gg<-c(2045,945,945)
# 
# for (ii in 1:3 )
# {
#   Hour_format_15min<-rep(gg[ii],nrow(Sentinel_LAZIO[[ii]]))
#   
#   Sentinel_LAZIO_managed[[ii]]<-cbind(Sentinel_LAZIO[[ii]],Hour_format_15min)  
#   
# }
# 
# names(Sentinel_LAZIO_managed)<-c("9905","9906","9907")

#######  Hydrometric data managed for LAZIO  ###############

#Ponte Felice

Ponte_Felice_list<-list()

x<-0
for (gg in 2005:2019){
  
  
x<-x+1
Nome_laz_file<- paste0("Tevere a Ponte Felice portate e livelli ", gg,".csv")
Ponte_Felice_list [[x]]<-read.csv(Nome_laz_file,sep=";",skip=3,dec=",")
}

Ponte_Felice<- merge_recurse(Ponte_Felice_list)

#save(Ponte_Felice,Ponte_Felice_list,file="PonteFelice_processed.RData")

#Fiumara e Mezzocammino

setwd("C:/Users/39349/Documents/Remote Sensing/1. Dati/In-situ data/Lazio/Tevere livelli idrometrici 2005_2019")


Fiumara_list_1<-list()

x<-0
for (gg in 2005:2010){
  
  x<-x+1
  Nome_laz_file<- paste0("Tevere livelli idrometrici ", gg,".csv")
  Fiumara_list_1 [[x]]<-read.csv(Nome_laz_file,sep=";", dec = ",")
}

Fiumara_1<- merge_recurse(Fiumara_list_1)


Fiumara_list_2<-list()

x<-0
for (gg in 2011:2019){
  
  x<-x+1
  Nome_laz_file<- paste0("Tevere livelli idrometrici ", gg,".csv")
  Fiumara_list_2 [[x]]<-read.csv(Nome_laz_file,sep=";")
}

Fiumara_2<- merge_recurse(Fiumara_list_2)
#save(Ponte_Felice,Ponte_Felice_list,file="PonteFelice_processed.RData")



###################### HYDRO DATA ##################################

Fiumara<-rbind(Fiumara_1[,1:3],Fiumara_2[,1:3])
Mezzocammino<-rbind(Fiumara_1[,c(1,2,4)],Fiumara_2[,c(1,2,4)])
CapoDueRami<-Fiumara_2[,c(1,2,5)]
colnames(Fiumara)<-c("Data","Ora","Livelli")
colnames(Mezzocammino)<-c("Data","Ora","Livelli")
colnames(CapoDueRami)<-c("Data","Ora","Livelli")

Ponte_Felice<- data.frame(sapply(Ponte_Felice, gsub, pattern = ",", replacement= ".")  )
Fiumara<- data.frame(sapply(Fiumara, gsub, pattern = ",", replacement= ".")  )
Mezzocammino<- data.frame(sapply(Mezzocammino, gsub, pattern = ",", replacement= ".")  )
CapoDueRami<- data.frame(sapply(CapoDueRami, gsub, pattern = ",", replacement= ".")  )

HYDRO_data_LAZIO<-list(Ponte_Felice,Fiumara,Mezzocammino,CapoDueRami)

names(HYDRO_data_LAZIO)<-c("Ponte_Felice","Fiumara","Mezzocammino","CapoDueRami")



##################### HYDROMETRIC Data processing   #################################à

HYDRO_data_LAZIO_managed<-list()


for (xx in 1:length(HYDRO_data_LAZIO))
{
  
  Data_format<-sapply(1:nrow(HYDRO_data_LAZIO[[xx]]),function(x){ paste0(substring(HYDRO_data_LAZIO[[xx]]$Data[x],7,10),
                                                                         substring(HYDRO_data_LAZIO[[xx]]$Data[x],4,5),
                                                                         substring(HYDRO_data_LAZIO[[xx]]$Data[x],1,2))})
  
  
  
  Hour_format<-sapply(1:nrow(HYDRO_data_LAZIO[[xx]]),function(x){ as.numeric(paste0(substring(HYDRO_data_LAZIO[[xx]]$Ora[x],1,2),
                                                                                    substring(HYDRO_data_LAZIO[[xx]]$Ora[x],4,5)))})
  
  HYDRO_data_LAZIO_managed[[xx]]<-cbind(HYDRO_data_LAZIO[[xx]], Data_format, Hour_format)
  
}

names(HYDRO_data_LAZIO_managed)<-names(HYDRO_data_LAZIO)

############### Merging Part: Matching Data for Po RIVER ##########################??


SENT_HYDRO_data_LAZIO<-list()
check_ok<-c(0)

for (xx in 1:length(HYDRO_data_LAZIO_managed))
{ 
  Pos_stat<-which(names(HYDRO_data_LAZIO_managed[xx])== STAZIONI_ALL_INFO$NomeStazione)
  Pos_sent<-which(Sentinel3_data_Info$Identifier==STAZIONI_ALL_INFO$SENTINEL[Pos_stat])
  
  
  
  SENT_HYDRO_data_LAZIO[[xx]]<-data.frame( merge(Sentinel3_data_managed[[Pos_sent]],HYDRO_data_LAZIO_managed[[Pos_stat]],
                                                  by.x = c("Data_format","Hour_format_15min"), by.y = c( "Data_format","Hour_format"))) 
  
  
  # Calculating the difference 
  SENT_HYDRO_data_LAZIO[[xx]]$`Water.level`<-as.numeric(as.character(SENT_HYDRO_data_LAZIO[[xx]]$`Water.level`))
  SENT_HYDRO_data_LAZIO[[xx]]$`Livelli`<-as.numeric(as.character(SENT_HYDRO_data_LAZIO[[xx]]$`Livelli`))
  
  SENT_HYDRO_data_LAZIO[[xx]]<-SENT_HYDRO_data_LAZIO[[xx]] %>%mutate(Hydro_diff = `Livelli` - lag(`Livelli`, default = first(`Livelli`))) 
  SENT_HYDRO_data_LAZIO[[xx]]<-SENT_HYDRO_data_LAZIO[[xx]] %>%mutate(Sentinel_diff = `Water.level` - lag(`Water.level`, default = first(`Water.level`)))
  
}

names(SENT_HYDRO_data_LAZIO)<-names(HYDRO_data_LAZIO_managed)

setwd("C:/Users/39349/Documents/Remote Sensing/1. Dati/In-situ data/Lazio")

# Save data
#save(HYDRO_data_LAZIO_managed,HYDRO_data_LAZIO,SENT_HYDRO_data_LAZIO, file="HYDRO_data_LAZIO.RData")
save(HYDRO_data_LAZIO_managed,HYDRO_data_LAZIO,SENT_HYDRO_data_LAZIO, file="HYDRO_data_LAZIO_v2.RData")

############################################################### TOSCANA STATIONS #############################################
############################################################

setwd("C:/Users/39349/Documents/Remote Sensing/1. Dati/In-situ data/Toscana")

Dir0<-c("C:/Users/39349/Documents/Remote Sensing/1. Dati/In-situ data/Toscana/")
Toscana_stat<-STAZIONI_ALL_INFO$Stazione[which(STAZIONI_ALL_INFO$Regione=="TOSCANA")]
Tosc_file<-STAZIONI_ALL_INFO$NomeStazione[which(STAZIONI_ALL_INFO$Regione=="TOSCANA")]

HYDRO_data_TOSCANA<-list()

for (tt in 1:length(Toscana_stat))
{
  Dir<-paste0(Dir0,Toscana_stat[tt])
  setwd(Dir)
  List1<-list()
  Anno_i<-STAZIONI_ALL_INFO$Anno[which(STAZIONI_ALL_INFO$Stazione==Toscana_stat[tt])]
  
  x<-0
  for (gg in Anno_i:2020){
    
    x<-x+1
    Nome_tosc_file<- paste0(Tosc_file[tt],"_", gg,".csv")
    List1 [[x]]<-read.csv(Nome_tosc_file,sep=";",row.names = NULL)
  }
  
  HYDRO_data_TOSCANA[[tt]]<-List1

}

names(HYDRO_data_TOSCANA)<-Tosc_file


##################### HYDROMETRIC Data processing   ###################################

HYDRO_data_TOSCANA_managed<-HYDRO_data_TOSCANA

for (xx in 1:length(HYDRO_data_TOSCANA))
{
  if(ncol(HYDRO_data_TOSCANA[[xx]][[1]])!=3){
    for (gg in 1:length(HYDRO_data_TOSCANA[[xx]])){
      Data_format<-sapply(1:length(HYDRO_data_TOSCANA[[xx]][[gg]]$yyyy.mm.dd.hh.mm),function(x){ paste0(substring(HYDRO_data_TOSCANA[[xx]][[gg]]$yyyy.mm.dd.hh.mm[x],1,4),
                                                                                                        substring(HYDRO_data_TOSCANA[[xx]][[gg]]$yyyy.mm.dd.hh.mm[x],6,7),
                                                                                                        substring(HYDRO_data_TOSCANA[[xx]][[gg]]$yyyy.mm.dd.hh.mm[x],9,10))})
      
      
      
      Hour_format<-sapply(1:length(HYDRO_data_TOSCANA[[xx]][[gg]]$yyyy.mm.dd.hh.mm),function(x){ as.numeric(paste0(substring(HYDRO_data_TOSCANA[[xx]][[gg]]$yyyy.mm.dd.hh.mm[x],12,13),
                                                                                                                   substring(HYDRO_data_TOSCANA[[xx]][[gg]]$yyyy.mm.dd.hh.mm[x],15,16)))})
      
      HYDRO_data_TOSCANA_managed[[xx]][[gg]]<-cbind(HYDRO_data_TOSCANA[[xx]][[gg]], Data_format, Hour_format)
    }} else{
      
      for (gg in 1:length(HYDRO_data_TOSCANA[[xx]])){
        Data_format<-sapply(1:length(HYDRO_data_TOSCANA[[xx]][[gg]]$yyyy.mm.dd.hh.mm),function(x){ paste0(substring(HYDRO_data_TOSCANA[[xx]][[gg]]$row.names[x],1,4),
                                                                                                          substring(HYDRO_data_TOSCANA[[xx]][[gg]]$row.names[x],6,7),
                                                                                                          substring(HYDRO_data_TOSCANA[[xx]][[gg]]$row.names[x],9,10))})
        
        
        
        Hour_format<-sapply(1:length(HYDRO_data_TOSCANA[[xx]][[gg]]$yyyy.mm.dd.hh.mm),function(x){ as.numeric(paste0(substring(HYDRO_data_TOSCANA[[xx]][[gg]]$row.names[x],12,13),
                                                                                                                     substring(HYDRO_data_TOSCANA[[xx]][[gg]]$row.names[x],15,16)))})
        
        HYDRO_data_TOSCANA_managed[[xx]][[gg]]<-cbind(HYDRO_data_TOSCANA[[xx]][[gg]], Data_format, Hour_format)
        names(HYDRO_data_TOSCANA_managed[[xx]][[gg]])<-c( "yyyy.mm.dd.hh.mm","valore","Bo", "Data_format", "Hour_format" )
      }
    }
}


setwd("C:/Users/39349/Documents/Remote Sensing/1. Dati/In-situ data/Toscana")

save(HYDRO_data_TOSCANA,HYDRO_data_TOSCANA_managed, file="HYDRO_data_TOSCANA.RData")

##########################################################################################
##################################     MERGING DATA      ###############################

SENT_HYDRO_data_TOSCANA<-list()

for (xx in 1:length(HYDRO_data_TOSCANA_managed))
{
  Pos_TOSCANA<-which(names(HYDRO_data_TOSCANA_managed[xx])== STAZIONI_ALL_INFO$NomeStazione)
  
  PosTsent<-which(Sentinel3_data_Info$Identifier==STAZIONI_ALL_INFO$SENTINEL[Pos_TOSCANA])
  
  
  SENT_HYDRO_data_workon<-list()
  SENT_HYDRO_data_workon<-lapply(1:length(HYDRO_data_TOSCANA[[xx]]), function(x){ 
    
                                            data.frame( merge(Sentinel3_data_managed[[PosTsent]],HYDRO_data_TOSCANA_managed[[xx]][[x]],
                                                   by.x = c("Data_format","Hour_format_15min"), by.y = c( "Data_format","Hour_format")))}) 
  
  Sent_j<-merge_recurse(SENT_HYDRO_data_workon)
  if(is.na(Sent_j$Identifier[1])){Sent_j$Identifier[1]<-STAZIONI_ALL_INFO$SENTINEL[Pos_TOSCANA]}
  SENT_HYDRO_data_TOSCANA[[xx]]<-Sent_j
}


  # Calculating the difference 

for (xx in 1:length(SENT_HYDRO_data_TOSCANA))
{

  SENT_HYDRO_data_TOSCANA[[xx]]$`Water.level`<-as.numeric(as.character(SENT_HYDRO_data_TOSCANA[[xx]]$`Water.level`))
  SENT_HYDRO_data_TOSCANA[[xx]]$valore<-as.numeric(as.character(SENT_HYDRO_data_TOSCANA[[xx]]$valore))
  
  SENT_HYDRO_data_TOSCANA[[xx]]<-SENT_HYDRO_data_TOSCANA[[xx]] %>%mutate(Hydro_diff = valore - lag(valore, default = first(valore))) 
  SENT_HYDRO_data_TOSCANA[[xx]]<-SENT_HYDRO_data_TOSCANA[[xx]] %>%mutate(Sentinel_diff = `Water.level` - lag(`Water.level`, default = first(`Water.level`)))
  
}


names(SENT_HYDRO_data_TOSCANA)<-names(HYDRO_data_TOSCANA_managed)

# SAVE 
#save(SENT_HYDRO_data_TOSCANA,HYDRO_data_TOSCANA, HYDRO_data_TOSCANA_managed, file="SENT_HYDRO_data_TOSCANA.RData")
save(SENT_HYDRO_data_TOSCANA, file="SENT_HYDRO_data_TOSCANA_v2.RData")


############################################################### VENETO STATIONS #############################################
                                            ############################################################


setwd("C:/Users/39349/Documents/Remote Sensing/1. Dati/In-situ data/Veneto")

Adige_Po_list<-lapply(2:13,function(x){ data.frame(read_excel("Adige_Po.xls",sheet=x,skip=3))})
Adige_Po_name<-lapply(2:13,function(x){ data.frame(read_excel("Adige_Po.xls",sheet=x)[1:3,])})
Livenza_list<-lapply(1:5,function(x){ data.frame(read_excel("Livenza.xls",sheet=x))})


Livenza<- merge_recurse(Livenza_list)
Livenza<- Livenza[,1:2]

Adige_parte1<- merge_recurse(Adige_Po_list[1:8])
Adige_parte2<- merge_recurse(Adige_Po_list[9:12])
Adige_parte2<- Adige_parte2[last(which(is.na(Adige_parte2$Data.Ora...1))):nrow(Adige_parte2),]

colnames(Adige_parte2)<-names(Adige_Po_name[[10]])

Ad_b<-Adige_parte2[2:nrow(Adige_parte2),9:10]
colnames(Ad_b)<-colnames(Adige_parte1)

Adige.a.Boara.Pisani<- rbind(Adige_parte1, Ad_b)
Adige.a.Badia.Polesine<-Adige_parte2[2:nrow(Adige_parte2),1:2]
Adige.a.Cavarzere<-Adige_parte2[2:nrow(Adige_parte2),3:4]
Adige.a.Verona<-Adige_parte2[2:nrow(Adige_parte2),5:6]
Po.a.Ficarolo<-Adige_parte2[2:nrow(Adige_parte2),7:8]


HYDRO_data_VENETO<-list(Livenza,Adige.a.Boara.Pisani,Adige.a.Badia.Polesine,Adige.a.Cavarzere,Adige.a.Verona,Po.a.Ficarolo)
names(HYDRO_data_VENETO)<-c("Livenza","Adige.a.Boara.Pisani","Adige.a.Badia.Polesine","Adige.a.Cavarzere","Adige.a.Verona","Po.a.Ficarolo")

CName<-c("Data_Ora","Livelli")
HYDRO_data_VENETO<-lapply(HYDRO_data_VENETO, setNames, CName)


######### MAnaged #############

HYDRO_data_VENETO_managed<-list()


for (xx in 1:length(HYDRO_data_VENETO))
{
  
  Data_format<-sapply(1:nrow(HYDRO_data_VENETO[[xx]]),function(x){ paste0(substring(HYDRO_data_VENETO[[xx]]$Data_Ora[x],1,4),
                                                                          substring(HYDRO_data_VENETO[[xx]]$Data_Ora[x],6,7),
                                                                          substring(HYDRO_data_VENETO[[xx]]$Data_Ora[x],9,10))})
  
  
  Hour_format<-sapply(1:nrow(HYDRO_data_VENETO[[xx]]),function(x){ as.numeric(paste0(substring(HYDRO_data_VENETO[[xx]]$Data_Ora[x],12,13),
                                                                                     substring(HYDRO_data_VENETO[[xx]]$Data_Ora[x],15,16)))})
  
  #Hour_format<-sapply(1:nrow(HYDRO_data_VENETO[[xx]]),function(x){ as.numeric(strftime(HYDRO_data_VENETO[[xx]]$Data_Ora[x], format="%H%M"))})
  #######risolvere il problema degli zerozero
  
  HYDRO_data_VENETO_managed[[xx]]<-cbind(HYDRO_data_VENETO[[xx]], Data_format, Hour_format)
  
}

names(HYDRO_data_VENETO_managed)<-names(HYDRO_data_VENETO)


##################################     MERGING DATA      ###############################
xxx<-1

VENETO_COUPLES<-which(STAZIONI_ALL_INFO$Regione=="VENETO")

SENT_HYDRO_data_VENETO<-list()

for (xx in VENETO_COUPLES)
{ #print(xx)
  
  Pos_stat<-which(names(HYDRO_data_VENETO_managed)== STAZIONI_ALL_INFO$NomeStazione[xx])
  Pos_sent<-which(Sentinel3_data_Info$Identifier==STAZIONI_ALL_INFO$SENTINEL[xx])
  
  
  ########CODE ERRORRR#########
  #print("ERROR IN THE CODE:LOOK AGAIN!")
  
  SENT_HYDRO_data_VENETO[[xxx]]<-data.frame( merge(Sentinel3_data_managed[[Pos_sent]],HYDRO_data_VENETO_managed[[Pos_stat]],
                                                   by.x = c("Data_format","Hour_format_30min"), by.y = c( "Data_format","Hour_format"))) 
  
  
  # Calculating the difference 
  SENT_HYDRO_data_VENETO[[xxx]]$`Water.level`<-as.numeric(as.character(SENT_HYDRO_data_VENETO[[xxx]]$`Water.level`))
  SENT_HYDRO_data_VENETO[[xxx]]$`Livelli`<-as.numeric(as.character(SENT_HYDRO_data_VENETO[[xxx]]$`Livelli`))
  
  SENT_HYDRO_data_VENETO[[xxx]]<-SENT_HYDRO_data_VENETO[[xxx]] %>%mutate(Hydro_diff = `Livelli` - lag(`Livelli`, default = first(`Livelli`))) 
  SENT_HYDRO_data_VENETO[[xxx]]<-SENT_HYDRO_data_VENETO[[xxx]] %>%mutate(Sentinel_diff = `Water.level` - lag(`Water.level`, default = first(`Water.level`)))
  
  if(is.na(SENT_HYDRO_data_VENETO[[xxx]]$Identifier[1]))
  
  {
    SENT_HYDRO_data_VENETO[[xxx]]$Identifier[1]<-STAZIONI_ALL_INFO$SENTINEL[xx]
  }
  
  
  xxx<-xxx+1
}

#names(SENT_HYDRO_data_VENETO)<-names(HYDRO_data_VENETO_managed)
names(SENT_HYDRO_data_VENETO)<-STAZIONI_ALL_INFO$NomeStazione2[VENETO_COUPLES]

#save(SENT_HYDRO_data_VENETO,HYDRO_data_VENETO, HYDRO_data_VENETO_managed, file="SENT_HYDRO_data_VENETO.RData")
save(SENT_HYDRO_data_VENETO, file="SENT_HYDRO_data_VENETO_v2.RData")

#####################################################################################################################################################
#                                 END CODE: GO TO ANALYSIS OF DATA                                                                  #
# #####################################################################################################################################################


######################IF YOU HAVE ALREADY PO,UMBRIA, LAZIO DATA##############
Dir1<-c("C:/Users/39349/Documents/Remote Sensing/1. Dati/")
load(paste0(Dir1,"In-situ data/Po/SENT_HYDRO_data_PO_v2.RData"))
load(paste0(Dir1,"In-situ data/Umbria/SENT_HYDRO_data_UMBRIA_v2.RData"))
load(paste0(Dir1,"In-situ data/Lazio/HYDRO_data_LAZIO_v2.RData"))
load(paste0(Dir1,"In-situ data/Veneto/SENT_HYDRO_data_VENETO_v2.RData"))
load(paste0(Dir1,"In-situ data/Toscana/SENT_HYDRO_data_TOSCANA_v2.RData"))
load(paste0(Dir1,"In-situ data/Lombardia/SENT_HYDRO_data_Lombardy_v2.RData"))
# 
# 
# 
# ######################IF YOU HAVE ALREADY PO,UMBRIA, LAZIO DATA##############
# Dir1<-c("C:/Users/39349/Documents/Remote Sensing/1. Dati/")
# load(paste0(Dir1,"In-situ data/Po/SENT_HYDRO_data_PO.RData"))
# load(paste0(Dir1,"In-situ data/Umbria/SENT_HYDRO_data_UMBRIA.RData"))
# load(paste0(Dir1,"In-situ data/Lazio/HYDRO_data_LAZIO.RData"))
# load(paste0(Dir1,"In-situ data/Veneto/SENT_HYDRO_data_VENETO.RData"))
# load(paste0(Dir1,"In-situ data/Toscana/SENT_HYDRO_data_TOSCANA.RData"))
# 
# STAZIONI_ALL_INFO<-read_excel("STAZIONI_ALL_INFO.xlsx")
# STAZIONI_ALL_INFO<-data.frame(STAZIONI_ALL_INFO)

############################## MERGE ALL LIST ###############################################

SENT_HYDRO_data_ALL<-append( SENT_HYDRO_data_PO, SENT_HYDRO_data_LAZIO)

SENT_HYDRO_data_ALL<-append(SENT_HYDRO_data_ALL , SENT_HYDRO_data_VENETO)

SENT_HYDRO_data_ALL<-append(SENT_HYDRO_data_ALL , SENT_HYDRO_data_TOSCANA)

SENT_HYDRO_data_ALL$Ponte_Nuovo<-SENT_HYDRO_data_UMBRIA

SENT_HYDRO_data_ALL$Pizzighettone<-SENT_HYDRO_Data_Lombardy

# Save data
setwd("C:/Users/39349/Documents/Remote Sensing/3. Results")
save(SENT_HYDRO_data_ALL, file="SENT_HYDRO_data_ALL_v2.RData")

#################ENDCODE########################################




