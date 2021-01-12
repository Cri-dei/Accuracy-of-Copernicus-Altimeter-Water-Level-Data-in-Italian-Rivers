#########################################
####### Code in situ comparison #########
#########################################
####  Author: Cristina Deidda    ########
#########################################

library("readxl")
library(tidyr)
library(ggpubr)
library(reshape)
library("dplyr")
library("gmt")

####################### PROCESSING UNFYING SENTINEL DATA  ###########################


#LOAD Sentinel 3 Processed data

load("C:/Users/39349/Documents/Remote Sensing/Sentinel 3 data/Sentinel3Data_processed.RData")


############### PROCESSING Sentinel Data  ######################

Sentinel3_data_managed<-list()


for (xx in 1:length(Sentinel3_data))
{
  
  Data_format<-sapply(1:nrow(Sentinel3_data[[xx]]),function(x){ paste0(substring(Sentinel3_data[[xx]][x,5],7,10),
                                                                       substring(Sentinel3_data[[xx]][x,5],4,5),
                                                                       substring(Sentinel3_data[[xx]][x,5],1,2))})
  
  
  
  Hour_format<-sapply(1:nrow(Sentinel3_data[[xx]]),function(x){ 
    h<-as.numeric(as.character(substring(Sentinel3_data[[xx]][x,5],12,13)))
    minutes<-as.numeric(as.character(substring(Sentinel3_data[[xx]][x,5],15,16)))
    ifelse(minutes> 30, (h+1)*10,  h*10)})
  
  Sentinel3_data_managed[[xx]]<-cbind(Sentinel3_data[[xx]], Data_format, Hour_format)
  
}



############################### LOAD INFO FOR ALL THE STATIONS ###############################

setwd("C:/Users/39349/Documents/Remote Sensing/DATA")

STAZIONI_ALL_INFO<-read_excel("STAZIONI_ALL_INFO.xlsx")

Merged_STAZIONI_ALL_INFO<-merge(STAZIONI_ALL_INFO, Sentinel3_data_Info, by.x = "SENTINEL", by.y = "Identifier") 
Merged_STAZIONI_ALL_INFO$Latitude<-as.numeric(as.character(Merged_STAZIONI_ALL_INFO$Latitude))
Merged_STAZIONI_ALL_INFO$Longitude<-as.numeric(as.character(Merged_STAZIONI_ALL_INFO$Longitude))

Merged_STAZIONI_ALL_INFO$Distance<-sapply(1:nrow(Merged_STAZIONI_ALL_INFO),function(x){
  geodist(Merged_STAZIONI_ALL_INFO$Lat[x], Merged_STAZIONI_ALL_INFO$Long[x], Merged_STAZIONI_ALL_INFO$Latitude[x], Merged_STAZIONI_ALL_INFO$Longitude[x], units="km")})


            #######################  PO RIVER STATIONS ###########################
###################################################################################

#Load Idrometer data

#computer portatile
setwd("C:/Users/39349/Documents/Remote Sensing/In-situ data/Po")

Coordinate_stazioni<-read_excel("CoordinateStazioni.xlsx")
Hydrometric_file <-  readLines("ElencoFile.txt")
Sentinel3_data_Info<-data.frame(Sentinel3_data_Info)


#Readexcelfile

Hydrometric_data<-lapply(1:length(Hydrometric_file),function(x){ read_excel(Hydrometric_file[x])})

#######################CREATION MERGED INFO MATRIX #########################

Station_hydrometric_data<-lapply(1:length(Hydrometric_data),function(x){ Hydrometric_data[[x]]$Stazione[1]})

Merged_info<-merge(Coordinate_stazioni, Sentinel3_data_Info, by.x = "SENTINEL", by.y = "Identifier") 

Merged_info$DataStart<-lapply(1:nrow(Merged_info),function(x){ paste0(substring(Merged_info$Time_start[[x]],1,4),
                                                                      substring(Merged_info$Time_start[[x]],6,7),
                                                                      substring(Merged_info$Time_start[[x]],9,10))})

Merged_info$DataEnd<-lapply(1:nrow(Merged_info),function(x){ paste0(substring(Merged_info$Time_end[[x]],1,4),
                                                                      substring(Merged_info$Time_end[[x]],6,7),
                                                                      substring(Merged_info$Time_end[[x]],9,10))})
Merged_info<-data.frame(Merged_info)



############### Merging Part: Matching Data for Po RIVER ##########################??


Position_st<-which(Sentinel3_data_Info$Identifier%in%Merged_info$SENTINEL)

Sentinel3_data_managed_Po<-Sentinel3_data_managed[Position_st]

SENT_HYDRO_data_PO<-list()
check_ok<-c(0)
ponames<-c(0)

for (xx in 1:length(Sentinel3_data_managed_Po))
{ 
  DD<-which(Merged_info$SENTINEL==Sentinel3_data_managed_Po[[xx]][1,1])
  if(Merged_info$Basin[DD]=="Po"){
  Hydro_name<-Coordinate_stazioni$HYDROMETER[DD]
  Hydro_data_match<-Hydrometric_data[[DD]]
  check_ok[xx]<- ifelse(Hydro_data_match$Stazione[xx]==Hydro_name, "ok","ERROR")
  SENT_HYDRO_data_PO[[xx]]<-merge(Sentinel3_data_managed_Po[[xx]],Hydro_data_match, by.x = c("Data_format","Hour_format"), by.y = c( "data","ora")) 
  ponames[xx]<-Hydro_data_match$Stazione[xx]
    
  # Calculating the difference 
  SENT_HYDRO_data_PO[[xx]]$`Water level`<-as.numeric(as.character(SENT_HYDRO_data_PO[[xx]]$`Water level`))
  SENT_HYDRO_data_PO[[xx]]$`H cm`<-as.numeric(as.character(SENT_HYDRO_data_PO[[xx]]$`H cm`))
  
  SENT_HYDRO_data_PO[[xx]]<-SENT_HYDRO_data_PO[[xx]] %>%mutate(Hydro_diff = `H cm` - lag(`H cm`, default = first(`H cm`))) 
  SENT_HYDRO_data_PO[[xx]]<-SENT_HYDRO_data_PO[[xx]] %>%mutate(Sentinel_diff = `Water level` - lag(`Water level`, default = first(`Water level`)))
    }
}


SENT_HYDRO_data_PO<-SENT_HYDRO_data_PO[-2]
names(SENT_HYDRO_data_PO)<-na.omit(ponames)

# Save data
save(Sentinel3_data_managed_Po,Hydrometric_data,SENT_HYDRO_data_PO, file="SENT_HYDRO_data_PO.RData")


##############################################################


                        #######################  UMBRIA STATIONS ###########################

setwd("C:/Users/39349/Documents/Remote Sensing/In-situ data/Umbria")

Ponte_Nuovo_Umbria<-read_excel("Ponte Nuovo - Umbria.xlsx")
Umbria_stazioni<-read_excel("Umbria_stazioni.xlsx")


Ponte_Nuovo_Umbria$Ora_format<-seq(0,23)*10
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
save(Ponte_Nuovo_Umbria,SENT_HYDRO_data_UMBRIA, file="SENT_HYDRO_data_UMBRIA.RData")


###############################################################LAZIO STATIONS #############################################
                                                  ############################################################

setwd("C:/Users/39349/Documents/Remote Sensing/In-situ data/Lazio")

Stazioni_Lazio<-data.frame(read_excel("Elenco anagrafica e coordinate stazioni.xlsx"))

setwd("C:/Users/39349/Documents/Remote Sensing/In-situ data/Lazio/Tevere a Ponte Felice portate e livelli 2005_2019")

##Sentinel data managed for LAZIO

Laz_pos<-which(Sentinel3_data_Info$Identifier%in%Stazioni_Lazio$SENTINEL)
Sentinel_LAZIO<-Sentinel3_data_managed[Laz_pos]
Sentinel_LAZIO_managed<-list()


gg<-c(2045,945,945)

for (ii in 1:3 )
{
  Hour_format_15min<-rep(gg[ii],nrow(Sentinel_LAZIO[[ii]]))
  
  Sentinel_LAZIO_managed[[ii]]<-cbind(Sentinel_LAZIO[[ii]],Hour_format_15min)  
  
}

names(Sentinel_LAZIO_managed)<-c("9905","9906","9907")

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

setwd("C:/Users/39349/Documents/Remote Sensing/In-situ data/Lazio/Tevere livelli idrometrici 2005_2019")


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
  
  St_select<-Stazioni_Lazio$SENTINEL[which(Stazioni_Lazio$Name.file==names(HYDRO_data_LAZIO_managed)[xx])]
  DD<-which(names(Sentinel_LAZIO_managed)==St_select)
  
  SENT_HYDRO_data_LAZIO[[xx]]<-merge(HYDRO_data_LAZIO_managed[[xx]],Sentinel_LAZIO_managed[[DD]], by.x = c("Data_format","Hour_format"), by.y = c( "Data_format","Hour_format_15min")) 
  
  
  # Calculating the difference 
  SENT_HYDRO_data_LAZIO[[xx]]$`Water level`<-as.numeric(as.character(SENT_HYDRO_data_LAZIO[[xx]]$`Water level`))
  SENT_HYDRO_data_LAZIO[[xx]]$`Livelli`<-as.numeric(as.character(SENT_HYDRO_data_LAZIO[[xx]]$`Livelli`))
  
  SENT_HYDRO_data_LAZIO[[xx]]<-SENT_HYDRO_data_LAZIO[[xx]] %>%mutate(Hydro_diff = `Livelli` - lag(`Livelli`, default = first(`Livelli`))) 
  SENT_HYDRO_data_LAZIO[[xx]]<-SENT_HYDRO_data_LAZIO[[xx]] %>%mutate(Sentinel_diff = `Water level` - lag(`Water level`, default = first(`Water level`)))
  
}

names(SENT_HYDRO_data_LAZIO)<-names(HYDRO_data_LAZIO_managed)


# Save data
save(HYDRO_data_LAZIO_managed,HYDRO_data_LAZIO,SENT_HYDRO_data_LAZIO, file="HYDRO_data_LAZIO.RData")




############################################################### VENETO STATIONS #############################################
                                            ############################################################


setwd("C:/Users/39349/Documents/Remote Sensing/In-situ data/Veneto")

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
                                                                                     substring(HYDRO_data_VENETO[[xx]]$Data_Ora[x],15,16)))/10})
  
  HYDRO_data_VENETO_managed[[xx]]<-cbind(HYDRO_data_VENETO[[xx]], Data_format, Hour_format)
  
}

names(HYDRO_data_VENETO_managed)<-names(HYDRO_data_VENETO)

##################################     MERGING DATA      ###############################

SENT_HYDRO_data_VENETO<-list()

for (xx in 1:length(HYDRO_data_VENETO_managed))
{
  Pos_Veneto<-which(names(HYDRO_data_VENETO_managed[xx])== STAZIONI_ALL_INFO$NomeStazione)
  
  Pos_Veneto<-ifelse (length(Pos_Veneto)==2, Pos_Veneto[2], Pos_Veneto )
  
  SENT_HYDRO_data_VENETO[[xx]]<-data.frame( merge(Sentinel3_data_managed[[Pos_Veneto]],HYDRO_data_VENETO_managed[[xx]],
                                            by.x = c("Data_format","Hour_format"), by.y = c( "Data_format","Hour_format"))) 

  
  # Calculating the difference 
  SENT_HYDRO_data_VENETO[[xx]]$`Water.level`<-as.numeric(as.character(SENT_HYDRO_data_VENETO[[xx]]$`Water.level`))
  SENT_HYDRO_data_VENETO[[xx]]$`Livelli`<-as.numeric(as.character(SENT_HYDRO_data_VENETO[[xx]]$`Livelli`))
  
  SENT_HYDRO_data_VENETO[[xx]]<-SENT_HYDRO_data_VENETO[[xx]] %>%mutate(Hydro_diff = `Livelli` - lag(`Livelli`, default = first(`Livelli`))) 
  SENT_HYDRO_data_VENETO[[xx]]<-SENT_HYDRO_data_VENETO[[xx]] %>%mutate(Sentinel_diff = `Water.level` - lag(`Water.level`, default = first(`Water.level`)))
  
}

names(SENT_HYDRO_data_VENETO)<-names(HYDRO_data_VENETO_managed)


save(SENT_HYDRO_data_VENETO,HYDRO_data_VENETO, HYDRO_data_VENETO_managed, file="SENT_HYDRO_data_VENETO.RData")

######################IF YOU HAVE ALREADY PO,UMBRIA, LAZIO DATA##############
load("C:/Users/39349/Documents/Remote Sensing/In-situ data/Po/SENT_HYDRO_data_PO.RData")
load("C:/Users/39349/Documents/Remote Sensing/In-situ data/Umbria/SENT_HYDRO_data_UMBRIA.RData")
LOad("C:/Users/39349/Documents/Remote Sensing/In-situ data/Lazio/HYDRO_data_LAZIO.RData")



############################## MERGE ALL LIST ###############################################

SENT_HYDRO_data_ALL<-append( SENT_HYDRO_data_PO, SENT_HYDRO_data_LAZIO)

SENT_HYDRO_data_ALL<-append(SENT_HYDRO_data_ALL , SENT_HYDRO_data_VENETO)

SENT_HYDRO_data_ALL$Ponte_Nuovo<-SENT_HYDRO_data_UMBRIA


# Save data
setwd("C:/Users/39349/Documents/Remote Sensing/RESULTS")
save(SENT_HYDRO_data_ALL, file="SENT_HYDRO_data_ALL.RData")




######################################PLOTS###############################################

setwd("C:/Users/39349/Documents/Remote Sensing/Plots")

Summary_results<-data.frame(matrix(,length(SENT_HYDRO_data_ALL),3))
colnames(Summary_results)<-c("Station","Correlation","Section")

for ( jj in 1:length(SENT_HYDRO_data_ALL))
{ 
  Summary_results$Station[[jj]]<- names(SENT_HYDRO_data_ALL[jj])
  Summary_results$Correlation[[jj]]<-cor(SENT_HYDRO_data_ALL[[jj]]$Hydro_diff, SENT_HYDRO_data_ALL[[jj]]$Sentinel_diff)
  gh_pos<-which(STAZIONI_ALL_INFO$NomeStazione==names(SENT_HYDRO_data_ALL[jj]))
  Summary_results$Section[[jj]]<-  STAZIONI_ALL_INFO$`Larghezza Alveo [m]`[gh_pos[1]]
}

 plot(Summary_results$Section,Summary_results$Correlation)
 
################################## PLOT PER ALLL ########################################

pdf("ALL STATIONS_050620_2.pdf",onefile = TRUE)

for ( jj in 1:length(SENT_HYDRO_data_ALL))
{ 
  data_ALL<- data.frame(SENT_HYDRO_data_ALL[[jj]]$Hydro_diff, SENT_HYDRO_data_ALL[[jj]]$Sentinel_diff, SENT_HYDRO_data_ALL[[jj]]$Data_format)
  colnames(data_ALL)<-c("Hydro","Sent","Data")
  
  df_long_ALL = gather(na.omit(data_ALL), key = var, value = value, Hydro, Sent)
  p1_ALL<-ggplot(df_long_ALL, aes(x = Data, y = value, fill = var)) +
    geom_bar(stat = 'identity', position = 'dodge')
  
  #Dist_SH<-round(Merged_STAZIONI_ALL_INFO$Distance[Merged_STAZIONI_ALL_INFO$NomeStazione==names(SENT_HYDRO_data_ALL)[[jj]]])
  #River_SH<-Merged_STAZIONI_ALL_INFO$River[Merged_STAZIONI_ALL_INFO$NomeStazione==names(SENT_HYDRO_data_ALL)[[jj]]]
  
  p1_ALL<-p1_ALL +theme(axis.text.x = element_text(face="bold", color="#993333",  size=6, angle=45))+
    ggtitle(paste("Stazione : ",names(SENT_HYDRO_data_ALL)[[jj]]))+
    xlab("Date(YMD)") + ylab("Level difference [m]")+
    theme(plot.title = element_text(hjust = 0.5, size=16, face="bold"), axis.title.x = element_text( size=13),
          axis.title.y = element_text(size=13))
  
  
  p2_ALL<- ggplot(na.omit(data_ALL) , aes(x=Hydro, y=Sent)) + geom_point()+
    xlim(min(na.omit(data_ALL[,1:2])-0.5),max(na.omit(data_ALL[,1:2]))+0.5) +
    ylim(min(na.omit(data_ALL[,1:2])-0.5),max(na.omit(data_ALL[,1:2]))+0.5) +
    theme(axis.text = element_text(size = 13), axis.title = element_text(size = 15))+
    xlab("Hydrometer level diff [m]") + ylab("Sentinel 3A level diff [m]") + theme(panel.grid.major = element_blank(),
                                                                                   plot.margin = margin(1.2, 1.2, 1.2, 1.2, "cm"),
                                                                                   panel.grid.minor = element_blank(), 
                                                                                   panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    stat_cor(label.x = min(na.omit(data_ALL[,1:2])-0.5), label.y =max(na.omit(data_ALL[,1:2])),size=5) +
    geom_abline(slope=1, intercept = 0)
  #geom_smooth(method = "lm", se = FALSE)+#stat_regline_equation(label.x = -2, label.y = 2)
  
  P_ALL<-ggarrange(p1_ALL,p2_ALL, ncol = 1, nrow = 2) 
  print(P_ALL)
}

dev.off()

#########################################################################





