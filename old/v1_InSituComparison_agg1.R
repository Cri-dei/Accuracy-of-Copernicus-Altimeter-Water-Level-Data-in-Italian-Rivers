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

####################### PROCESSING UNFYING SENTINEL DATA  ###########################


#LOAD Sentinel 3 Processed data

load("C:/Users/39349/Documents/Remote Sensing/Sentinel 3 data/Sentinel3Data_processed.RData")

############### PROCESSING Sentinel Data  ######################àà

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

for (xx in 1:length(Sentinel3_data_managed_Po))
{ 
  DD<-which(Merged_info$SENTINEL==Sentinel3_data_managed_Po[[xx]][1,1])
  if(Merged_info$Basin[DD]=="Po"){
  Hydro_name<-Coordinate_stazioni$HYDROMETER[DD]
  Hydro_data_match<-Hydrometric_data[[DD]]
  check_ok[xx]<- ifelse(Hydro_data_match$Stazione[xx]==Hydro_name, "ok","ERROR")
  SENT_HYDRO_data_PO[[xx]]<-merge(Sentinel3_data_managed_Po[[xx]],Hydro_data_match, by.x = c("Data_format","Hour_format"), by.y = c( "data","ora")) 
  
  # Calculating the difference 
  SENT_HYDRO_data_PO[[xx]]$`Water level`<-as.numeric(as.character(SENT_HYDRO_data_PO[[xx]]$`Water level`))
  SENT_HYDRO_data_PO[[xx]]$`H cm`<-as.numeric(as.character(SENT_HYDRO_data_PO[[xx]]$`H cm`))
  
  SENT_HYDRO_data_PO[[xx]]<-SENT_HYDRO_data_PO[[xx]] %>%mutate(Hydro_diff = `H cm` - lag(`H cm`, default = first(`H cm`))) 
  SENT_HYDRO_data_PO[[xx]]<-SENT_HYDRO_data_PO[[xx]] %>%mutate(Sentinel_diff = `Water level` - lag(`Water level`, default = first(`Water level`)))
    }
}


################ BAR PLOT Comparison #########################

data <- data.frame(SENT_HYDRO_data_PO$Hydro_diff, SENT_HYDRO_data_PO$Sentinel_diff, SENT_HYDRO_data_PO$Data_format)
#data <- data.frame(SENT_Hydro_1$Hydro_diff, SENT_Hydro_1$Sentinel_diff,seq(1,nrow(data)))
colnames(data)<-c("Hydro","Sent","Data")

df_long = gather(data, key = var, value = value, Hydro, Sent)
ggplot(df_long, aes(x = Data, y = value, fill = var)) +
  geom_bar(stat = 'identity', position = 'dodge')

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


###################### HYDRO DATA ##################################à

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



######################################PLOTS###############################################

setwd("C:/Users/39349/Documents/Remote Sensing/Plots")

##################################PLOT PER LAZIO ########################################

pdf("LAZIO.pdf",onefile = TRUE)

for ( jj in 1:length(SENT_HYDRO_data_LAZIO))
{ 
  data_LAZIO<- data.frame(SENT_HYDRO_data_LAZIO[[jj]]$Hydro_diff, SENT_HYDRO_data_LAZIO[[jj]]$Sentinel_diff, SENT_HYDRO_data_LAZIO[[jj]]$Data_format)
  colnames(data_LAZIO)<-c("Hydro","Sent","Data")
  
  df_long_LAZIO = gather(na.omit(data_LAZIO), key = var, value = value, Hydro, Sent)
  p1_LAZIO<-ggplot(df_long_LAZIO, aes(x = Data, y = value, fill = var)) +
    geom_bar(stat = 'identity', position = 'dodge')
  
  p1_LAZIO<-p1_LAZIO +theme(axis.text.x = element_text(face="bold", color="#993333",  size=6, angle=45))+
    ggtitle(paste("Tevere - Stazione : ",names(SENT_HYDRO_data_LAZIO)[[jj]]))+
    xlab("Date(YMD)") + ylab("Level difference [m]")+
    theme(plot.title = element_text(hjust = 0.5, size=16, face="bold"), axis.title.x = element_text( size=13),
          axis.title.y = element_text(size=13))
  
  
  p2_LAZIO<- ggplot(na.omit(data_LAZIO) , aes(x=Hydro, y=Sent)) + geom_point()+
    xlim(min(na.omit(data_LAZIO[,1:2])-0.5),max(na.omit(data_LAZIO[,1:2]))+0.5) +
    ylim(min(na.omit(data_LAZIO[,1:2])-0.5),max(na.omit(data_LAZIO[,1:2]))+0.5) +
    theme(axis.text = element_text(size = 13), axis.title = element_text(size = 15))+
    xlab("Hydrometer level diff [m]") + ylab("Sentinel 3A level diff [m]") + theme(panel.grid.major = element_blank(),
                                                                                   plot.margin = margin(1.2, 1.2, 1.2, 1.2, "cm"),
                                                                                   panel.grid.minor = element_blank(), 
                                                                                   panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    stat_cor(label.x = min(na.omit(data_LAZIO[,1:2])-0.5), label.y =max(na.omit(data_LAZIO[,1:2])),size=5) +
    geom_abline(slope=1, intercept = 0)
  #geom_smooth(method = "lm", se = FALSE)+#stat_regline_equation(label.x = -2, label.y = 2)
  
  P_LAZIO<-ggarrange(p1_LAZIO,p2_LAZIO, ncol = 1, nrow = 2) 
  print(P_LAZIO)
}
dev.off()

#########################################


  #PLOT

data_umbria <- data.frame(SENT_HYDRO_data_UMBRIA$Hydro_diff, SENT_HYDRO_data_UMBRIA$Sentinel_diff, SENT_HYDRO_data_UMBRIA$Data_format)
#data <- data.frame(SENT_Hydro_1$Hydro_diff, SENT_Hydro_1$Sentinel_diff,seq(1,nrow(data)))
colnames(data_umbria)<-c("Hydro","Sent","Data")

df_long_umbria = gather(na.omit(data_umbria), key = var, value = value, Hydro, Sent)
p1_Umbria<-ggplot(df_long_umbria, aes(x = Data, y = value, fill = var)) +
  geom_bar(stat = 'identity', position = 'dodge')

p1_Umbria<-p1_Umbria +theme(axis.text.x = element_text(face="bold", color="#993333",  size=12, angle=45))+ ggtitle("Tevere - Stazione: Ponte Nuovo")+
  xlab("Date(YMD)") + ylab("Level difference [m]")+
  theme(plot.title = element_text(hjust = 0.5, size=16, face="bold"), axis.title.x = element_text( size=13),
        axis.title.y = element_text(size=13))


p2_Umbria<- ggplot(na.omit(data_umbria) , aes(x=Hydro, y=Sent)) + geom_point()+
  xlim(min(na.omit(data_umbria[,1:2]))+0.5,max(na.omit(data_umbria[,1:2]))+0.5) +
  ylim(min(na.omit(data_umbria[,1:2]))+0.5,max(na.omit(data_umbria[,1:2]))+0.5) +
  theme(axis.text = element_text(size = 13), axis.title = element_text(size = 15)) +
  xlab("Hydrometer level diff [m]") + ylab("Sentinel 3A level diff [m]") + theme(panel.grid.major = element_blank(),
                                                                                 plot.margin = margin(1.2, 1.2, 1.2, 1.2, "cm"),
                                                                                 panel.grid.minor = element_blank(), 
                                                                                 panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  stat_cor(label.x = min(na.omit(data_LAZIO[,1:2])-0.5), label.y =max(na.omit(data_LAZIO[,1:2])),size=5) +
  geom_abline(slope=1, intercept = 0)

                                                    
P_UM<-ggarrange(p1_Umbria,p2_Umbria, ncol = 1, nrow = 2) 


#PDF



pdf("POeUMBRIA.pdf",onefile = TRUE)

for ( jj in c(1,3,4,5))
{ 
  data_Po<- data.frame(SENT_HYDRO_data_PO[[jj]]$Hydro_diff, SENT_HYDRO_data_PO[[jj]]$Sentinel_diff, SENT_HYDRO_data_PO[[jj]]$Data_format)
  #data <- data.frame(SENT_Hydro_1$Hydro_diff, SENT_Hydro_1$Sentinel_diff,seq(1,nrow(data)))
  colnames(data_Po)<-c("Hydro","Sent","Data")
  
  df_long_Po = gather(na.omit(data_Po), key = var, value = value, Hydro, Sent)
  p1_Po<-ggplot(df_long_Po, aes(x = Data, y = value, fill = var)) +
    geom_bar(stat = 'identity', position = 'dodge')
  
  p1_Po<-p1_Po +theme(axis.text.x = element_text(face="bold", color="#993333",  size=12, angle=45))+
   ggtitle(paste("Po - Stazione : ",SENT_HYDRO_data_PO[[jj]]$Stazione[1]))+
    xlab("Date(YMD)") + ylab("Level difference [m]")+
    theme(plot.title = element_text(hjust = 0.5, size=16, face="bold"), axis.title.x = element_text( size=13),
          axis.title.y = element_text(size=13))
  
  
  p2_Po<- ggplot(na.omit(data_Po) , aes(x=Hydro, y=Sent)) + geom_point()+
    xlim(min(na.omit(data_Po[,1:2]))+0.5,max(na.omit(data_Po[,1:2]))+0.5) +
    ylim(min(na.omit(data_Po[,1:2]))+0.5,max(na.omit(data_Po[,1:2]))+0.5) +
    theme(axis.text = element_text(size = 13), axis.title = element_text(size = 15)) +
    xlab("Hydrometer level diff [m]") + ylab("Sentinel 3A level diff [m]") + theme(panel.grid.major = element_blank(),
                                                                                   plot.margin = margin(1.2, 1.2, 1.2, 1.2, "cm"),
                                                                                   panel.grid.minor = element_blank(), 
                                                                                   panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    stat_cor(label.x = min(na.omit(data_LAZIO[,1:2])-0.5), label.y =max(na.omit(data_LAZIO[,1:2])),size=5) +
    geom_abline(slope=1, intercept = 0)
  
  P_PO<-ggarrange(p1_Po,p2_Po, ncol = 1, nrow = 2) 
  print(P_PO)
}


print(P_UM)

dev.off()


