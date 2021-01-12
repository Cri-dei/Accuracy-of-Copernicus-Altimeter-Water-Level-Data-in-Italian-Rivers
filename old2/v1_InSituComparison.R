#########################################
####### Code in situ comparison #########
#########################################
####  Author: Cristina Deidda    ########
#########################################

library("readxl")
library(tidyr)
library(ggpubr)
#LOAD Sentinel 3 Processed data

load("C:/Users/39349/Documents/Remote Sensing/Sentinel 3 data/Sentinel3Data_processed.RData")


#Load Idrometer data

#computer portatile
setwd("C:/Users/39349/Documents/Remote Sensing/In-situ data/Po")

Coordinate_stazioni<-read_excel("CoordinateStazioni.xlsx")
Hydrometric_file <-  readLines("ElencoFile.txt")
Sentinel3_data_Info<-data.frame(Sentinel3_data_Info)


#Readexcelfile

Hydrometric_data<-lapply(1:length(Hydrometric_file),function(x){ read_excel(Hydrometric_file[x])})

Station_hydrometric_data<-lapply(1:length(Hydrometric_data),function(x){ Hydrometric_data[[x]]$Stazione[1]})

Merged_info<-merge(Coordinate_stazioni, Sentinel3_data_Info, by.x = "SENTINEL", by.y = "Identifier") 

Merged_info$DataStart<-lapply(1:nrow(Merged_info),function(x){ paste0(substring(Merged_info$Time_start[[x]],1,4),
                                                                      substring(Merged_info$Time_start[[x]],6,7),
                                                                      substring(Merged_info$Time_start[[x]],9,10))})

Merged_info$DataEnd<-lapply(1:nrow(Merged_info),function(x){ paste0(substring(Merged_info$Time_end[[x]],1,4),
                                                                      substring(Merged_info$Time_end[[x]],6,7),
                                                                      substring(Merged_info$Time_end[[x]],9,10))})

              #######################  PO RIVER STATIONS ###########################

#SentinelData
Position_Po<-which(Sentinel3_data_Info$Identifier%in%Merged_info$SENTINEL)

#Sentinel3_data_Po<-Map(cbind, Sentinel3_data[Position_Po], Data=c(0))
Sentinel3_data_Po<- Sentinel3_data[Position_Po]
Sentinel3_data_Po_managed<-list()


for (xx in 1:length(Sentinel3_data_Po))
{
  Data_format<-sapply(1:nrow(Sentinel3_data_Po[[xx]]),function(x){ paste0(substring(Sentinel3_data_Po[[xx]][x,5],7,10),
                                                                                        substring(Sentinel3_data_Po[[xx]][x,5],4,5),
                                                                                         substring(Sentinel3_data_Po[[xx]][x,5],1,2))})
  
  
  
  Hour_format<-sapply(1:nrow(Sentinel3_data_Po[[xx]]),function(x){ 
                                                                  h<-as.numeric(as.character(substring(Sentinel3_data_Po[[xx]][x,5],12,13)))
                                                                  minutes<-as.numeric(as.character(substring(Sentinel3_data_Po[[xx]][x,5],15,16)))
                                                                  ifelse(minutes> 30, (h+1)*10,  h*10)})
  
  Sentinel3_data_Po_managed[[xx]]<-cbind(Sentinel3_data_Po[[xx]], Data_format, Hour_format)
  
  }



SENT_HYDRO_data<-list()
check_ok<-list()

for (xx in 1:length(Sentinel3_data_Po_managed))
{
  DD<-which(Coordinate_stazioni$SENTINEL==Sentinel3_data_Po_managed[[xx]][1,1])
  Hydro_name<-Coordinate_stazioni$HYDROMETER[DD]
  Hydro_data_match<-Hydrometric_data[[DD]]
  check_ok[xx]<- ifelse(Hydro_data_match$Stazione[1]==Hydro_name, "ok","ERROR")
  SENT_HYDRO_data[[xx]]<-merge(Sentinel3_data_Po_managed[[xx]],Hydro_data_match, by.x = c("Data_format","Hour_format"), by.y = c( "data","ora"))
  
}


SENT_HYDRO_data$`Water level`<-as.numeric(as.character(SENT_HYDRO_data$`Water level`))
SENT_HYDRO_data$`H cm`<-as.numeric(as.character(SENT_HYDRO_data$`H cm`))

SENT_HYDRO_data<-SENT_HYDRO_data %>%mutate(Hydro_diff = `H cm` - lag(`H cm`, default = first(`H cm`))) 

SENT_HYDRO_data_PO<-SENT_HYDRO_data %>%mutate(Sentinel_diff = `Water level` - lag(`Water level`, default = first(`Water level`)))
 

################ BAR PLOT Comparison #########################

data <- data.frame(SENT_HYDRO_data_PO$Hydro_diff, SENT_HYDRO_data_PO$Sentinel_diff, SENT_HYDRO_data_PO$Data_format)
#data <- data.frame(SENT_Hydro_1$Hydro_diff, SENT_Hydro_1$Sentinel_diff,seq(1,nrow(data)))
colnames(data)<-c("Hydro","Sent","Data")

df_long = gather(data, key = var, value = value, Hydro, Sent)
ggplot(df_long, aes(x = Data, y = value, fill = var)) +
  geom_bar(stat = 'identity', position = 'dodge')

##################################################ààààààààààààààà


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
POS_UMBRIA<-which(Sentinel3_data_Info$Identifier==Sentinel_UMBRIA)

Sentinel3_data_UMBRIA<-data.frame(Sentinel3_data[[POS_UMBRIA]])

Sentinel3_data_UMBRIA$Data_format<-sapply(1:nrow(Sentinel3_data_UMBRIA),function(x){ paste0(substring(Sentinel3_data_UMBRIA[x,5],7,10),
                                                                                      substring(Sentinel3_data_UMBRIA[x,5],4,5),
                                                                                        substring(Sentinel3_data_UMBRIA[x,5],1,2))})



Sentinel3_data_UMBRIA$Hour_format<-sapply(1:nrow(Sentinel3_data_UMBRIA),function(x){ 
                                                                  h<-as.numeric(as.character(substring(Sentinel3_data_UMBRIA[x,5],12,13)))
                                                                  minutes<-as.numeric(as.character(substring(Sentinel3_data_UMBRIA[x,5],15,16)))
                                                                  ifelse(minutes> 30, (h+1)*10,  h*10)})


SENT_HYDRO_data_UMBRIA<-merge(Sentinel3_data_UMBRIA,Ponte_Nuovo_Umbria, by.x = c("Data_format","Hour_format"), by.y = c( "Data_format","Ora_format"))




SENT_HYDRO_data_UMBRIA$Livelli<-as.numeric(as.character(SENT_HYDRO_data_UMBRIA$Livelli))
SENT_HYDRO_data_UMBRIA$Water.level<-as.numeric(as.character(SENT_HYDRO_data_UMBRIA$Water.level))

SENT_HYDRO_data_UMBRIA<-SENT_HYDRO_data_UMBRIA %>%mutate(Hydro_diff = `Livelli` - 
                                                           lag(`Livelli`, default = first(`Livelli`))) 

SENT_HYDRO_data_UMBRIA<-SENT_HYDRO_data_UMBRIA %>%mutate(Sentinel_diff = `Water.level` - lag(`Water.level`, default = first(`Water.level`)))


#PLOT

data_umbria <- data.frame(SENT_HYDRO_data_UMBRIA$Hydro_diff, SENT_HYDRO_data_UMBRIA$Sentinel_diff, SENT_HYDRO_data_UMBRIA$Data_format)
#data <- data.frame(SENT_Hydro_1$Hydro_diff, SENT_Hydro_1$Sentinel_diff,seq(1,nrow(data)))
colnames(data_umbria)<-c("Hydro","Sent","Data")

df_long_umbria = gather(na.omit(data_umbria), key = var, value = value, Hydro, Sent)
p1_Umbria<-ggplot(df_long_umbria, aes(x = Data, y = value, fill = var)) +
  geom_bar(stat = 'identity', position = 'dodge')

p1_Umbria<-p1_Umbria +theme(axis.text.x = element_text(face="bold", color="#993333",  size=12, angle=45))+ ggtitle("Umbria -Stazione di Ponte Nuovo")+
  xlab("Date(YMD)") + ylab("Level difference [m]")+
  theme(plot.title = element_text(hjust = 0.5, size=16, face="bold"), axis.title.x = element_text( size=13),
        axis.title.y = element_text(size=13))



p2_Umbria<- ggplot(na.omit(data_umbria) , aes(x=Hydro, y=Sent)) + geom_point()+
  theme(axis.text = element_text(size = 13), axis.title = element_text(size = 15)) +
  xlab("Hydrometer level diff [m]") + ylab("Sentinel 3A level diff [m]") + theme(panel.grid.major = element_blank(),
                                                  plot.margin = margin(1.2, 1.2, 1.2, 1.2, "cm"),
                                                  panel.grid.minor = element_blank(), 
                                                  panel.background = element_blank(), axis.line = element_line(colour = "black"))



P_UM<-ggarrange(p1_Umbria,p2_Umbria, ncol = 1, nrow = 2)
#PDF

library(ggplot2)


pdf("allplots.pdf",onefile = TRUE)

  print(P_UM)

dev.off()





