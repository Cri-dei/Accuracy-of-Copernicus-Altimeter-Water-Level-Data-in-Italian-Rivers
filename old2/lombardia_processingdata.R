
setwd("C:/Users/39349/Documents/Remote Sensing/1. Dati/In-situ data/Lombardia/Pizzighettone")


Pizzighettone_2019<-data.frame(read.table("Pizzighettone_2019.csv", skip=1,sep=","))

Pizzighettone_2020<-data.frame(read.table("Pizzighettone_2020.csv", skip=1,sep=","))

Pizzighettone<- rbind(Pizzighettone_2019,Pizzighettone_2020)
colnames(Pizzighettone)<-c("ID","yyyy.mm.dd.hh.mm","Livelli")


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


