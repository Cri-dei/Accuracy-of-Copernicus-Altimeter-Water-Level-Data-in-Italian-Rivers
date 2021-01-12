##########################################################################################################
########################## CODE for PLOT INSITU-SENTINEL DATA SERIE#########################################
############################################################################################################
#############Author: Cristina Deidda #######################################################################
##########################################################################################################


#Delete everything before
rm(list = ls())


library(gridExtra)
library("readxl")
library(tidyr)
library(ggpubr)
library(reshape)
library("dplyr")
library("gmt")
library(plotly)
library(ggpubr)
library(grid)
library(Metrics)
library("tdr")
library("data.table")
library(viridis)
library(lattice)
library(ggplot2)
library(hrbrthemes)
library(GGally)
library(tidyverse)
library(sf)
library(maps)       # Provides functions that let us plot the maps
library(mapdata)
library(mapproj)
library(ggplot2)
library(cowplot)

#Load in situ data


 load("C:/Users/39349/Documents/Remote Sensing/1. Dati/In-situ data/Lombardia/HYDRO_data_Lombardia.RData")
 load("C:/Users/39349/Documents/Remote Sensing/1. Dati/In-situ data/Po/SENT_HYDRO_data_PO.RData")
 load("C:/Users/39349/Documents/Remote Sensing/1. Dati/In-situ data/Toscana/HYDRO_data_TOSCANA.RData")
 load("C:/Users/39349/Documents/Remote Sensing/1. Dati/In-situ data/Umbria/SENT_HYDRO_data_UMBRIA.RData")
 load("C:/Users/39349/Documents/Remote Sensing/1. Dati/In-situ data/Umbria/SENT_HYDRO_data_UMBRIA_v2.RData")
 load("C:/Users/39349/Documents/Remote Sensing/1. Dati/In-situ data/Veneto/HYDRO_data_VENETO_managed.RData")
 load("C:/Users/39349/Documents/Remote Sensing/1. Dati/In-situ data/Lazio/HYDRO_data_LAZIO_v2.RData")
 
 load("C:/Users/39349/Documents/Remote Sensing/1. Dati/Info data/Merged_STAZIONI_ALL_INFO.RData")
 load("C:/Users/39349/Documents/Remote Sensing/3. Results/SENT_HYDRO_data_ALL_v2.RData")
 
 #FIX HYDRO DATA WITH LIST
 
 HYDRO_data_TOSCANA_managed_ok<- lapply(1:length(HYDRO_data_TOSCANA_managed),function(x){ rbindlist(lapply(HYDRO_data_TOSCANA_managed[[x]],as.data.frame))})
 hh<-rbindlist(lapply(HYDRO_data_TOSCANA_managed[[1]],as.data.frame))
 
 names(HYDRO_data_TOSCANA_managed_ok)<-names(HYDRO_data_TOSCANA_managed)
 
 #Fix name toscana
 name_t<-c("Data_Ora","Livelli","Data_format","Hour_format" )
 name_t2<-c("Data_Ora","Livelli","NV","Data_format","Hour_format" )
 
 HYDRO_data_TOSCANA_final<-lapply(1:length(HYDRO_data_TOSCANA_managed_ok),function(x){
   
   if(length(HYDRO_data_TOSCANA_managed_ok[[x]])==4)
   {setNames(HYDRO_data_TOSCANA_managed_ok[[x]],name_t)}
   else{setNames(HYDRO_data_TOSCANA_managed_ok[[x]],name_t2)}
   })
 
 names(HYDRO_data_TOSCANA_final)<-names(HYDRO_data_TOSCANA_managed_ok)
 
 HYDRO_data_PO_final<-lapply(1:length(HYDRO_data_PO),function(x){setNames(HYDRO_data_PO[[x]],c("Stazione","Data_format","Hour_format","Livelli"))})
 
 names(HYDRO_data_PO_final)<-names(HYDRO_data_PO)
 
 colnames(Pizzighettone_managed)<-c("ID", "Data_Ora" ,"Livelli","Data_format","Hour_format" ) 
 colnames(Ponte_Nuovo_Umbria)<-c("Data", "Ora","Livelli", "Portate","Pioggia", "Temperature","Hour_format","Data_format" ) 
 
 
 #Bind all
 
 HYDRO_data_ALL<- append(HYDRO_data_PO_final,HYDRO_data_TOSCANA_final)
 HYDRO_data_ALL<- append(HYDRO_data_ALL,HYDRO_data_VENETO_managed)
 HYDRO_data_ALL<- append(HYDRO_data_ALL,HYDRO_data_LAZIO_managed)
 
 HYDRO_data_ALL$Ponte_Nuovo<-Ponte_Nuovo_Umbria
 HYDRO_data_ALL$Pizzighettone<-Pizzighettone_managed
 
 ##
 Bad_stations<- c("Adige.a.Verona","Berrettino","CassaPiaggioniInvaso","Montelupo","PonteaSigna", "Borgoforte")
 pos_bst<-na.omit(match(Bad_stations,names(SENT_HYDRO_data_ALL)))
 
#Aggiungere linea per togliere a sent hydro data all le posizioni
 
 pp<- na.omit(match(names(SENT_HYDRO_data_ALL),names(HYDRO_data_ALL)))
 
 FINAL_Hydro<-HYDRO_data_ALL[pp]
 
 FINAL_Hydro$Adige.a.Badia.Polesine.1<-HYDRO_data_ALL$Adige.a.Badia.Polesine
 FINAL_Hydro$Adige.a.Badia.Polesine.2<-HYDRO_data_ALL$Adige.a.Badia.Polesine
 
 FINAL_Hydro$Po.a.Ficarolo.1<-HYDRO_data_ALL$Po.a.Ficarolo
 FINAL_Hydro$Po.a.Ficarolo.2<-HYDRO_data_ALL$Po.a.Ficarolo
 
 FINAL_Hydro$PONTELAG.2<-HYDRO_data_ALL$PONTELAG
 
 ###########
 
 HYDRO_data_ALL2<- FINAL_Hydro
 
 ## Uniform data format ######
 
 for (ii in 1:length(HYDRO_data_ALL2))
 { 
   if(any(names(HYDRO_data_ALL2[[ii]])=="Data_Ora"))
   {
     HYDRO_data_ALL2[[ii]]$DATAORA<-as.POSIXct(HYDRO_data_ALL2[[ii]]$Data_Ora, format="%Y-%m-%d %H:%M")
     print(names(HYDRO_data_ALL2[ii]))
   }else
   {
     HYDRO_data_ALL2[[ii]]$DATAORA<-as.POSIXct(paste(HYDRO_data_ALL2[[ii]]$Data_format, HYDRO_data_ALL2[[ii]]$Hour_format/100), format="%Y%m%d %H")
   }
  
   } 
 
 HYDRO_data_ALL2$Pizzighettone$DATAORA<-as.POSIXct(HYDRO_data_ALL2$Pizzighettone$Data_Ora, format="%Y/%m/%d %H:%M")
 
 
 ###### SENT data format #########
 
 for (ii in 1:length(SENT_HYDRO_data_ALL))
 {
         SENT_HYDRO_data_ALL[[ii]]$SENT_DATAORA<-as.POSIXct(SENT_HYDRO_data_ALL[[ii]]$Data_Time_CET, format="%d-%m-%Y-%H:%M")
         
 }

 
########Put the same starting point ##########à
 
 
 for (ii in 1:length(SENT_HYDRO_data_ALL))
 {      
         if (ii<6){ 
                 Diff<- SENT_HYDRO_data_ALL[[ii]]$Water.level[1]- SENT_HYDRO_data_ALL[[ii]]$H.cm[1]
                SENT_HYDRO_data_ALL[[ii]]$SENT_refined<-SENT_HYDRO_data_ALL[[ii]]$Water.level-Diff
                 
         }
                 
                 
         if (ii>=6 && ii<=17){ 
                 Diff<- SENT_HYDRO_data_ALL[[ii]]$Water.level[1]- SENT_HYDRO_data_ALL[[ii]]$Livelli[1]
                SENT_HYDRO_data_ALL[[ii]]$SENT_refined<-SENT_HYDRO_data_ALL[[ii]]$Water.level-Diff
         }
         
         if (ii>17 && ii<=26 ){ 
                 Diff<- SENT_HYDRO_data_ALL[[ii]]$Water.level[1]- SENT_HYDRO_data_ALL[[ii]]$valore[1]
                 SENT_HYDRO_data_ALL[[ii]]$SENT_refined<-SENT_HYDRO_data_ALL[[ii]]$Water.level-Diff}
        
        if (ii==c(27)){ 
                         Diff<- SENT_HYDRO_data_ALL[[ii]]$'Water level'[1]- SENT_HYDRO_data_ALL[[ii]]$Livelli[1]
                         SENT_HYDRO_data_ALL[[ii]]$SENT_refined<-SENT_HYDRO_data_ALL[[ii]]$'Water level'-Diff
        }   
         if (ii==c(28)){ 
                 Diff<- SENT_HYDRO_data_ALL[[ii]]$Water.level[1]- SENT_HYDRO_data_ALL[[ii]]$Livelli[1]
                 SENT_HYDRO_data_ALL[[ii]]$SENT_refined<-SENT_HYDRO_data_ALL[[ii]]$Water.level-Diff
         }   
 }
 
 ## Final dataset ###
 
 save(SENT_HYDRO_data_ALL,file="SENT_HYDRO_data_ALL.RData")
 save(HYDRO_data_ALL2,file="HYDRO_data_ALL2.RData")
 
 ###CHANGE THE NAME
 #KK<-HYDRO_data_ALL2
 
 #New_name<-c("Stazione","Data_format","ora","H cm","DATAORA") 

# HYDRO_data_ALL2[[28]]<-setNames(HYDRO_data_ALL2[[28]],New_name)


# li_2 <- lapply(1:4, function(x) setNames(KK[[x]], New_name) )
 #names(li_2)<-names(HYDRO_data_ALL2[1:4])
 #
# HYDRO_data_ALL2[1:4]<-li_2
 
#New_name2<-c("Data_Ora", "Livelli","Data_format","Hour_format","DATAORA")  
 
 
 #which(namesHYDRO_data_ALL)
 
 ##plot
 setwd("C:/Users/39349/Documents/Remote Sensing/3. Results/Plot-insituSent")
 
 #pdf(file="Insitu-sentcomp.pdf",onefile = TRUE)
 
 HYDRO_data_ALL2$CapoDueRami$Livelli<-as.numeric(as.character(HYDRO_data_ALL2$CapoDueRami$Livelli))
 HYDRO_data_ALL2$Ponte_Felice$Livelli<-as.numeric(as.character(HYDRO_data_ALL2$Ponte_Felice$Livelli))
 HYDRO_data_ALL2$Mezzocammino$Livelli<-as.numeric(as.character(HYDRO_data_ALL2$Mezzocammino$Livelli)) 
 HYDRO_data_ALL2$Fiumara$Livelli<-as.numeric(as.character(HYDRO_data_ALL2$Fiumara$Livelli)) 
 
 

 
 
 
 for (xx in 1:length(SENT_HYDRO_data_ALL))
 {  
   pos<-which(names(HYDRO_data_ALL2)==names(SENT_HYDRO_data_ALL[xx]))
    Y_start<-which(HYDRO_data_ALL2[[pos]]$Data_format==as.numeric(as.character(SENT_HYDRO_data_ALL[[xx]]$Data_format[1])))[1] 
    Y_start<- Y_start-20
 
 png(paste(names(HYDRO_data_ALL2[pos]),".png"),width = 10.33, height = 6.29, units = "in",   res =400)
 
 #par(mfrow=c(2,1))
 
 plot( HYDRO_data_ALL2[[pos]]$DATAORA[Y_start:nrow(HYDRO_data_ALL2[[pos]])],HYDRO_data_ALL2[[pos]]$Livelli[Y_start:nrow(HYDRO_data_ALL2[[pos]])], 
       xlab="Date", ylab="Level [m]",type="l", main=paste(names(HYDRO_data_ALL2[pos]),"-",names(SENT_HYDRO_data_ALL[xx])))
 
 points(SENT_HYDRO_data_ALL[[xx]]$SENT_DATAORA, SENT_HYDRO_data_ALL[[xx]]$SENT_refined, col="red", pch=20)
 legend("topright", legend=c("Satellite","In situ"), col=c("red","black"),pch=20)   
 
 dev.off()
 
 }
 
 #PLOt PIZZIGHETTONE: LIM ,ylim=c(-1.5,2)
 
 xx<-which(names(SENT_HYDRO_data_ALL)=="Pizzighettone")
 pos<-which(names(HYDRO_data_ALL2)==names(SENT_HYDRO_data_ALL[xx]))
 Y_start<-which(HYDRO_data_ALL2[[pos]]$Data_format==as.numeric(as.character(SENT_HYDRO_data_ALL[[xx]]$Data_format[1])))[1] 
 Y_start<- Y_start-20
 
 miny<-min(HYDRO_data_ALL2[[pos]]$Livelli)
 png(paste(names(HYDRO_data_ALL2[pos]),".png"),width = 10.33, height = 6.29, units = "in",   res =400)
 
 
 plot( HYDRO_data_ALL2[[pos]]$DATAORA[Y_start:nrow(HYDRO_data_ALL2[[pos]])],HYDRO_data_ALL2[[pos]]$Livelli[Y_start:nrow(HYDRO_data_ALL2[[pos]])], 
       xlab="Date", ylab="Level [m]",type="l",ylim=c(-1.5,2), main=paste(names(HYDRO_data_ALL2[pos]),"-",names(SENT_HYDRO_data_ALL[xx])))
 
 points(SENT_HYDRO_data_ALL[[xx]]$SENT_DATAORA, SENT_HYDRO_data_ALL[[xx]]$SENT_refined, col="red", pch=20)
 legend("topright", legend=c("Satellite","In situ"), col=c("red","black"),pch=20)   
 
 dev.off()
 
 #PLOT SAN GIOVANNI A VALLE
 
 xx<-which(names(SENT_HYDRO_data_ALL)=="SGiovanniallaVenaaValle")
 pos<-which(names(HYDRO_data_ALL2)==names(SENT_HYDRO_data_ALL[xx]))
 Y_start<-which(HYDRO_data_ALL2[[pos]]$Data_format==as.numeric(as.character(SENT_HYDRO_data_ALL[[xx]]$Data_format[1])))[1] 
 Y_start<- Y_start-20
 
 miny<-min(HYDRO_data_ALL2[[pos]]$Livelli)
 png(paste(names(HYDRO_data_ALL2[pos]),".png"),width = 10.33, height = 6.29, units = "in",   res =400)
 
 
 plot( HYDRO_data_ALL2[[pos]]$DATAORA[Y_start:nrow(HYDRO_data_ALL2[[pos]])],HYDRO_data_ALL2[[pos]]$Livelli[Y_start:nrow(HYDRO_data_ALL2[[pos]])], 
       xlab="Date", ylab="Level [m]",type="l", main=paste(names(HYDRO_data_ALL2[pos]),"-",names(SENT_HYDRO_data_ALL[xx])), ylim=c(-5,10))
 
 points(SENT_HYDRO_data_ALL[[xx]]$SENT_DATAORA, SENT_HYDRO_data_ALL[[xx]]$SENT_refined, col="red", pch=20)
 legend("topright", legend=c("Satellite","In situ"), col=c("red","black"),pch=20)   
 
 dev.off()
 
 #PONTE NUOVO
 
 xx<-which(names(SENT_HYDRO_data_ALL)=="Ponte_Nuovo")
 pos<-which(names(HYDRO_data_ALL2)==names(SENT_HYDRO_data_ALL[xx]))
 Y_start<-which(HYDRO_data_ALL2[[pos]]$Data_format==as.numeric(as.character(SENT_HYDRO_data_ALL[[xx]]$Data_format[1])))[1] 
 Y_start<- Y_start-20
 
 miny<-min(HYDRO_data_ALL2[[pos]]$Livelli)
 png(paste(names(HYDRO_data_ALL2[pos]),".png"),width = 10.33, height = 6.29, units = "in",   res =400)
 
 
 plot( HYDRO_data_ALL2[[pos]]$DATAORA[Y_start:nrow(HYDRO_data_ALL2[[pos]])],HYDRO_data_ALL2[[pos]]$Livelli[Y_start:nrow(HYDRO_data_ALL2[[pos]])], 
       xlab="Date", ylab="Level [m]",type="l", main=paste(names(HYDRO_data_ALL2[pos]),"-",names(SENT_HYDRO_data_ALL[xx])), ylim=c(-5,10))
 
 points(SENT_HYDRO_data_ALL[[xx]]$SENT_DATAORA, SENT_HYDRO_data_ALL[[xx]]$SENT_refined, col="red", pch=20)
 legend("topright", legend=c("Satellite","In situ"), col=c("red","black"),pch=20)   
 
 dev.off()
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 for (xx in 1:length(SENT_HYDRO_data_ALL))
 {      pos<-which(names(HYDRO_data_ALL2)==names(SENT_HYDRO_data_ALL[xx]))
 
 
       if(is.list(HYDRO_data_ALL2[[pos]][[1]])=="FALSE")
       {
        Y_start<-which(HYDRO_data_ALL2[[pos]]$Data_format==as.numeric(as.character(SENT_HYDRO_data_ALL[[xx]]$Data_format[1])))[1] 
        Y_start<- Y_start-20
        
        png(paste(names(HYDRO_data_ALL2[pos]),".png"),width = 10.33, height = 6.29, units = "in",   res =400)
        
        if(any(names(HYDRO_data_ALL2[[pos]])== "Livelli"))
        {
                plot( HYDRO_data_ALL2[[pos]]$DATAORA[Y_start:nrow(HYDRO_data_ALL2[[pos]])],HYDRO_data_ALL2[[pos]]$Livelli[Y_start:nrow(HYDRO_data_ALL2[[pos]])], 
                      xlab="Date", ylab="Level [m]",type="l", main=paste(names(HYDRO_data_ALL2[pos]),"-",names(SENT_HYDRO_data_ALL[xx])))
                
                points(SENT_HYDRO_data_ALL[[xx]]$SENT_DATAORA, SENT_HYDRO_data_ALL[[xx]]$SENT_refined, col="red", pch=20)
                legend("topright", legend=c("Satellite","In situ"), col=c("red","black"),pch=20)   
                dev.off()
                
                
        }
        else if(any(names(HYDRO_data_ALL2[[pos]])== "valore"))
        {
          plot( HYDRO_data_ALL2[[pos]]$DATAORA[Y_start:nrow(HYDRO_data_ALL2[[pos]])],HYDRO_data_ALL2[[pos]]$valore[Y_start:nrow(HYDRO_data_ALL2[[pos]])], 
                xlab="Date", ylab="Level [m]",type="l", main=paste(names(HYDRO_data_ALL2[pos]),"-",names(SENT_HYDRO_data_ALL[xx])))
          
          points(SENT_HYDRO_data_ALL[[xx]]$SENT_DATAORA, SENT_HYDRO_data_ALL[[xx]]$SENT_refined, col="red", pch=20)
          legend("topright", legend=c("Satellite","In situ"), col=c("red","black"),pch=20)   
          dev.off()
       }       
        
        
        
        
        else{
                plot( HYDRO_data_ALL2[[pos]]$DATAORA[Y_start:nrow(HYDRO_data_ALL2[[pos]])],HYDRO_data_ALL2[[pos]]$`H cm`[Y_start:nrow(HYDRO_data_ALL2[[pos]])],
                      xlab="Date", ylab="Level [m]", type="l",main=paste(names(HYDRO_data_ALL2[pos]),"-",names(SENT_HYDRO_data_ALL[xx])))
                points(SENT_HYDRO_data_ALL[[xx]]$SENT_DATAORA, SENT_HYDRO_data_ALL[[xx]]$SENT_refined, col="red", pch=20)
                legend("topright", legend=c("Satellite","In situ"), col=c("red","black"),pch=20)   
                dev.off()
                
                } }
 } 
 
 for(pos in 22:27)
 {
   xx<-which(names(HYDRO_data_ALL2[pos])==names(SENT_HYDRO_data_ALL))
   Y_start<-which(HYDRO_data_ALL2[[pos]]$Data_Ora==as.numeric(as.character(SENT_HYDRO_data_ALL[[1]]$Data_format[1])))[1] 
   Y_start<- Y_start-20
   
   png(paste(names(HYDRO_data_ALL2[pos]),".png"),width = 10.33, height = 6.29, units = "in",   res =400)
   
   plot( HYDRO_data_ALL2[[pos]]$Data_Ora[Y_start:nrow(HYDRO_data_ALL2[[pos]])],HYDRO_data_ALL2[[pos]]$Livelli[Y_start:nrow(HYDRO_data_ALL2[[pos]])], 
         xlab="Date", ylab="Level [m]",type="l", main=paste(names(HYDRO_data_ALL2[pos]),"-",names(SENT_HYDRO_data_ALL[xx])))
   
   points(SENT_HYDRO_data_ALL[[xx]]$SENT_DATAORA, SENT_HYDRO_data_ALL[[xx]]$SENT_refined, col="red", pch=20)
   legend("topright", legend=c("Satellite","In situ"), col=c("red","black"),pch=20)   
   dev.off()
   
   
 }
 
 ##########
 load("C:/Users/39349/Documents/Remote Sensing/3. Results/Summary_results.RData")
 load("C:/Users/39349/Documents/Remote Sensing/3. Results/Processed_Summary.RData")
 
 ######plot 3 più bassi
 
 Station_worst<-c("Pizzighettone","Adige.a.Cavarzere","Adige.a.Badia.Polesine.1","Adige.a.Badia.Polesine.2")
 
 Station_better<-c("Boretto","Pontelagoscuro","Ficarolo.1","Ficarolo.2")
 
 ####
 
 
 
#dev.off()
 
 #pt.cex=20, pch=15, cex=1.2)  
 
 plot(SENT_HYDRO_data_ALL$BORETTO$SENT_DATAORA, SENT_HYDRO_data_ALL$BORETTO$SENT_refined, col="red", pch=20)
         
         
         
         
 
 
 jg<-SENT_HYDRO_data_ALL[[1]]
 jg$Water.level[1]- jg$H.cm[1]
 
 
 
 
 
 
 
 
                 
 for (jj in 1:length(SENT_HYDRO_data_ALL))
 {
         SENT_HYDRO_data_ALL[[ii]]$DATAORA<-as.POSIXct(paste(SENT_HYDRO_data_ALL[[ii]]$Data_format, SENT_HYDRO_data_ALL[[ii]]$ora/100), format="%Y%m%d %H")
 }
 
 
 
 
 for (ii in c(1,2,3,4,20:25))
 { 
         xx<-dim(HYDRO_data_ALL2[[ii]])[2]
         if (length(xx)>0){
         HYDRO_data_ALL2[[ii]]$DATAORA<-as.POSIXct(paste(HYDRO_data_ALL2[[ii]]$data, HYDRO_data_ALL2[[ii]]$ora/100), format="%Y%m%d %H")}
         else{
                 for (jj in 1: length(HYDRO_data_ALL2[[ii]]))
                 {
                         HYDRO_data_ALL2[[ii]][[jj]]$DATAORA<-as.POSIXct(paste(HYDRO_data_ALL2[[ii]][[jj]]$Data_format, HYDRO_data_ALL2[[ii]][[jj]]$Hour_format/100), format="%Y%m%d %H")
                         
                 }
                 
                 
                 
         }
   }
 
 
 
 lapply(1:length(HYDRO_data_ALL),function(x){ HH<- as.POSIXct(paste(HYDRO_data_ALL[[x]]$data, HYDRO_data_ALL[[x]]$ora/100), format="%Y%m%d %H")
                                                                HYDRO_data_ALL[[x]]$DATA<- HH})

 
 
 
 
 
 H1<- as.POSIXct(SENT_HYDRO_data_ALL$BORETTO$Data_Time_CET, format="%d-%m-%Y-%H:%M")

 
 
 #points(H1,SENT_HYDRO_data_ALL$BORETTO$`H cm`,col="red")
 plot(HH,BOR$`H cm`)
 points(H1,SENT_HYDRO_data_ALL$BORETTO$H.cm)
 
 
 merge(Sentinel3_data_managed[[Pos_sent]],HYDRO_data_VENETO_managed[[Pos_stat]],
       by.x = c("Data_format","Hour_format_30min"), by.y = c( "Data_format","Hour_format"))
 
 
 plot(HYDRO_data_ALL2[[1]] , HYDRO_data_ALL$BORETTO$`H cm`)
 
 
 
 length(FINAL_Hydro)
 length(SENT_HYDRO_data_ALL)
 
 names(FINAL_Hydro)== names(SENT_HYDRO_data_ALL)
 
 sent_hydro<-names(SENT_HYDRO_data_ALL)

hydro<-names(HYDRO_data_ALL)

which(sent_hydro %in% hydro)

sent_hydro[-which(sent_hydro %in% hydro)]
 
join(SENT_HYDRO_data_ALL$B)
 
 names(HYDRO_data_ALL)[pp]