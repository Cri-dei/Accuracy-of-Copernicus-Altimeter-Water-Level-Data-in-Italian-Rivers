

#Load in situ data


 load("C:/Users/39349/Documents/Remote Sensing/1. Dati/In-situ data/Lombardia/HYDRO_data_Lombardia.RData")
 load("C:/Users/39349/Documents/Remote Sensing/1. Dati/In-situ data/Po/SENT_HYDRO_data_PO.RData")
 load("C:/Users/39349/Documents/Remote Sensing/1. Dati/In-situ data/Toscana/HYDRO_data_TOSCANA.RData")
 load("C:/Users/39349/Documents/Remote Sensing/1. Dati/In-situ data/Umbria/SENT_HYDRO_data_UMBRIA.RData")
 load("C:/Users/39349/Documents/Remote Sensing/1. Dati/In-situ data/Umbria/SENT_HYDRO_data_UMBRIA_v2.RData")
 load("C:/Users/39349/Documents/Remote Sensing/1. Dati/In-situ data/Veneto/HYDRO_data_VENETO_managed.RData")
 load("C:/Users/39349/Documents/Remote Sensing/1. Dati/In-situ data/Lazio/HYDRO_data_LAZIO_v2.RData")
 
 
 HYDRO_data_ALL<- append(HYDRO_data_PO,HYDRO_data_TOSCANA_managed)
 HYDRO_data_ALL<- append(HYDRO_data_ALL,HYDRO_data_VENETO_managed)
 HYDRO_data_ALL<- append(HYDRO_data_ALL,HYDRO_data_LAZIO_managed)
 
 HYDRO_data_ALL$Ponte_Nuovo<-Ponte_Nuovo_Umbria
 HYDRO_data_ALL$Pizzighettone<-Pizzighettone_managed
 
 pp<- na.omit(match(names(SENT_HYDRO_data_ALL),names(HYDRO_data_ALL)))
 
 FINAL_Hydro<-HYDRO_data_ALL[pp]
 
 FINAL_Hydro$Adige.a.Badia.Polesine.1<-HYDRO_data_ALL$Adige.a.Badia.Polesine
 FINAL_Hydro$Adige.a.Badia.Polesine.2<-HYDRO_data_ALL$Adige.a.Badia.Polesine
 
 FINAL_Hydro$Po.a.Ficarolo.1<-HYDRO_data_ALL$Po.a.Ficarolo
 FINAL_Hydro$Po.a.Ficarolo.2<-HYDRO_data_ALL$Po.a.Ficarolo
 
 FINAL_Hydro$PONTELAG.2<-HYDRO_data_ALL$PONTELAG
 
 
 pg<-FINAL_Hydro$Livenza
 pg$DD<-paste0(pg$Hour_format,substring(pg$Data_format,7,8),substring(pg$Data_format,5,6),substring(pg$Data_format,1,4))
 
 
 HYDRO_data_ALL2<-HYDRO_data_ALL
 HYDRO_data_ALL2<-lapply(1:length(HYDRO_data_ALL2),function(x){ rownames(HYDRO_data_ALL[[x]])})
 
 plot(pg$DD, pg$Livelli)
 
 BOR<- HYDRO_data_ALL$BORETTO
 BOR$DD<-rownames(BOR)
 
 HYDRO_data_ALL2<- FINAL_Hydro
 
 
 for (ii in c(1,2,3,4,20:25))
 { 
       if(ii <20){
                 
                 HYDRO_data_ALL2[[ii]]$DATAORA<-as.POSIXct(paste(HYDRO_data_ALL2[[ii]]$data, HYDRO_data_ALL2[[ii]]$ora/100), format="%Y%m%d %H")}
        else{
                 HYDRO_data_ALL2[[ii]]$DATAORA<-as.POSIXct(paste(HYDRO_data_ALL2[[ii]]$Data_format, HYDRO_data_ALL2[[ii]]$Hour_format/100), format="%Y%m%d %H")}
 }
 
 
 for (ii in 1:length(SENT_HYDRO_data_ALL))
 {
         SENT_HYDRO_data_ALL[[ii]]$SENT_DATAORA<-as.POSIXct(SENT_HYDRO_data_ALL[[ii]]$Data_Time_CET, format="%d-%m-%Y-%H:%M")
         
 }

 
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
 
 for (xx in length(SENT_HYDRO_data_ALL))
 {
        pos<-which(names(HYDRO_data_ALL2)==names(SENT_HYDRO_data_ALL[xx]))
         plot( HYDRO_data_ALL2[[pos]]$DATAORA,HYDRO_data_ALL2[[pos]]$`H cm`, xlab="Date", ylab="Level [m]", main=paste(names(HYDRO_data_ALL[[xx]])))
               points(SENT_HYDRO_data_ALL[[xx]]$SENT_DATAORA, SENT_HYDRO_data_ALL[[xx]]$SENT_refined, col="red", pch=20)
               legend("topright", legend=c("Satellite","In situ"), col=c("black","red"),pch=20)      
        
 } 

 
 
 
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