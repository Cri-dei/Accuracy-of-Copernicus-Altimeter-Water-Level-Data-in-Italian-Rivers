#########################################
####### Code in situ comparison #########
#########################################
####  Author: Cristina Deidda    ########
#########################################
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

####################### LOAD IN SITU PROCESSED DATA: SENT-HYDRO  ###########################

Dir1<-c("C:/Users/39349/Documents/Remote Sensing/1. Dati/")

load(paste0(Dir1,"In-situ data/Po/SENT_HYDRO_data_PO.RData"))
load(paste0(Dir1,"In-situ data/Umbria/SENT_HYDRO_data_UMBRIA.RData"))
load(paste0(Dir1,"In-situ data/Lazio/HYDRO_data_LAZIO.RData"))
load(paste0(Dir1,"In-situ data/Veneto/SENT_HYDRO_data_VENETO.RData"))
load(paste0(Dir1,"In-situ data/Toscana/SENT_HYDRO_data_TOSCANA.RData"))

setwd("C:/Users/39349/Documents/Remote Sensing/1. Dati/Info data")
load("Merged_STAZIONI_ALL_INFO.RData")


############################## MERGE ALL LIST ###############################################

SENT_HYDRO_data_ALL<-append( SENT_HYDRO_data_PO, SENT_HYDRO_data_LAZIO)

SENT_HYDRO_data_ALL<-append(SENT_HYDRO_data_ALL , SENT_HYDRO_data_VENETO)

SENT_HYDRO_data_ALL<-append(SENT_HYDRO_data_ALL , SENT_HYDRO_data_TOSCANA)

SENT_HYDRO_data_ALL$Ponte_Nuovo<-SENT_HYDRO_data_UMBRIA


# Save data
setwd("C:/Users/39349/Documents/Remote Sensing/3. Results")
save(SENT_HYDRO_data_ALL, file="SENT_HYDRO_data_ALL.RData")


######################################Summary ###############################################

rsq2 <- function (x, y) cor(x, y, use="complete.obs") ^ 2


Summary_results<-data.frame(matrix(,length(SENT_HYDRO_data_ALL),8))
colnames(Summary_results)<-c("Station","Correlation","Rsquared","Section","Distance","Region","Identifier","Satellite")

for ( jj in 1:length(SENT_HYDRO_data_ALL))
{
  Summary_results$Station[[jj]]<- names(SENT_HYDRO_data_ALL[jj])
  Summary_results$Correlation[[jj]]<-cor(SENT_HYDRO_data_ALL[[jj]]$Hydro_diff, SENT_HYDRO_data_ALL[[jj]]$Sentinel_diff, use="complete.obs")
  Summary_results$Rsquared[[jj]]<-rsq2(SENT_HYDRO_data_ALL[[jj]]$Hydro_diff, SENT_HYDRO_data_ALL[[jj]]$Sentinel_diff)
  gh_pos<-which(Merged_STAZIONI_ALL_INFO$NomeStazione2==names(SENT_HYDRO_data_ALL[jj]))
  Summary_results$Section[[jj]]<-  Merged_STAZIONI_ALL_INFO$`Larghezza.Alveo..m.`[gh_pos[1]]
  Summary_results$Region[[jj]]<-  Merged_STAZIONI_ALL_INFO$Regione[gh_pos[1]]
  Summary_results$Distance[[jj]]<-  Merged_STAZIONI_ALL_INFO$Distance[gh_pos[1]]
  Summary_results$Identifier[[jj]]<-  Merged_STAZIONI_ALL_INFO$SENTINEL[gh_pos[1]]
  Summary_results$Satellite[[jj]]<-  as.character(Merged_STAZIONI_ALL_INFO$Satellite[gh_pos[1]])
}

save(Merged_STAZIONI_ALL_INFO, file="Merged_STAZIONI_ALL_INFO.RData")


setwd("C:/Users/39349/Documents/Remote Sensing/2. Plot")

png("Correlation_table.png", height = 50*nrow(Summary_results), width = 100*ncol(Summary_results))
grid.table(Summary_results)
dev.off()

#Statistics
par(mfrow=c(1,2))

hist(Summary_results$Correlation,main="Histogram Correlation")
summary(Summary_results$Correlation)
boxplot(Summary_results$Correlation,main="Boxplot Correlation")

plot(sort(Summary_results$Correlation))

####################### Plot ##############################################

plot(Summary_results$Section,Summary_results$Correlation, xlab="Section [km]", ylab= "Pearson Correlation")
plot(Summary_results$Distance,Summary_results$Correlation, xlab="Distance [km]", ylab= "Pearson Correlation")

#Save plot

#Pearson-Section

png(filename = "Correlation and Section.png",
    width = 10.33, height = 6.29, units = "in",   res =400)
plot(Summary_results$Section,Summary_results$Correlation, xlab="Section [km]", ylab= "Pearson Correlation", main="Correlation - section")
dev.off()

#Pearson-Distance
png(filename = "Correlation and Distance.png",
    width = 10.33, height = 6.29, units = "in",   res =400)
plot(Summary_results$Distance,Summary_results$Correlation, xlab="Distance [km]", ylab= "Pearson Correlation", main="Correlation - distance")
dev.off()

#GROUP plot

ggplot(Summary_results, aes(x = Section, y = Correlation, col = Satellite,shape=Satellite)) +
  geom_point(size=4)
ggsave("Plot_group_sect.jpeg", units="in", dpi=400, width=8,height=6.5)

ggplot(Summary_results, aes(x = Section, y = Rsquared, col = Satellite,shape=Satellite)) +
  geom_point(size=4)
ggsave("Plot_group_Rsquaresect.jpeg", units="in", dpi=400, width=8,height=6.5)


ggplot(Summary_results, aes(x = Distance, y = Correlation, col = Satellite,shape=Satellite)) +
  geom_point(size=4)
ggsave("Plot_group_dist.jpeg", units="in", dpi=400, width=8,height=6.5)

################################## PLOT PER ALLL ########################################

setwd("C:/Users/39349/Documents/Remote Sensing/2. Plot")

pdf("Sentinel_results_18112020.pdf",onefile = TRUE)

plot(Summary_results$Section,Summary_results$Correlation, xlab="Section [km]", ylab= "Pearson Correlation", main="Correlation and section")
plot(Summary_results$Distance,Summary_results$Correlation, xlab="Distance [km]", ylab= "Pearson Correlation", main="Correlation and distance between couples")


for ( jj in 1:length(SENT_HYDRO_data_ALL))
{ 
  
  
  data_ALL<- data.frame(SENT_HYDRO_data_ALL[[jj]]$Hydro_diff, SENT_HYDRO_data_ALL[[jj]]$Sentinel_diff, SENT_HYDRO_data_ALL[[jj]]$Data_format)
  colnames(data_ALL)<-c("Hydro","Sent","Data")
  data_ALL<-na.omit(data_ALL)
  
  preds <- data_ALL$Sent
  actual <-  data_ALL$Hydro

  rsq<-round(rsq2(preds,actual),3)
  
  df_long_ALL = gather(na.omit(data_ALL), key = var, value = value, Hydro, Sent)
  p1_ALL<-ggplot(df_long_ALL, aes(x = Data, y = value, fill = var)) +
    geom_bar(stat = 'identity', position = 'dodge')
  
  
  p1_ALL<-p1_ALL +theme(axis.text.x = element_text(face="bold", color="#993333",  size=6, angle=45))+
            labs(title=paste(Summary_results$Region[jj], "- Stazione:", names(SENT_HYDRO_data_ALL)[[jj]]), 
            subtitle=paste("Section:",Summary_results$Section[jj],"km" ,
                           "Distance:",round(Summary_results$Distance[jj],2),"km",
                           "Identifier:",Summary_results$Identifier[jj],
                           SENT_HYDRO_data_ALL[[jj]]$Identifier,
                           "Satellite:",Summary_results$Satellite[jj]
                           ))+
                            xlab("Date(YMD)") + ylab("Level difference [m]")+
            theme(plot.title = element_text(hjust = 0.5, size=16, face="bold",lineheight=1),
                  plot.subtitle = element_text(size = 11,lineheight=1), axis.title.x = element_text( size=13),
                  axis.title.y = element_text(size=13))
  
  
  p2_ALL<- ggplot(na.omit(data_ALL) , aes(x=Hydro, y=Sent)) + geom_point()+
    xlim(min(na.omit(data_ALL[,1:2])-0.5),max(na.omit(data_ALL[,1:2]))+0.5) +
    ylim(min(na.omit(data_ALL[,1:2])-0.5),max(na.omit(data_ALL[,1:2]))+0.5) +
    theme(axis.text = element_text(size = 13),axis.title = element_text(size = 15))+
    xlab("Hydrometer level diff [m]") + ylab("Sentinel 3A level diff [m]") + theme(panel.grid.major = element_blank(),
                                                                                   plot.margin = margin(1.2, 1.2, 1.2, 1.2, "cm"),
                                                                                   panel.grid.minor = element_blank(), 
                                                                                   panel.background = element_blank(), axis.line = element_line(colour = "black"))+
                                                                                    geom_abline(slope=1, intercept = 0) 
  
  grob <- grobTree(textGrob(paste0('R2=',rsq), x=0.1,  y=0.95, hjust=0,
                            gp=gpar(col="red", fontsize=13, fontface="italic")))
  
  
  p2_ALL<-p2_ALL + annotation_custom(grob)
  
  #geom_smooth(method = "lm", se = FALSE)+#stat_regline_equation(label.x = -2, label.y = 2)
  
  P_ALL<-ggarrange(p1_ALL,p2_ALL, ncol = 1, nrow = 2) 
  print(P_ALL)
  
}


dev.off()



###########################CHECK##############################################

Bad_stations<-which(Summary_results$Correlation<0.50)
Good_stations<-which(Summary_results$Correlation>=0.50)

CHOOSE<-"Good"
#CHOOSE<-"Bad"


if(CHOOSE=="Good")
{
  Stat_select<-Good_stations
  namefile<-"Good_stations.pdf"
}
 if(CHOOSE=="Bad")
{
   Stat_select<-Bad_stations
  namefile<-"Bad_stations.pdf"
}


pdf(namefile,onefile = TRUE)

for ( jj in Stat_select)
{ 
  
data_ALL<- data.frame(SENT_HYDRO_data_ALL[[jj]]$Hydro_diff, SENT_HYDRO_data_ALL[[jj]]$Sentinel_diff, SENT_HYDRO_data_ALL[[jj]]$Data_format)
colnames(data_ALL)<-c("Hydro","Sent","Data")
data_ALL<-na.omit(data_ALL)

preds <- data_ALL$Sent
actual <-  data_ALL$Hydro

rsq<-round(rsq2(preds,actual),3)


df_long_ALL = gather(na.omit(data_ALL), key = var, value = value, Hydro, Sent)
p1_ALL<-ggplot(df_long_ALL, aes(x = Data, y = value, fill = var)) +
  geom_bar(stat = 'identity', position = 'dodge')

#X11()
p1_ALL<-p1_ALL +theme(axis.text.x = element_text(face="bold", color="#993333",  size=6, angle=45))+
  labs(title=paste(Summary_results$Region[jj], "- Stazione:", names(SENT_HYDRO_data_ALL)[[jj]]), 
       subtitle=paste("Section:",Summary_results$Section[jj],"km" ,
                      "Distance:",round(Summary_results$Distance[jj],2),"km",
                      "Identifier:",Summary_results$Identifier[jj],
                      SENT_HYDRO_data_ALL[[jj]]$Identifier,
                      "Satellite:",Summary_results$Satellite[jj]
       ))+
    xlab("Date(YMD)") + ylab("Level difference [m]")+
  theme(plot.title = element_text(hjust = 0.5, size=16, face="bold",lineheight=1),
        plot.subtitle = element_text(size = 11,lineheight=1), axis.title.x = element_text( size=13),
        axis.title.y = element_text(size=13))


p2_ALL<- ggplot(na.omit(data_ALL) , aes(x=Hydro, y=Sent)) + geom_point()+
  xlim(min(na.omit(data_ALL[,1:2])-0.5),max(na.omit(data_ALL[,1:2]))+0.5) +
  ylim(min(na.omit(data_ALL[,1:2])-0.5),max(na.omit(data_ALL[,1:2]))+0.5) +
  theme(axis.text = element_text(size = 13),axis.title = element_text(size = 15))+
  xlab("Hydrometer level diff [m]") + ylab("Sentinel 3A level diff [m]") + theme(panel.grid.major = element_blank(),
                                                                                 plot.margin = margin(1.2, 1.2, 1.2, 1.2, "cm"),
                                                                                 panel.grid.minor = element_blank(),
                                                                                 panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  geom_abline(slope=1, intercept = 0)

grob <- grobTree(textGrob(paste0('R2=',rsq), x=0.1,  y=0.95, hjust=0,
                          gp=gpar(col="red", fontsize=13, fontface="italic")))


#JUST P1_ALL
# p2_ALL<-p1_ALL + annotation_custom(grob)
# P_ALL<-ggarrange(p2_ALL, ncol = 1, nrow = 1) 

#BOTH P1_ALL E P2ALL

p2_ALL<-p2_ALL + annotation_custom(grob)
P_ALL<-ggarrange(p1_ALL,p2_ALL, ncol = 1, nrow = 2) 


print(P_ALL)


}

dev.off()

#############################################################################




















