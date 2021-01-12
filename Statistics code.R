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
library("DescTools")
library("tdr")

##Load SENT HYDRO DATA ALL
setwd("C:/Users/39349/Documents/Remote Sensing/3. Results")
load("SENT_HYDRO_data_ALL_diff_v2.RData")
load("Merged_STAZIONI_ALL_INFO.RData")
load("Summary_results.RData")

#Code stations for which there are yet confluences or lakes
#CODE_Bad_stations<-c(16,20,21,23,24)
Bad_stations<- c("Adige a Verona","Berrettino","Cassa Piaggioni Invaso","Montelupo","Ponte a Signa", "Borgoforte")


  
#Summary_results$Station[which(Summary_results$Station%in%CODE_Bad_stations)]
Bad_stat_pos<- which(Summary_results$Station%in%Bad_stations)
CODE_Bad_stations<-Summary_results$Code[Bad_stat_pos]


Processed_ALL<-SENT_HYDRO_data_ALL[-which(names(SENT_HYDRO_data_ALL)%in%Bad_stations)]
Processed_DIFF<-DIFFERENCES[-which(names(DIFFERENCES)%in%CODE_Bad_stations)]
Processed_Summary<-Summary_results[-Bad_stat_pos,]


### Num final couples

#Sum_reg<-table(Processed_Summary$Region)
Proc_sum_tab<-Processed_Summary[,-c(1,11)]
Proc_sum_tab[,c(4:9)]<-round(Proc_sum_tab[,c(4:9)],2)

Sum_reg<-tableGrob(Proc_sum_tab, rows = NULL)

save(Processed_Summary,Sum_reg,file="Processed_Summary.RData")

png("0.Sum_regions.png", height = 25*nrow(Sum_reg), width = 75*ncol(Sum_reg))
grid.arrange(Sum_reg)
dev.off()

#Simple plot

png(filename = "Processed_station.png",
    width = 10.33, height = 6.29, units = "in",   res =400)
plot(Summary_results$Section,Summary_results$Correlation, xlab="Section [km]", ylab= "Pearson Correlation")
points(Summary_results$Section[Bad_stat_pos],Summary_results$Correlation[Bad_stat_pos], col="red",pch = 17)
dev.off()

plot(Processed_Summary$Distance,Processed_Summary$Correlation, xlab="Distance [km]", ylab= "Pearson Correlation")


ggplot(Processed_Summary, aes(x = Section, y = Correlation, col = Satellite,shape=Satellite)) +
  geom_point(size=4)+xlab("River section [m]")+ ylab("Pearson correlation")+
  theme_classic()
ggsave("02.Correlation-section.jpeg", units="in", dpi=400, width=8,height=6.5)

ggplot(Processed_Summary, aes(x = Section, y = Rsquared, col = Satellite,shape=Satellite)) +
  geom_point(size=4)+ 
  theme_classic()

ggsave("Plot_group_Rsquaresect.jpeg", units="in", dpi=400, width=8,height=6.5)

#Histogram

par(mfrow=c(1,2))

hist(Processed_Summary$Correlation,main="Histogram Correlation", xlab="Pearson Correlation")
boxplot(Processed_Summary$Correlation,main="Boxplot Correlation", ylab="Pearson correlation")
summary(Processed_Summary$Correlation)

#Boxplot difference
############################################################################

k<-Processed_DIFF


# REGION PLOT

#X11()
d5 <- data.frame(x = unlist(k), 
                 #code = rep(1:length(k),times = sapply(k,length)),
                 code = rep(Processed_Summary$Code,times = sapply(k,length)),
                 Section= rep(Processed_Summary$Section,times = sapply(k,length)),
                 Region= rep(Processed_Summary$Region,times = sapply(k,length)),
                 River= rep(Processed_Summary$River,times = sapply(k,length)),
                 Distance= rep(Processed_Summary$Distance,times = sapply(k,length)),
                 Satellite= rep(Processed_Summary$Satellite,times = sapply(k,length)),
                 namestat= rep(Processed_Summary$Station,times = sapply(k,length)))


#Statistics


png("Correlation_table.png",
    width = 10.33, height = 6.29, units = "in",   res =400)
par(mfrow=c(1,2))
hist(Processed_Summary$Correlation,main="Histogram Correlation")
boxplot(Processed_Summary$Correlation,main="Boxplot Correlation")
dev.off()


summary(Processed_Summary$Correlation)

# REGION PLOT
GREG<-ggplot(d5,aes(x = as.factor(code), y = x, fill=Region)) + geom_boxplot()+
  xlab("Couples")+ylab("Hydro-Satellite differences")+ 
  ggtitle("Difference Hydro-Satellite processed")+
    theme(plot.title = element_text(hjust = 0.5, size=16, face="bold",lineheight=1))

#ggsave("Statistics_region.jpeg", units="in", dpi=400, width=8,height=6.5)

# SATELLITE TYPE

GSAT<-ggplot(d5,aes(x = as.factor(code), y = x, fill=Satellite)) + geom_boxplot()+
  xlab("Couples")+ylab("Hydro-Satellite differences")#+ 
 # ggtitle("Difference Hydro-Satellite processed")

#ggsave("Statistics_satellite.jpeg", units="in", dpi=400, width=8,height=6.5)

ggarrange(GREG,GSAT, ncol=1,nrow=2,labels=c("a)","b)"))

ggsave("01.Region_Sat.jpeg", units="in", dpi=400, width=8.3,height=7.8)

############################################################################
# GROUPED PLOT
##UPDATE

da1<-ggplot(d5, aes(x=as.factor(Section), y=x, fill=Satellite)) +geom_boxplot(outlier.shape = NA) +
  facet_wrap(~Region, scales='free')+ scale_y_continuous(limits=c(-2,2)) +
  xlab("Section [km]")+ ylab("Hydro-Satellite difference")

da1 + ggforce::facet_row(vars(Region), scales = 'free', space = 'free')

ggsave("Group_region_1row.jpeg", units="in", dpi=400, width=12,height=1.5)


#Region
ggplot(d5, aes(x=as.factor(Section), y=x, fill=Satellite)) +geom_boxplot(outlier.shape = NA) +
                        facet_wrap(~Region, scales='free')+ scale_y_continuous(limits=c(-2,2)) +
                        xlab("Section [km]")+ ylab("Hydro-Satellite difference")

ggsave("01.Grouped Info_region.jpeg", units="in", dpi=400, width=8.3,height=7.8)

#River
ggplot(d5, aes(x=as.factor(Section), y=x, fill=Satellite)) +geom_boxplot(outlier.shape = NA) +
  facet_wrap(~River, scales='free')+ scale_y_continuous(limits=c(-2,2)) +
  xlab("Section [km]")+ ylab("Hydro-Satellite difference")

ggsave("01.Grouped Info_river.jpeg", units="in", dpi=400, width=8.3,height=7.8)




# SECTION PLOT

Sort_sect<-Sort(d5,ord="Section")

level_order<-unique(Sort_sect$code)
level_orderstat<-unique(Sort_sect$namestat)

G2<-ggplot(Sort_sect,aes(x = factor(code, level=level_order), y = x, fill=Section)) + geom_boxplot()+
  xlab("Couples")+ylab("Hydro-Satellite differences")+ 
  ggtitle("Difference Hydro-Satellite processed")+
  #scale_fill_gradient(low="red", high="blue")
 scale_fill_gradientn(colours = rainbow(5))

#WITH NAME STATION IN THE X
ggplot(d5,aes(x = factor(namestat,level=level_orderstat), y = x, fill=Section)) + geom_boxplot()+
  xlab("Couples")+ylab("Hydro-Satellite differences")+ 
  ggtitle("Difference Hydro-Satellite processed")+
  #scale_fill_gradient(low="red", high="blue")
  scale_fill_gradientn(colours = rainbow(5)) +
  theme(axis.text.x=element_text(angle=45,hjust=1))

ggsave("Statistics_sectionwihtname.jpeg", units="in", dpi=400, width=8,height=6.5)

#ggsave("Statistics_section.jpeg", units="in", dpi=400, width=8,height=6.5)

############################################################################


G3<-ggplot(d5,aes(x = factor(code, level=level_order), y = x, fill=Distance)) + geom_boxplot()+
  xlab("Couples")+ylab("Hydro-Satellite differences")+ 
  ggtitle("Difference Hydro-Satellite processed")+
  scale_fill_gradient(low="red", high="blue")

#ggsave("Statistics_distance.jpeg", units="in", dpi=400, width=8,height=6.5)

###########################################################################
X11()
#ggarrange(G1,G2,G3,ncol=1,nrow=3)

#ggsave("Statistics_3x1.jpeg", units="in", dpi=400, width=6,height=8)

ggarrange(G2,G3,ncol=1,nrow=2)

ggsave("Statistics_2x1.jpeg", units="in", dpi=400, width=6,height=8)

#################### PER PAPER ################################
summary(Processed_Summary$Correlation)
summary(Processed_Summary$Section)

MASSIMI<-sapply(1:length(DIFFERENCES),function(x){max(DIFFERENCES[[x]],na.rm = TRUE)})
MINIMI<-sapply(1:length(DIFFERENCES),function(x){min(DIFFERENCES[[x]],na.rm = TRUE)})
MEDIAN<-sapply(1:length(DIFFERENCES),function(x){median(DIFFERENCES[[x]],na.rm = TRUE)})


length(which(Processed_Summary$Correlation>=0.66))/nrow(Processed_Summary)

par(mfrow=c(1,1)) 

png(filename = "Diff_max_min.png",
    width = 10.33, height = 6.29, units = "in",   res =400)
plot(MASSIMI, col="red", ylim=c(-7,+7), type="line", xlab="Couples" , ylab="Differences Hydro-Satellite")
lines(MINIMI,col="yellow")
lines(MEDIAN,col="blue")
legend("bottomright",legend=c("Maximum","Median","Minimum"),col=c("red","blue","yellow"), pt.cex=2, pch=15, cex=1.2)
dev.off()

  
d6<-na.omit(d5)
summary(d6$x)
plot(sort(d6$x), main="Distribution of differences: Hydro - Satellite")
min((d6$x))
max((d6$x))
