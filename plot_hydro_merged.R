

Station_worst<-c("Pizzighettone","Adige.a.Cavarzere","Adige.a.Badia.Polesine.1","Adige.a.Badia.Polesine.2")

Station_better<-c("BORETTO","PONTELAG","PONTELAG.2","Po.a.Ficarolo.1","Po.a.Ficarolo.2")

Station_better<-Station_worst
rsq2 <- function (x, y) cor(x, y, use="complete.obs") ^ 2


for ( jj in 1:length(Station_better))
{ 
  pos_sent<-which(names(SENT_HYDRO_data_ALL)==Station_better[jj])
  pos_hydro<-which(names(HYDRO_data_ALL2)==Station_better[jj])
  
  
  data_ALL<- data.frame(SENT_HYDRO_data_ALL[[pos_sent]]$Hydro_diff, SENT_HYDRO_data_ALL[[pos_sent]]$Sentinel_diff, SENT_HYDRO_data_ALL[[pos_sent]]$Data_format)
  colnames(data_ALL)<-c("Hydro","Sent","Data")
  data_ALL<-na.omit(data_ALL)
  
  preds <- data_ALL$Sent
  actual <-  data_ALL$Hydro
  rsq<-round(rsq2(preds,actual),3)
  
  df_long_ALL = gather(na.omit(data_ALL), key = var, value = value, Hydro, Sent)
  
   
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
  
  png(paste0(names(HYDRO_data_ALL2[pos_hydro]),"_1",".png"),width = 10.33, height = 6.29, units = "in",   res =400)
     
  ggarrange(  p2_ALL)
  dev.off()

  Plot_1<-paste(names(HYDRO_data_ALL2[pos_hydro]),".png")
  Plot_2<-paste0(names(HYDRO_data_ALL2[pos_hydro]),"_1",".png")
  
  g1<-ggdraw()+draw_image( Plot_1)
  g2<-ggdraw()+draw_image( Plot_2)
  
  
  png(paste(names(HYDRO_data_ALL2[pos_hydro]),"_merged",".png"),width = 10.33, height = 12, units = "in",   res =400)
  
  ggarrange(g1,g2,nrow=2,ncol=1)
  dev.off()

  }