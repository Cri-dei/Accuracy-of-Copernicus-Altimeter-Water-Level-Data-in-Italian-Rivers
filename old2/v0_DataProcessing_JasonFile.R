rm(list = ls())

library("rjson")
library("lubridate")


#computer portatile
setwd("C:/Users/39349/Documents/Remote Sensing/Sentinel 3 data")

#json_file <-  readLines("ElencoFile.txt")
json_file<-list.files(pattern="*.json")


Sentinel3_data<-list()
Sentinel3_data_Info<-matrix(,nrow=length(json_file),ncol=8)
colnames(Sentinel3_data_Info)<-c("Identifier","Longitude","Latitude","Basin","River","Time_start","Time_end","WaterSurfRef_Alt")
Type_sat<-data.frame(matrix(,nrow=length(json_file),ncol=2))
colnames(Type_sat)<-c("Identifier","Satellite")

for (f in 1: length(json_file))

{

my.JSON <- fromJSON(file = json_file[f])

my.JSON2 <- my.JSON$data
Name_col<-unlist(attributes(my.JSON2[[1]]))

###################create a matrix ##############################################

df1 <- lapply(my.JSON2, function(play) # Loop through each "play"
  {
  # Convert each group to a data frame.
  # This assumes you have 6 elements each time
  data.frame(matrix(unlist(play), ncol=length(Name_col), byrow=T))
  })

# Now you have a list of data frames, connect them together in
# one single dataframe
df1 <- do.call(rbind, df1)

# Make column names nicer, remove row names
colnames(df1) <- Name_col



############# Creation Data Matrix###################################################

Time<-as.numeric(as.character(df1$time))
Water_sur_reference<-as.numeric(as.character(my.JSON$properties$water_surface_reference_datum_altitude))
Water_level<- as.numeric(as.character( df1$water_surface_height_above_reference_datum))+Water_sur_reference
Coordinate<-as.numeric(as.character(my.JSON$geometry$coordinates))
Identifier<-as.numeric(as.character(my.JSON$properties$resource))
Type_sat$Identifier[f]<-Identifier
Type_sat$Satellite[f]<-my.JSON$properties$platform

M_time<- matrix(,nrow=length(Time),ncol=6) 
colnames(M_time)=c("Identifier","Longitude","Latitude","Time","Time-h-d","Water level") 

for (i in 1:length(Time))
{ 
M_time[1,1]<- Identifier
M_time[1,2]<- Coordinate[1]  
M_time[1,3]<- Coordinate [2]
M_time[i,4]<- Time[i]
M_time[i,5]<-format(date_decimal(Time[i]), "%d-%m-%Y-%H:%M")
M_time[i,6]<- Water_level[i]

}

Sentinel3_data[[f]]<-M_time

# Optional:Save excel processed file 
write.table(M_time ,paste0("S3A_Processed_",Identifier,".csv"), row.names=F)

#################### Creation Attribute Matrix#####################################################


Sentinel3_data_Info[f,]<-c(as.numeric(my.JSON$properties$resource),
                        my.JSON$geometry$coordinates,
                        my.JSON$properties$basin,
                        my.JSON$properties$river,
                        my.JSON$properties$time_coverage_start,
                        my.JSON$properties$time_coverage_end,
                        my.JSON$properties$water_surface_reference_datum_altitude)

}

write.table(Sentinel3_data_Info ,"Sentinel3_data_Info.csv", row.names=F)

#Save workspace #

save(Sentinel3_data,Sentinel3_data_Info,Type_sat, file="Sentinel3Data_processed.RData")

