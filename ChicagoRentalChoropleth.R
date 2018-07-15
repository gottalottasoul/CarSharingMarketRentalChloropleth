##############################################################################################
###   Zipcar Chicago Reservation by Platform
###   Let's make some maps!
###
###   built on: 1/15/16
###
##############################################################################################



#set working directory
setwd("C:\\Users\\Babbenante\\documents\\my stuff\\code\\geoproto\\")


#load the required libraries
#require(rgdal) || install.packages("rgeos", repos="http://cran.rstudio.org") 
#library(rgdal)
require(rgeos) || install.packages("rgeos", repos="http://cran.rstudio.org") 
library(rgeos)
#require(maptools) || install.packages("maptools", repos="http://cran.rstudio.org") 
#library(maptools)
#require(ggmap) || install.packages("ggmap", repos="http://cran.rstudio.org") 
#library(ggmap)
require(leaflet) || install.packages("leaflet", repos="http://cran.rstudio.org") 
library(leaflet)
require(ggplot2) || install.packages("ggplot2", repos="http://cran.rstudio.org") 
library(ggplot2)
require(scales) || install.packages("scales", repos="http://cran.rstudio.org") 
library(scales)
#require(Cairo) || install.packages("Cairo", repos="http://cran.rstudio.org") 
#library(Cairo)
require(plyr) || install.packages("plyr", repos="http://cran.rstudio.org") 
library(plyr)
require(dplyr) || install.packages("dplyr", repos="http://cran.rstudio.org") 
library(dplyr)
require(tidyr) || install.packages("tidyr", repos="http://cran.rstudio.org") 
library(tidyr)
require(data.table) || install.packages("data.table", repos="http://cran.rstudio.org") 
library(data.table)
require(RColorBrewer) || install.packages("RColorBrewer", repos="http://cran.rstudio.org") 
library(RColorBrewer)

#require(wellknown) || install.packages("wellknown", repos="http://cran.rstudio.org") 
#library(wellknown)


## clear the console of all variables
rm(list = ls())
## free up unused memory
gc()

###########################################################################
# Functions                                                             ###
###########################################################################
getColorPalette <- function(minColor=-2000,maxColor=2500){
  
  pal <- colorNumeric(
#    palette = "RdYlGn",
    palette = "YlOrRd",
    domain = c(minColor,maxColor)
  )
  return(pal)
  
}


###########################################################################
# End Functions                                                         ###
###########################################################################


#read our data
chi_geo = read.csv("..\\..\\Data\\Chicago Reservation\\chi-zone-geo.txt",sep=";",colClasses = c("integer", "character","character","integer","character"))
chi_res = read.csv("..\\..\\Data\\Chicago Reservation\\Chicago Reservations.txt",sep=";")

#something with the Oracle data export makes the first characters funky, so fix the names
names(chi_geo)[1]<-paste("ZONE_ID")
names(chi_res)[1]<-paste("RES_YEAR")


#chi_geo$polys <- chi_geo %$% 
#  mapply(rgeos::readWKT, id = ZONE_ID, text = ZONEGEOBOUNDS_WKT) %>% 
#  do.call(sp::rbind.SpatialPolygons, .)
#chi_geo<-data.table(chi_geo)

##########################################
#########crunch some numbers##############
##########################################

#build data view looking at Q4 YoY
#filter for oct/nov/dec
chi.res.Q4<-chi_res %>%
  filter(grepl('10|11|12',RES_MONTH)) %>%
  group_by(ZONE_ID,RES_YEAR) %>%
  summarise(TOT_RESERVATIONS=sum(TOT_RES)) %>%
  dcast(.,ZONE_ID~RES_YEAR) %>%
  group_by(ZONE_ID)
#figure out a way to do this better;
#it's stupid to not be able to pipe in the renaming
names(chi.res.Q4)[2]<-paste("yr2014")
names(chi.res.Q4)[3]<-paste("yr2015")
#switch to data table for quicker joining
chi.res.Q4<-data.table(chi.res.Q4)
chi.res.Q4<-chi.res.Q4 %>%
  group_by(ZONE_ID) %>%
  summarise(Yearly_diff=yr2015-yr2014) %>%
  left_join(chi_geo,.)
#back to data frame because everyone loves a data frame
chi.res.Q4<-data.frame(chi.res.Q4)

#build data view looking at monthlies YoY
chi.res.YoY<-chi_res %>%
  group_by(ZONE_ID,RES_YEAR,RES_MONTH) %>%
  summarise(TOT_RESERVATIONS=sum(TOT_RES)) %>%
  dcast(.,ZONE_ID+RES_MONTH~RES_YEAR,fun=sum)
names(chi.res.YoY)[3]<-paste("yr2014")
names(chi.res.YoY)[4]<-paste("yr2015")
chi.res.YoY<-data.table(chi.res.YoY)
chi.res.YoY<-chi.res.YoY %>%
  group_by(ZONE_ID,RES_MONTH) %>%
  summarise(Yearly_diff=yr2015-yr2014) %>%
  left_join(chi_geo,.) %>%
dcast(.,ZONE_ID+ZONE_DESCRIPTION+LEVEL_5_KEY+LEVEL_5_ID+ZONEGEOBOUNDS_WKT~RES_MONTH)
names(chi.res.YoY)[6]<-paste("Jan")
names(chi.res.YoY)[7]<-paste("Feb")
names(chi.res.YoY)[8]<-paste("Mar")
names(chi.res.YoY)[9]<-paste("Apr")
names(chi.res.YoY)[10]<-paste("May")
names(chi.res.YoY)[11]<-paste("Jun")
names(chi.res.YoY)[12]<-paste("Jul")
names(chi.res.YoY)[13]<-paste("Aug")
names(chi.res.YoY)[14]<-paste("Sep")
names(chi.res.YoY)[15]<-paste("Oct")
names(chi.res.YoY)[16]<-paste("Nov")
names(chi.res.YoY)[17]<-paste("Dec")

chi.res.YoY<-data.frame(chi.res.YoY)

##########################################
#########end data manipulation############
##########################################

# assign whatever dataset from above to to the var
# chi.data.map so we don't have to keep switching names
# and more than likely break something
chi.data.map<-chi.res.YoY 


#tranform WKT to polys
chi.data.map$polys <- chi.data.map %$% 
  mapply(rgeos::readWKT, id = ZONE_ID, text = ZONEGEOBOUNDS_WKT) %>% 
  do.call(sp::rbind.SpatialPolygons, .)



#build the range of color shades
#my_palette <- getColorPalette(min(chi.data.map$Yearly_diff,na.rm=TRUE),max(chi.data.map$Yearly_diff,na.rm=TRUE))
my_palette=colorNumeric(c("red","white","yellow"), domain = chi.data.map[,6:17])
#previewColors(pal,c(-2000,-1700,-1000,-800,-25,1,50,100))
#now build the map!
m <- leaflet() %>%
#  addTiles() %>%
  addProviderTiles("CartoDB.Positron") %>%  # Add default OpenStreetMap map tiles
  setView(lng = -87.6847, lat = 41.8360, zoom = 9) %>% 
  addPolygons(data=chi.data.map$polys,
              popup=paste0("<strong>",
                           chi.data.map$ZONE_DESCRIPTION,
                           "</strong><br>",
                           "<strong>difference: </strong>", 
                           chi.data.map$Oct, 
                           "<br>"), 
              stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,          
              color = my_palette(chi.data.map$Oct)) %>%
  addLegend(position='bottomleft',pal=my_palette,values=chi.data.map$Oct)

library(htmlwidgets)
saveWidget(m, file="m.html")


