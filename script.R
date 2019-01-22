# tmap : To use qtm read_shape
library(tmap)
library(tmaptools)
library(sf)
library(sp)

# To read excel files
#install.packages("rio")
library(rio)
# We use 'which' to specify id of data set , we've choose the 7th one
mydata <- rio::import("Data/MasterBioInfo20182019.xlsx", which = 7)
# Cleanning my data set
names(mydata) <- mydata[1, ]
mydata <- mydata[-c(1), ]

# Dont display the scietific notation
options(scipen = 999)

# Read shape File (map)
#method 1
mW <- st_read("Data/wilayas/Wilayas_wgs.84.shp")
#method 2
# (as.sf = TRUE) Means I want 'mW' to be a simple features object.
# The simple features standards were recently implemented in R with the 'sf' package
mW <- read_shape(file="Data/wilayas/Wilayas_wgs.84.shp", as.sf=TRUE)


# Wilaya's names to character to be able to compare it and join it with my data
mW$wi_nom_fr <- as.character(mW$wi_nom_fr)

# Take the same order for shapeFile and dataSet to be able to join theme
mW <- mW[order(mW$wi_nom_fr),]
mydata <- mydata[order(mydata$wilaya),]

# Test if the columns are identical , if not w can't join shp <-> data
ifelse(identical(mW$wi_nom_fr,mydata$wilaya ),
       #join 
       finalMap <- append_data(mW, mydata, key.shp = "wi_nom_fr", key.data="wilaya"),
       #else
       stop("Error in matching SHP <-> DATA")
       )


#project the result , we can project any row from the data/shp
tmap_mode("view")

#Determinate the number of classes : using (b-a)/(3.5*sig*N**(-1/3))
N <- nrow(mydata)
values <- c( as.numeric(mydata$`Year 2016  Map1`),
             as.numeric(mydata$`Year 2016 Map2`),
             as.numeric(mydata$`Year 2016 Map3`),
             as.numeric(mydata$`Year 2016 Map4`)
           )
a <- min(values)
b <- max(values)

nb_class = round( (b-a)/(3.5*sd(values)*N**(-1/3)),0 )

tmap_options(max.categories = nb_class)

myMaps <- tm_shape(finalMap)+
          tm_polygons(names(mydata)[2:5])+
          tm_facets( ncol = 2)
myMaps

tmap_mode("plot")
myMaps <- tm_shape(finalMap)+
               tm_polygons(names(mydata)[2:5])+
               tm_facets( ncol = 2)+
              #data showing in the map
              tm_text("wi_no",size = 0.8) 
myMaps

# To save those maps
tmap_save(myMaps, "StingMaps7.pdf", height=10)


# DO IT BETTER -----------------------------------


#install.packages("scales")
library(scales)

#install.packages("magrittr") # only needed the first time you use it
#install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # need to run every time you start R and want to use %>%
library(dplyr)    # alternative, this also loads %>%

# Using JavaScript libraries for interactive maps.
#install.packages("leaflet")
library(leaflet)

 
 #NEXT LEVEL 
 # Color palette  / domain attribute is for values rang
 new_Palette <- colorBin(palette = "Blues",
                         bins = nb_class,
                         domain=c(as.numeric(mydata$m1),
                                  as.numeric(mydata$m2),
                                  as.numeric(mydata$m3),
                                  as.numeric(mydata$m4)
                                  )
                          )

 # Personalize popups infos
 colnames(mydata)[2:5] <- c("m1","m2","m3","m4")
 D1_wPopup <- paste0("Wilaya: ", mW$wi_nom_fr," - ","data1  : ", as.numeric(mydata$m1))
 D2_wPopup <- paste0("Wilaya: ", mW$wi_nom_fr," - ","data2 : ", as.numeric(mydata$m2))
 D3_wPopup <- paste0("Wilaya: ", mW$wi_nom_fr," - ","data3 : ", as.numeric(mydata$m3))
 D4_wPopup <- paste0("Wilaya: ", mW$wi_nom_fr," - ","data4 : ", as.numeric(mydata$m4))
 
 # Change some attributes for projecting map
 projectedMap <- st_transform(mW, "+proj=longlat +datum=WGS84")
 
 mapWithControl <- leaflet(projectedMap) %>%
   #map background provider
   addProviderTiles("CartoDB.Positron") %>% 
   addTiles(group = "all") %>%
   addPolygons(stroke=FALSE,
               smoothFactor = 0.2,
               fillOpacity = .8,
               popup=D1_wPopup,
               color= ~new_Palette(as.numeric(mydata$m1)) ,
               group = "Year 2016 Map1"
               ) %>% 
   addPolygons(stroke=FALSE,
               smoothFactor = 0.2,
               fillOpacity = .8,
               popup=D2_wPopup,
               color= ~new_Palette(as.numeric(mydata$m2)),
               group = "Year 2016 Map2"
   ) %>%
   addTiles(group = "Year 2016 Map3") %>%
   addPolygons(stroke=FALSE,
               smoothFactor = 0.2,
               fillOpacity = .8,
               popup=D3_wPopup,
               color= ~new_Palette(as.numeric(mydata$m3)) ,
               group = "Year 2016 Map3"
   ) %>%
   addTiles(group = "Year 2016 Map4") %>%
   addPolygons(stroke=FALSE,
               smoothFactor = 0.2,
               fillOpacity = .8,
               popup=D4_wPopup,
               color= ~new_Palette(as.numeric(mydata$m4)) ,
               group = "Year 2016 Map4"
   ) %>%
   addLegend(pal = new_Palette, values = ~values, opacity = 1,title = "Map Lengend" , "bottomright")%>%
   addLayersControl(baseGroups=c("Year 2016 Map1","Year 2016 Map2","Year 2016 Map3","Year 2016 Map4"),
                     position = "bottomleft",
                     options = layersControlOptions(collapsed = FALSE)
                    )%>%
   addControl("We can mention here a significant title for this projection",
              position = "topright"
              )
 
 mapWithControl
 
 # SAVE as HTML with all Js dependecies 
 #install.packages("htmlwidgets")
 library(htmlwidgets)
 saveWidget(widget=mapWithControl,
            file="ProjectMap_withdependencies.html",
            selfcontained=FALSE,
            libdir = "js",
            title = "BioStatistics Project")

 
 #  FINISHED ---
 