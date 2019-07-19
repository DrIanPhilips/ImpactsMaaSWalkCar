#' ---
#' title: "Make MaaS Indicator"
#' author: "Ian Philips, https://environment.leeds.ac.uk/transport/staff/972/dr-ian-philips"
#' date: "17 July 2019"
#'
#' ---
#'

#packages
library(tidyverse)
library(sf)
library(tmap)
library(gridExtra)
library(tmaptools)
library(lubridate)


#'------------ **Introduction** -----------
#The data used in the analysis comes from several sources including some which 
#are not publically available.    


#'------------ **Read in the get the wrangled data** -----------
mot_acornGM <- read_csv("data/mot_acornGM_wrangled060419.csv")

#'------------- **make a postcode level sf data set** ---------------

#make an sf point data set from the mot_acornGM data
#The wrangled data includes UK national Grid x and y co-ordinates
mot_acornGM_sf <- st_as_sf(x = mot_acornGM, 
                           coords = c("resAd.Easting", "resAd.Northing"),
                           crs = "+init=epsg:27700")


#'----------- **do the indicator production steps** ------------.
#'
#'**step1 code Define an initial MaaS catchment for GM**
#'The catchment is where the full range of MaaS services 
#'including unlimited taxi use would be available.    
#'Buffer a 3 mile (5km) radius of the city centre.   
#'Whim West Midlands has a rule that you can only start a Gett journey within 3 miles 
#'of the city centre. 
#'I will put a 5km buffer around Manchester Town Hall 
#'<https://helpcenter.whimapp.com/hc/en-us/articles/360002685713-Whim-Subscription-Plans-in-Birmingham> 
#'  NB also max taxi (Gett) journey length is 3 miles)

x = 383864  
y = 398084  
town_hall <- data.frame(x,y)
town_hall<- st_as_sf(x = town_hall, 
                     coords = c("x", "y"),
                     crs = "+init=epsg:27700")

#' A 3 mile buffer is 3*1602 metres = 4806
town_hall_buf <- st_buffer(x = town_hall,4806)


#' make sure everything has the same crs
st_crs(town_hall)
st_crs(town_hall_buf)
st_crs(mot_acornGM_sf)
#'If the crs don't match then transform. 
#st_transform(x = mot_acornGM_sf,crs = 27700)
#st_transform(x = town_hall,crs = 27700)
#st_transform(x = town_hall_buf,crs = st_crs(mot_acornGM_sf))

#simple check that the crs match and things plot properly
plot(st_geometry(mot_acornGM_sf),col ='orange')
plot(st_geometry(town_hall_buf),col = 'black',add = T)


#' Select the the postcodes which are inside the buffer 
taxi_range1 = mot_acornGM_sf[town_hall_buf, ]
# alternative code to find acorn postcodes within the 3 mile buffer 
#does the same thing
#taxi_range2 <- st_join(town_hall_buf,mot_acornGM_sf , join = st_intersects)

#simple check that postcodes in the buffer have been selected
plot(st_geometry(taxi_range1),col= 'blue')
plot(st_geometry(town_hall), pch = 3, col = 'red', add = TRUE)
plot(st_geometry(town_hall_buf), add = TRUE)



#'**Step 2 Define affordability **
#'The Whim unlimited MaaS package is initially priced at £349 per month. 
#'As a simple assumption We excluded any areas which are in ACORN categories 4 and 5.
#' "4 Financially Stretched", "5 Urban Adversity",

#'Postcodes less likely to afford MaaS unlimited package shown in red,
#'postcodes in green are those more likely to be able to afford MaaS unlimited package
afford_MaaS <- taxi_range1 %>%
  filter(AcornCategory != 4 & AcornCategory != 5)

cant_afford_MaaS <- taxi_range1 %>%
  filter(AcornCategory == 4 | AcornCategory == 5)

#check crs
st_crs(afford_MaaS)
st_crs(cant_afford_MaaS)
#set if needed
#cant_afford_MaaS <- st_transform(x = cant_afford_MaaS,crs = st_crs(mot_acornGM_sf))
#afford_MaaS <- st_transform(x = afford_MaaS,crs = st_crs(mot_acornGM_sf))
#plot to check 
plot(st_geometry(cant_afford_MaaS),col= 'red')
plot(st_geometry(afford_MaaS),  col = 'green', add = TRUE)
plot(st_geometry(town_hall_buf), add = TRUE)

#'plot the spatial distribution of affordability with a backdrop map

#Map the data with a basemap and save as plots
tmap_mode("plot")
#This generates the OSM backdrop
c_osm <- read_osm(afford_MaaS, ext = 1.25)

afford_MaaS_map <- afford_MaaS %>%  filter(walku5 >= 0)
#afford_MaaS_map <- afford_MaaS %>%  drop_na(walku5)
map_c <- tm_shape(c_osm) +
  tm_rgb(alpha = 0.5) +
  tm_shape(afford_MaaS) +
  tm_dots(
    col = "green", alpha = 0.5)+
  tm_shape(cant_afford_MaaS) +
  tm_dots(
    col = "red", alpha = 0.5)+
  tm_shape(town_hall_buf)+
  tm_fill(col = NA,alpha = 0)+
  tm_borders(col = "black", lwd = 1, lty = "solid")+
    tm_shape(town_hall)+
  tm_dots(col = "black",size = 1)+
  tm_scale_bar()
 
map_c
#paste together a filename which includes today's date 
fname <- paste0("affordmaas_",today("GMT"), ".png")
#save the map
tmap::tmap_save(map_c, filename= fname)
#save_tmap(map_c, filename= fname)


#'Count the people in the can and can't afford MaaS datasets 
#total population in the buffer 364512
pop_taxirange <- sum(afford_MaaS$postcodepopTotal,na.rm=T) + sum(cant_afford_MaaS$postcodepopTotal,na.rm=T)
sum(afford_MaaS$postcodepopTotal,na.rm=T) / pop_taxirange
#33% of population in taxi range likely to afford MaaS unlimited
sum(cant_afford_MaaS$postcodepopTotal,na.rm=T)/ pop_taxirange
#67% less likely to afford it 


#' Only 33% of the population within the buffer could afford 
#' MaaS.  This shows transport equity may be an issue should be investigated further


#' Vulnerability to externalities
#' If carrying out a social and distributional impact assessment the following 
#' might be useful:
#' Those who walk or cycle and who cannot afford MaaS gives an indicator of 
#' those wo do not gain user benefits and who are
#' vulnerable to externalities if MaaS increases traffic
sum(cant_afford_MaaS$walku5,na.rm = T) + sum(cant_afford_MaaS$bikeu5,na.rm = T) 
#16071 


#'**Step 3: Calculate the current commuting mode of potential MaaS adopters and potential for mode shift**

#'in the data wrangling phase the number of commuters by mode travelling less than 5km 
#'was estimated using 
#'Census travel to work by mode: 
#'  https://www.nomisweb.co.uk/census/2011/qs702ew 
#'UK National Travel Survey: proportion of trips under 5km in UK by mode: 
#'  https://www.nomisweb.co.uk/census/2011/qs701ew and NTS table0306

#to see a comparison of numbers of postcodes and the number of people in 
#each who could change mode to MaaS taxi you could plot this 
#multiple frequency polygon.  
#However this is a "marmite" plot  - 
#some people like them and others really don't!
gg <- 
  afford_MaaS %>% 
#filter(pc_walk/100 * Total < 25) %>%  
  ggplot(show.legend = TRUE)+
  geom_freqpoly(aes(walku5),colour = "blue")+
  geom_freqpoly(aes(bikeu5),colour = "green")+
  geom_freqpoly(aes(busu5),colour = "yellow")+
  geom_freqpoly(aes(metrou5),colour = "orange")+
  geom_freqpoly(aes(railu5),colour = "purple")+
  #  geom_freqpoly(aes((pc_drivecar/100 + pc_pass_car/100)* Total),colour = "red")+
  #labs(title="Modeshare * population", y= "Number of postcodes", x="number of people", 
  labs(title="People who may change to MaaS \nfrom existing sustainable modes", y= "Number of postcodes", x=" number of people in a postcode \nwho may change to MaaS from sustainable modes", 
       #     caption="walk-blue \n bike-green \n bus-yellow \n metro-orange")
       caption="walk-blue \n bike-green \n bus-yellow \n metro-orange \n train-purple")

  ggsave(filename = "freqpolyFig.jpeg",width=297,height=210,units="mm",dpi=300)

gg

#The count data can also be summarised as a barplot
summary(afford_MaaS$walku5,na.rm = T)
count_walk <- sum(afford_MaaS$walku5,na.rm = T) #10515
count_bike <- sum(afford_MaaS$bikeu5,na.rm = T) #372
count_bus <- sum(afford_MaaS$busu5,na.rm = T) #1490
count_metro <- sum(afford_MaaS$metrou5,na.rm = T)#343
count_rail <- sum(afford_MaaS$railu5,na.rm = T)#89
#'Segment (Anable) suggests positive attitude towards driving less 
#'is associated with people who already drive less than average. 
#'Where car use is low and people can afford MaaS and people want to make 
#'a car based trip, they are more likely to use MaaS than where car ownership and 
#'use is higher.   

#filter keep car users with below mean mileage as those with potential to use MaaS

countcar_nocontext <- sum(afford_MaaS$caru5,na.rm = T)#7213 if no filtering 
count_car_context <- afford_MaaS %>% filter(Miles_pp < 2.6557)
count_car_context <- sum(count_car_context$caru5,na.rm = T)#5900 

count_walk_no_context <- sum(afford_MaaS$walku5,na.rm = T)#10515# if no filtering 
count_walk_context <- afford_MaaS %>% filter(Miles_pp < 2.6557)
count_walk_context <- sum(count_walk_context$walku5,na.rm = T)#10007 


df_count_walk_caru5 <- data.frame( mode = c("walk_nobehavioural_context",
                                           "walk_withbehavioural_context",
                                           "car_nobehavioural_context",
                                           "car_withbehavioural_context"),
                                           people =  c(count_walk_no_context,
                                                       count_walk_context,
                                                       countcar_nocontext,
                                                       count_car_context
                                                       )
                                           )
df_count_walk_caru5
write_csv(df_count_walk_caru5,"df_count_walk_caru5.csv")                                  

#make a barplot 
p <-ggplot(data=df_count_walk_caru5, aes(x=mode, y=people)) +
  geom_bar(stat="identity",fill=c("grey","blue","grey","orange"))+
  theme(axis.text=element_text(size=20),
          axis.title=element_text(size=14,face="bold"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  ggsave(filename = "barplot190719.jpeg",width=297,height=210,units="mm",dpi=300)

p


walk <- as.vector(summary(afford_MaaS$walku5,na.rm = T))
bike <- as.vector(summary(afford_MaaS$bikeu5,na.rm = T))
bus <- as.vector(summary(afford_MaaS$busu5,na.rm = T))
metro <- as.vector(summary(afford_MaaS$metrou5,na.rm = T))
statistic <- c("Min","1stQuartile","Median","Mean","3rdQuartile","Max","NA's")

summarydf <- tibble(statistic,walk,bike,bus,metro)
write_csv(summarydf,"summarydf190719.csv")



#'--------------------- **mapping** -------------------- 

#'we want to identify postcodes where there is a risk of large numbers
#'of people shifting from currently sustainable modes modes to 
#'MaaS 

#'We would also like to identify where there is potential use of MaaS taxi by current car users  
#'The impacts here could be positive e.g. reduction car fleet
#'or negative e.g increased empty running of taxis to service previous journeys by owned cars.  


#'map sustainable modes
#https://stackoverflow.com/questions/52208470/saving-a-tmap-with-a-basemap-as-an-image

#map walk with no context about adopters
#library(tmaptools)
#Map the data with a basemap and save as plots
#this makes Figure 2 in the paper
tmap_mode("plot")
c_osm <- read_osm(afford_MaaS, ext = 1.25)
afford_MaaS_map$walk <- afford_MaaS_map$walku5
afford_MaaS_map <- afford_MaaS_map %>%  filter(walk >=0)

map_c <- tm_shape(c_osm) +
  tm_rgb(alpha=0.5)+
  tm_shape(afford_MaaS_map) +
  tm_dots(
    col = "blue", alpha = 0.5,
    size = "walk" , 
    legend.show = T, title = "walk"
    #style="jenks",
    #palette="-RdYlBu", contrast=1
  )+
  tm_scale_bar()+
  tm_shape(town_hall_buf)+
  tm_fill(col = NA,alpha = 0)+
  tm_borders(col = "black", lwd = 1, lty = "solid")+
  tm_legend(position=c("left", "top"), bg.color="grey95", frame=TRUE)
#tm_layout(legend.outside = T,legend.outside.position = "bottom")
map_c
#tmap::tmap_save(map_c, filename="walk_riskBD_170719.png")
#save_tmap(map_c, filename="walk_risknocontextBD_190719.png")
#paste together a filename which includes today's date 
fname <- paste0("walk_risknocontextBD_",today("GMT"), ".png")
#save the map
tmap::tmap_save(map_c, filename= fname)





#walk with adopter context 
# adopters are likley to be people who already have below average mileage 
afford_MaaS_map$walk <- afford_MaaS_map$walku5
afford_MaaS_map <- afford_MaaS_map %>%  filter(Miles_pp < 2.6557)

map_c <- tm_shape(c_osm) +
  tm_rgb(alpha=0.5)+
  tm_shape(afford_MaaS_map) +
  tm_dots(
    col = "blue", alpha = 0.5,
    size = "walk" , 
    legend.show = T, title = "walk"
    #style="jenks",
    #palette="-RdYlBu", contrast=1
  )+
  tm_scale_bar()+
  tm_shape(town_hall_buf)+
  tm_fill(col = NA,alpha = 0)+
  tm_borders(col = "black", lwd = 1, lty = "solid")+
  tm_legend(position=c("left", "top"), bg.color="grey95", frame=TRUE)
#tm_layout(legend.outside = T,legend.outside.position = "bottom")
map_c
#tmap::tmap_save(map_c, filename="walk_riskBD_170719.png")
##paste together a filename which includes today's date 
fname <- paste0("walk_riskcontextBD_",today("GMT"), ".png")
#save the map
tmap::tmap_save(map_c, filename= fname)



#car  potential to shift to MaaS taxi no context
afford_MaaS_map <- afford_MaaS_map#%>% filter(caru5 >= 0)
afford_MaaS_map$car <- afford_MaaS_map$caru5
map_c <- tm_shape(c_osm) +
  tm_rgb(0.5) +
  tm_shape(afford_MaaS_map) +
  tm_dots(scale = 0.5,
          col = "orange", alpha = 0.8,
          size = "car" , 
          legend.show = T
          #,
          #style="quantile",
          #  palette="RdYlBu", contrast=1
  )+
  tm_shape(town_hall_buf)+
  tm_fill(col = NA,alpha = 0)+
  tm_borders(col = "black", lwd = 1, lty = "solid")+
  tm_scale_bar()+
  tm_legend(position=c("right", "top"), bg.color="grey95", frame=TRUE)

map_c
#save_tmap(map_c, filename="car_opportunityselectedbelowAvgMilesppBD_180719.png")
##paste together a filename which includes today's date 
fname <- paste0("car_opportunityselectedbelowAvgMilesppBD_",today("GMT"), ".png")
#save the map
tmap::tmap_save(map_c, filename= fname)





#car  potential to shift to MaaS taxi with context
# adopters are likley to be people who already have below average mileage 
#this makes figure 2b in the paper 
afford_MaaS_map <- afford_MaaS_map %>% filter(Miles_pp < 2.6557)
afford_MaaS_map$car <- afford_MaaS_map$caru5
map_c <- tm_shape(c_osm) +
  tm_rgb(0.5) +
  tm_shape(afford_MaaS_map) +
  tm_dots(scale = 0.5,
          col = "orange", alpha = 0.8,
          size = "car" , 
          legend.show = T
          #,
          #style="quantile",
          #  palette="RdYlBu", contrast=1
  )+
  tm_shape(town_hall_buf)+
  tm_fill(col = NA,alpha = 0)+
  tm_borders(col = "black", lwd = 1, lty = "solid")+
  tm_scale_bar()+
  tm_legend(position=c("right", "top"), bg.color="grey95", frame=TRUE)

map_c
#save_tmap(map_c, filename="car_opportunityselectedbelowAvgMilesppBD_180719.png")
##paste together a filename which includes today's date 
fname <- paste0("car_opportunityselectedbelowAvgMilesppBDcontext_",today("GMT"), ".png")
#save the map
tmap::tmap_save(map_c, filename= fname)






#--------- explore if greatest potential to change from car and walk are in different places.  
#first filter 


summary(afford_MaaS_map$car)
summary(afford_MaaS_map$walk)
#first makesure we only take places where the users have below the 
#car mileage threshold
afford_MaaS_map <- afford_MaaS_map %>%  filter(Miles_pp < 2.6557)
hiqcar <- afford_MaaS_map %>% filter(car > 3.97920)
hiqwalk <- afford_MaaS_map %>% filter(walk > 6.13199)


#car  potential to shift to MaaS taxi
#this makes figure 3 in the paper 
#afford_MaaS_map <- afford_MaaS_map %>% filter(Miles_pp < 2.6557)
#afford_MaaS_map$car <- afford_MaaS_map$caru5
map_c <- tm_shape(c_osm) +
  tm_rgb(0.5) +
  tm_shape(hiqcar) +
  tm_dots(shape = 0,
    scale = 1.5,
          col = "orange", alpha = 0.8,
          #size = "car" , 
          #legend.show = T
          #,
          #style="quantile",
          #  palette="RdYlBu", contrast=1
  )+
  tm_shape(hiqwalk) +
  tm_dots(shape = 3,
    scale = 1.5,
    col = "blue", alpha = 0.5,
    #size = "car" , 
    #legend.show = T
    #,
    #style="quantile",
    #  palette="RdYlBu", contrast=1
  )+
  tm_shape(town_hall_buf)+
  tm_fill(col = NA,alpha = 0)+
  tm_borders(col = "black", lwd = 1, lty = "solid")+
  tm_scale_bar()+
  tm_legend(position=c("right", "top"), bg.color="grey95", frame=TRUE)

map_c
#save_tmap(map_c, filename="wherecarwalkBD_180719v5.png")

fname <- paste0("wherecarwalkBD_",today("GMT"), ".png")
#save the map
tmap::tmap_save(map_c, filename= fname)


