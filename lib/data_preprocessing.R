##### Download Packages Needed
if (!require("shiny")) {
  install.packages("shiny")
  library(shiny)}

if (!require("broom")) {
  install.packages("broom")
  library(broom)}

if (!require("dplyr")) {
  install.packages("dplyr")
  library(dplyr)}

if (!require("geojsonio")) {
  install.packages("geojsonio")
  library(geojsonio)}

if (!require("ggnewscale")) {
  install.packages("ggnewscale")
  library(ggnewscale)}


if (!require("osmdata")) {
  install.packages("osmdata")
  library(osmdata)}


if (!require("RSocrata")) {
  install.packages("RSocrata")
  library(RSocrata)}

if (!require("rstudioapi")) {
  install.packages("rstudioapi")
  library(rstuioapi)}

if (!require("sf")) {
  install.packages("sf")
  library(sf)}

if (!require("tidyverse")) {
  install.packages("tidyverse")
  library(tidyverse)}

if (!require("wesanderson")) {
  install.packages("wesanderson")
  library(wesanderson)}

if (!require("leaflet.extras")) {
  install.packages("leaflet.extras")
  library(leaflet.extras)}

if (!require("htmltools")) {
  install.packages("htmltools")
  library(htmltools)}


# Set working directory
current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
getwd()

# Load the restaurant dataset
data <- read.socrata(
  "https://data.cityofnewyork.us/resource/43nn-pn8j.json",
  app_token = "zTRehp1897SQtpYtBiIOUMfR4"
)

# Extract years
data$year <- format(data$inspection_date,"%Y")

# Filter the dataset
df <- data %>%
  filter(data$year >= 2019 & zipcode != "" & dba != "") %>%
  mutate(grade = replace(grade, grade == "", NA))
df$latitude <- as.numeric(df$latitude)
df$longitude <- as.numeric(df$longitude)
# Read the geojson file containing spatial info
spdf_file <- geojson_read("../data/zip_code_040114.geojson", what = "sp")

stats_df <- spdf_file@data

# Convert it to a spatial data frame, with zip code as index
spdf_data <- tidy(spdf_file,
                  region = "ZIPCODE"  # Use ZIPCODE variable as index, the index will be named "id"
)


borough_list <- append("Overall", unique(df$boro))
cuisine_list <- append("Overall", unique(df$cuisine_description))
saveRDS(borough_list,file="../data/borough_list.Rda")
saveRDS(cuisine_list,file="../data/cuisine_list.Rda")
saveRDS(df,file = "../data/df.Rda")
##============ Data Preparation for Violation Maps =============
####=====critical violations ============#####
### Grade info for each type of restaurant
grades =
  df %>% 
  group_by(dba, latitude, longitude) %>%   ##windows function partition by DBA, Lat, and Long
  mutate(rowNum = row_number(desc(inspection_date))) %>% #specify how to list row numbers here; desc for latest first.
  filter(rowNum == 1)%>%
  drop_na(longitude)%>% 
  mutate(CUISINE.TYPE = 
           case_when(cuisine_description == "American" ~ "American", 
                     cuisine_description == "Chinese" ~ "Chinese",
                     cuisine_description == "Coffee/Tea" ~ "Coffee", 
                     cuisine_description == "Pizza" ~ "Pizza",
                     cuisine_description == "Italian" ~ "Italian", 
                     cuisine_description == "Mexican" ~ "Mexican",
                     TRUE ~ "Others"))

American = 
  grades%>%filter(CUISINE.TYPE == "American")

Chinese= 
  grades%>%filter(CUISINE.TYPE == "Chinese")

Coffee = 
  grades%>%filter(CUISINE.TYPE == "Coffee")

Italian = 
  grades%>%filter(CUISINE.TYPE == "Italian")

Pizza = 
  grades%>%filter(CUISINE.TYPE == "Pizza")

Mexican = 
  grades%>%filter(CUISINE.TYPE == "Mexican")

Others = 
  grades%>%filter(CUISINE.TYPE == "Others")

#### Number of restaurant per ZIPCODE
Num_Rest_Code =
  df%>%
  group_by(zipcode, dba, latitude, longitude)%>%
  count() %>%
  group_by(zipcode)%>%
  count()

Critical_2019_by_Code = 
  df%>%
  filter(year == 2019)%>%
  group_by(zipcode)%>%
  summarize(Total = n())

Critical_2020_by_Code = 
  df%>%
  filter(year == 2020)%>%
  group_by(zipcode)%>%
  summarize(Total = n())

Critical_2021_by_Code = 
  df%>%
  filter(year == 2021)%>%
  group_by(zipcode)%>%
  summarize(Total = n())

Critical_2022_by_Code = 
  df%>%
  filter(year == 2022)%>%
  group_by(zipcode)%>%
  summarize(Total = n())


spdf_file_2022 = spdf_file
spdf_file_2022@data =
  spdf_file_2022@data %>%
  left_join(Critical_2022_by_Code, c("ZIPCODE"="zipcode"))



spdf_file_2019 = spdf_file
spdf_file_2019@data =
  spdf_file_2019@data %>%
  left_join(Critical_2019_by_Code, c("ZIPCODE"="zipcode"))

spdf_file_2020 = spdf_file
spdf_file_2020@data =
  spdf_file_2020@data %>%
  left_join(Critical_2020_by_Code, c("ZIPCODE"="zipcode"))

spdf_file_2021 = spdf_file
spdf_file_2021@data =
  spdf_file_2021@data %>%
  left_join(Critical_2021_by_Code, c("ZIPCODE"="zipcode"))

nc_pal= colorNumeric(palette="YlOrBr", domain= spdf_file_2022@data$Total, na.color = 'transparent')


Critical_Violation_Map=
  leaflet()%>%
  addProviderTiles("CartoDB")%>%
  addSearchOSM()%>%
  addReverseSearchOSM()%>%
  #### First Layer of PolyGons
  addPolygons(
    data = spdf_file_2022 ,
    weight = 0.5,
    color = "black",
    stroke=TRUE ,
    opacity = 1 ,
    fillColor = ~nc_pal(Total),
    label = ~paste0 ('Total Critical Violation : ' , Total),
    group = '2022',
    fillOpacity = 0.7,
    highlight = highlightOptions(weight  = 3, color = "red", bringToFront =  T)
  ) %>%
  
  #### Second Layer of PolyGons
  addPolygons(
    data = spdf_file_2021 ,
    weight = 0.5,
    color = "black",
    stroke=TRUE ,
    opacity = 1 ,
    fillColor = ~nc_pal(Total),
    label =~paste0 ('Total Critical Violation : ' , Total),
    group = '2021',
    fillOpacity = 0.7,
    highlight = highlightOptions(weight  = 3, color = "red", bringToFront =  T)
  ) %>%
  
  addPolygons(
    data = spdf_file_2020 ,
    weight = 0.5,
    color = "black",
    stroke=TRUE ,
    opacity = 1 ,
    fillColor = ~nc_pal(Total),
    label = ~paste0 ('Total Critical Violation : ' , Total),
    group = '2020',
    fillOpacity = 0.7,
    highlight = highlightOptions(weight  = 3, color = "red", bringToFront =  T)
  ) %>%
  
  #### Third Layer of PolyGons
  addPolygons(
    data = spdf_file_2019 ,
    weight = 0.5,
    color = "black",
    stroke=TRUE ,
    opacity = 1 ,
    fillColor = ~nc_pal(Total),
    label =~paste0 ('Total Critical Violation : ' , Total),
    group = '2019',
    fillOpacity = 0.7,
    highlight = highlightOptions(weight  = 3, color = "red", bringToFront =  T)
  ) %>%
  
  ##### layer of grades of restaurnts
  
  addMarkers(data = American,lng = ~longitude, lat = ~latitude, 
             
             label = ~htmlEscape(dba),
             group = 'American',
             popup  = paste0("<b>",American$dba,"</b>", 
                             "<br/>", 'Cuisine Type: ', American$cuisine_description, 
                             "<br/>", 'Phone Number: ', American$phone,
                             "<br/>", 'Grade: ', American$grade,
                             "<br/>",'Latest Violation: ', American$violation_description ),
             clusterOptions = markerClusterOptions()
  ) %>% 
  addMarkers(data = Chinese,lng = ~longitude, lat = ~latitude, 
             label = ~htmlEscape(dba),
             group = 'Chinese',
             popup  = paste0("<b>",Chinese$dba,"</b>", 
                             "<br/>", 'Cuisine Type: ', Chinese$cuisine_description, 
                             "<br/>", 'Phone Number: ', Chinese$phone,
                             "<br/>", 'Grade: ', Chinese$grade,
                             "<br/>",'Latest Violation: ', Chinese$violation_description ),
             clusterOptions = markerClusterOptions()
  ) %>% 
  
  addMarkers(data = Pizza,lng = ~longitude, lat = ~latitude, 
             
             label = ~htmlEscape(dba),
             group = 'Pizza',
             popup  = paste0("<b>",Pizza$dba,"</b>", 
                             "<br/>", 'Cuisine Type: ', Pizza$cuisine_description, 
                             "<br/>", 'Phone Number: ', Pizza$phone,
                             "<br/>", 'Grade: ', Pizza$grade,
                             "<br/>",'Latest Violation: ', Pizza$violation_description ),
             clusterOptions = markerClusterOptions()
  ) %>% 
  
  
  addMarkers(data = Mexican,lng = ~longitude, lat = ~latitude, 
             label = ~htmlEscape(dba),
             group = 'Mexican',
             popup  = paste0("<b>",Mexican$dba,"</b>", 
                             "<br/>", 'Cuisine Type: ', Mexican$cuisine_description, 
                             "<br/>", 'Phone Number: ', Mexican$phone,
                             "<br/>", 'Grade: ', Mexican$grade,
                             "<br/>",'Latest Violation: ', Mexican$violation_description ),
             clusterOptions = markerClusterOptions()
  ) %>% 
  
  
  addMarkers(data = Italian,lng = ~longitude, lat = ~latitude, 
             label = ~htmlEscape(dba),
             group = 'Italian',
             popup  = paste0("<b>",Italian$dba,"</b>", 
                             "<br/>", 'Cuisine Type: ', Italian$cuisine_description, 
                             "<br/>", 'Phone Number: ', Italian$phone,
                             "<br/>", 'Grade: ', Italian$grade,
                             "<br/>",'Latest Violation: ', Italian$violation_description ),
             clusterOptions = markerClusterOptions()
  ) %>% 
  addMarkers(data = Coffee,lng = ~longitude, lat = ~latitude, 
             label = ~htmlEscape(dba),
             group = 'Coffee',
             popup  = paste0("<b>",Coffee$dba,"</b>", 
                             "<br/>", 'Cuisine Type: ', Coffee$cuisine_description, 
                             "<br/>", 'Phone Number: ', Coffee$phone,
                             "<br/>", 'Grade: ', Coffee$grade,
                             "<br/>",'Latest Violation: ', Coffee$violation_description ),
             clusterOptions = markerClusterOptions()
  ) %>% 
  addMarkers(data = Others,lng = ~longitude, lat = ~latitude, 
             label = ~htmlEscape(dba),
             group = 'Others',
             popup  = paste0("<b>",Others$dba,"</b>", 
                             "<br/>", 'Cuisine Type: ', Others$cuisine_description, 
                             "<br/>", 'Phone Number: ', Others$phone,
                             "<br/>", 'Grade: ', Others$grade,
                             "<br/>",'Latest Violation: ', Others$violation_description),
             clusterOptions = markerClusterOptions()
  ) %>% 
  
  
  addLayersControl( baseGroups = c("2022", "2021","2020","2019"),overlayGroups = c("American", "Chinese","Coffee","Pizza", "Italian","Mexican", "Others"))%>%
  addLegend( pal=nc_pal, values= spdf_file_2022$Total, opacity=0.9, title = "Critical Violation Counts", position = "bottomleft" )

Critical_Violation_Map
saveRDS(Critical_Violation_Map,file="../lib/critical_cluster.Rda")

###=====total violations========
Total_2019_by_Code = 
  df%>%
  filter(year == 2019 & critical_flag %in% c('Critical', 'Not Critical'))%>%
  group_by(zipcode)%>%
  summarize(Total = n())

Total_2020_by_Code = 
  df%>%
  filter(year == 2020 & critical_flag %in% c('Critical', 'Not Critical'))%>%
  group_by(zipcode)%>%
  summarize(Total = n())

Total_2021_by_Code = 
  df%>%
  filter(year == 2021 & critical_flag %in% c('Critical', 'Not Critical'))%>%
  group_by(zipcode)%>%
  summarize(Total = n())

Total_2022_by_Code = 
  df%>%
  filter(year == 2022 & critical_flag %in% c('Critical', 'Not Critical'))%>%
  group_by(zipcode)%>%
  summarize(Total = n())





##### Join datasets
spdf_file_2022 = spdf_file
spdf_file_2022@data =
  spdf_file_2022@data %>%
  left_join(Total_2022_by_Code, c("ZIPCODE"="zipcode"))



spdf_file_2019 = spdf_file
spdf_file_2019@data =
  spdf_file_2019@data %>%
  left_join(Total_2019_by_Code, c("ZIPCODE"="zipcode"))


spdf_file_2020 = spdf_file
spdf_file_2020@data =
  spdf_file_2020@data %>%
  left_join(Total_2020_by_Code, c("ZIPCODE"="zipcode"))

spdf_file_2021 = spdf_file
spdf_file_2021@data =
  spdf_file_2021@data %>%
  left_join(Total_2021_by_Code, c("ZIPCODE"="zipcode"))






##### colors
nc_pal= colorNumeric(palette="YlOrBr", domain= spdf_file_2022@data$Total, na.color = 'transparent')


Total_Violation_Map=
  
  leaflet()%>%
  addProviderTiles("CartoDB")%>%
  setView(lng= -73.95223 , lat =40.78410	 , zoom = 10)%>%
  addSearchOSM()%>%
  #### First Layer of PolyGons
  addPolygons(
    data = spdf_file_2022 ,
    weight = 0.5,
    color = "black",
    stroke=TRUE ,
    opacity = 1 ,
    fillColor = ~nc_pal(Total),
    label = ~paste0 ('Total Critical Violation : ' , Total),
    group = '2022',
    fillOpacity = 0.7,   
    highlight = highlightOptions(weight  = 3, color = "red", bringToFront =  T)
  ) %>%
  
  #### Second Layer of PolyGons
  addPolygons(
    data = spdf_file_2021 ,
    weight = 0.5,
    color = "black",
    stroke=TRUE ,
    opacity = 1 ,
    fillColor = ~nc_pal(Total),
    label =~paste0 ('Total  Violation : ' , Total),
    group = '2021',
    fillOpacity = 0.7,
    highlight = highlightOptions(weight  = 3, color = "red", bringToFront =  T)
  ) %>%
  addLayersControl(overlayGroups = c("2022", "2021"))%>%
  
  #####Third layer
  addPolygons(
    data = spdf_file_2020 ,
    weight = 0.5,
    color = "black",
    stroke=TRUE ,
    opacity = 1 ,
    fillColor = ~nc_pal(Total),
    fillOpacity = 0.7,
    label =~paste0 ('Total  Violation : ' , Total),
    group = '2020',
    highlight = highlightOptions(weight  = 3, color = "red", bringToFront =  T)
  ) %>%
  
  ####Fourth Layer
  addPolygons(
    data = spdf_file_2019 ,
    weight = 0.5,
    color = "black",
    stroke=TRUE ,
    opacity = 1 ,
    fillColor = ~nc_pal(Total),
    label =~paste0 ('Total  Violation : ' , Total),
    group = '2019',
    fillOpacity = 0.7,
    highlight = highlightOptions(weight  = 3, color = "red", bringToFront =  T)
  ) %>%
  
  
  ##### Fifth layer of grades of restaurants
  
  
  
  addMarkers(data = American,lng = ~longitude, lat = ~latitude, 
             
             label = ~htmlEscape(dba),
             group = 'American',
             popup  = paste0("<b>",American$dba,"</b>", 
                             "<br/>", 'Cuisine Type: ', American$cuisine_description, 
                             "<br/>", 'Phone Number: ', American$phone,
                             "<br/>", 'Grade: ', American$grade,
                             "<br/>",'Latest Violation: ', American$violation_description),
             clusterOptions = markerClusterOptions()
  ) %>% 
  addMarkers(data = Chinese,lng = ~longitude, lat = ~latitude, 
             label = ~htmlEscape(dba),
             group = 'Chinese',
             popup  = paste0("<b>",Chinese$dba,"</b>", 
                             "<br/>", 'Cuisine Type: ', Chinese$cuisine_description, 
                             "<br/>", 'Phone Number: ', Chinese$phone,
                             "<br/>", 'Grade: ', Chinese$grade,
                             "<br/>",'Latest Violation: ', Chinese$violation_description ),
             clusterOptions = markerClusterOptions()
  ) %>% 
  
  addMarkers(data = Pizza,lng = ~longitude, lat = ~latitude, 
             
             label = ~htmlEscape(dba),
             group = 'Pizza',
             popup  = paste0("<b>",Pizza$dba,"</b>", 
                             "<br/>", 'Cuisine Type: ', Pizza$cuisine_description, 
                             "<br/>", 'Phone Number: ', Pizza$phone,
                             "<br/>", 'Grade: ', Pizza$grade,
                             "<br/>",'Latest Violation: ', Pizza$violation_description),
             clusterOptions = markerClusterOptions()
  ) %>% 
  
  
  addMarkers(data = Mexican,lng = ~longitude, lat = ~latitude, 
             label = ~htmlEscape(dba),
             group = 'Mexican',
             popup  = paste0("<b>",Mexican$dba,"</b>", 
                             "<br/>", 'Cuisine Type: ', Mexican$cuisine_description, 
                             "<br/>", 'Phone Number: ', Mexican$phone,
                             "<br/>", 'Grade: ', Mexican$grade,
                             "<br/>",'Latest Violation: ', Mexican$violation_description ),
             clusterOptions = markerClusterOptions()
  ) %>% 
  
  
  addMarkers(data = Italian,lng = ~longitude, lat = ~latitude, 
             label = ~htmlEscape(dba),
             group = 'Italian',
             popup  = paste0("<b>",Italian$dba,"</b>", 
                             "<br/>", 'Cuisine Type: ', Italian$cuisine_description, 
                             "<br/>", 'Phone Number: ', Italian$phone,
                             "<br/>", 'Grade: ', Italian$grade,
                             "<br/>",'Latest Violation: ', Italian$violation_description ),
             clusterOptions = markerClusterOptions()
  ) %>% 
  addMarkers(data = Coffee,lng = ~longitude, lat = ~latitude, 
             label = ~htmlEscape(dba),
             group = 'Coffee',
             popup  = paste0("<b>",Coffee$dba,"</b>", 
                             "<br/>", 'Cuisine Type: ', Coffee$cuisine_description, 
                             "<br/>", 'Phone Number: ', Coffee$phone,
                             "<br/>", 'Grade: ', Coffee$grade,
                             "<br/>",'Latest Violation: ', Coffee$violation_description ),
             clusterOptions = markerClusterOptions()
  ) %>% 
  addMarkers(data = Others,lng = ~longitude, lat = ~latitude, 
             label = ~htmlEscape(dba),
             group = 'Others',
             popup  = paste0("<b>",Others$dba,"</b>", 
                             "<br/>", 'Cuisine Type: ', Others$cuisine_description, 
                             "<br/>", 'Phone Number: ', Others$phone,
                             "<br/>", 'Grade: ', Others$grade,
                             "<br/>",'Latest Violation: ', Others$violation_description ),
             clusterOptions = markerClusterOptions()
  ) %>% 
  
  
  addLayersControl( baseGroups = c("2022", "2021","2020","2019"),overlayGroups = c("American", "Chinese","Coffee","Pizza", "Italian","Mexican", "Others"))%>%
  addLegend( pal=nc_pal, values= spdf_file_2022$Total, opacity=0.9, title = "Total Violation Counts", position = "bottomleft" )

Total_Violation_Map
saveRDS(Total_Violation_Map,file="../lib/total_cluster.Rda")



##=======================Data Preparation for Score Map ===========
df$score <- as.numeric(df$score)
score_total <- df %>%
  filter(!is.na(score))%>%
  group_by(zipcode)%>%
  summarise(mean_score = mean(score))

score_2019 <- df%>%
  filter(!is.na(score)&year==2019)%>%
  group_by(zipcode)%>%
  summarise(mean_score = mean(score))
score_2019$mean_score <- round(score_2019$mean_score,2)

score_2020 <- df%>%
  filter(!is.na(score)&year==2020)%>%
  group_by(zipcode)%>%
  summarise(mean_score = mean(score))
score_2020$mean_score <- round(score_2020$mean_score,2)

score_2021 <- df%>%
  filter(!is.na(score)&year==2021)%>%
  group_by(zipcode)%>%
  summarise(mean_score = mean(score))
score_2021$mean_score <- round(score_2021$mean_score,2)

score_2022 <- df%>%
  filter(!is.na(score)&year==2022)%>%
  group_by(zipcode)%>%
  summarise(mean_score = mean(score))
score_2022$mean_score <- round(score_2022$mean_score,2)


Score_spdf_file_2019 <- spdf_file
Score_spdf_file_2019@data <- Score_spdf_file_2019@data %>%
  left_join(score_2019, c("ZIPCODE" = "zipcode"))

Score_spdf_file_2020 <- spdf_file
Score_spdf_file_2020@data <- Score_spdf_file_2020@data %>%
  left_join(score_2020, c("ZIPCODE" = "zipcode"))

Score_spdf_file_2021 <- spdf_file
Score_spdf_file_2021@data <- Score_spdf_file_2021@data %>%
  left_join(score_2021, c("ZIPCODE" = "zipcode"))

Score_spdf_file_2022 <- spdf_file
Score_spdf_file_2022@data <- Score_spdf_file_2022@data %>%
  left_join(score_2022, c("ZIPCODE" = "zipcode"))

score_map <- list(Score_spdf_file_2019,Score_spdf_file_2020,Score_spdf_file_2021,Score_spdf_file_2022)
names(score_map) <- c("2019","2020","2021","2022")

#save processed score data
saveRDS(score_map,file = "../data/score_map.Rda")









