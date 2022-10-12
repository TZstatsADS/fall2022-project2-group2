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
saveRDS(borough_list,file="../output/borough_list.Rda")
saveRDS(cuisine_list,file="../output/cuisine_list.Rda")
saveRDS(df,file = "../output/df.Rda")
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
saveRDS(American,file="../output/American.Rda")

Chinese= 
  grades%>%filter(CUISINE.TYPE == "Chinese")
saveRDS(Chinese,file="../output/Chinese.Rda")

Coffee = 
  grades%>%filter(CUISINE.TYPE == "Coffee")
saveRDS(Coffee,file="../output/Coffee.Rda")

Italian = 
  grades%>%filter(CUISINE.TYPE == "Italian")
saveRDS(Italian,file="../output/Italian.Rda")

Pizza = 
  grades%>%filter(CUISINE.TYPE == "Pizza")
saveRDS(Pizza,file="../output/Pizza.Rda")

Mexican = 
  grades%>%filter(CUISINE.TYPE == "Mexican")
saveRDS(Mexican,file="../output/Mexican.Rda")

Others = 
  grades%>%filter(CUISINE.TYPE == "Others")
saveRDS(Others,file="../output/Others.Rda")

#### Number of restaurant per ZIPCODE
Num_Rest_Code =
  df%>%
  group_by(zipcode, dba, latitude, longitude)%>%
  count() %>%
  group_by(zipcode)%>%
  count()

Critical_2019_by_Code = 
  df%>%
  filter(year == 2019&critical_flag == "Critical")%>%
  group_by(zipcode)%>%
  summarize(Total = n())

Critical_2020_by_Code = 
  df%>%
  filter(year == 2020&critical_flag == "Critical")%>%
  group_by(zipcode)%>%
  summarize(Total = n())

Critical_2021_by_Code = 
  df%>%
  filter(year == 2021&critical_flag == "Critical")%>%
  group_by(zipcode)%>%
  summarize(Total = n())

Critical_2022_by_Code = 
  df%>%
  filter(year == 2022&critical_flag == "Critical")%>%
  group_by(zipcode)%>%
  summarize(Total = n())


Critical_spdf_file_2022 = spdf_file
Critical_spdf_file_2022@data =
  Critical_spdf_file_2022@data %>%
  left_join(Critical_2022_by_Code, c("ZIPCODE"="zipcode"))
saveRDS(Critical_spdf_file_2022,file="../output/critical_2022.Rda")



Critical_spdf_file_2019 = spdf_file
Critical_spdf_file_2019@data =
  Critical_spdf_file_2019@data %>%
  left_join(Critical_2019_by_Code, c("ZIPCODE"="zipcode"))

saveRDS(Critical_spdf_file_2019,file="../output/critical_2019.Rda")

Critical_spdf_file_2020 = spdf_file
Critical_spdf_file_2020@data =
  Critical_spdf_file_2020@data %>%
  left_join(Critical_2020_by_Code, c("ZIPCODE"="zipcode"))
saveRDS(Critical_spdf_file_2020,file="../output/critical_2020.Rda")

Critical_spdf_file_2021 = spdf_file
Critical_spdf_file_2021@data =
  Critical_spdf_file_2021@data %>%
  left_join(Critical_2021_by_Code, c("ZIPCODE"="zipcode"))
saveRDS(Critical_spdf_file_2021,file="../output/critical_2021.Rda")

critical_violations <- list(Critical_spdf_file_2019, Critical_spdf_file_2020, Critical_spdf_file_2021, Critical_spdf_file_2022)
names(critical_violations) <- c("2019", "2020", "2021", "2022")


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
Total_spdf_file_2022 = spdf_file
Total_spdf_file_2022@data =
  Total_spdf_file_2022@data %>%
  left_join(Total_2022_by_Code, c("ZIPCODE"="zipcode"))
saveRDS(Total_spdf_file_2022,file="../output/total_2022.Rda")


Total_spdf_file_2019 = spdf_file
Total_spdf_file_2019@data =
  Total_spdf_file_2019@data %>%
  left_join(Total_2019_by_Code, c("ZIPCODE"="zipcode"))
saveRDS(Total_spdf_file_2019,file="../output/total_2019.Rda")

Total_spdf_file_2020 = spdf_file
Total_spdf_file_2020@data =
  Total_spdf_file_2020@data %>%
  left_join(Total_2020_by_Code, c("ZIPCODE"="zipcode"))
saveRDS(Total_spdf_file_2020,file="../output/total_2020.Rda")

Total_spdf_file_2021 = spdf_file
Total_spdf_file_2021@data =
  Total_spdf_file_2021@data %>%
  left_join(Total_2021_by_Code, c("ZIPCODE"="zipcode"))
saveRDS(Total_spdf_file_2021,file="../output/total_2021.Rda")


total_violation <- list(Total_spdf_file_2019, Total_spdf_file_2020, Total_spdf_file_2021, Total_spdf_file_2022)
names(total_violation) <- c("2019", "2020", "2021", "2022")



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

comparison <- list(critical_violations,total_violation,score_map)
names(comparison) <-  c("Number of Total Violations", "Number of Critical Violations","Mean Score")
violations <- list(total_violation,critical_violations)
names(violations) <- c("Number of Total Violations","Number of Critical Violations")

#save processed score data
saveRDS(comparison,file = "../output/comparison.Rda")
saveRDS(score_map,file="../output/score_map.Rda")
saveRDS(violations,file = "../output/violations.Rda")








