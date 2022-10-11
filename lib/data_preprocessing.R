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
####=====critical violations #####
# Number of restaurants per zipcode
Num_Rest_Code <- df %>%
  group_by(zipcode, dba, latitude, longitude) %>%
  count() %>%
  group_by(zipcode) %>%
  count()

Critical_2019_by_Code <- df %>%
  filter(year == 2019) %>%
  group_by(zipcode) %>%
  summarize(Total = n())

Critical_2020_by_Code <- df %>%
  filter(year == 2020) %>%
  group_by(zipcode) %>%
  summarize(Total = n())

Critical_2021_by_Code <- df %>%
  filter(year == 2021) %>%
  group_by(zipcode) %>%
  summarize(Total = n())

Critical_2022_by_Code <- df %>%
  filter(year == 2022) %>%
  group_by(zipcode) %>%
  summarize(Total = n())

Critical_spdf_file_2022 <- spdf_file
Critical_spdf_file_2022@data <- Critical_spdf_file_2022@data %>%
  left_join(Critical_2022_by_Code, c("ZIPCODE" = "zipcode"))

Critical_spdf_file_2019 <- spdf_file
Critical_spdf_file_2019@data <- Critical_spdf_file_2019@data %>%
  left_join(Critical_2019_by_Code, c("ZIPCODE" = "zipcode"))

Critical_spdf_file_2020 <- spdf_file
Critical_spdf_file_2020@data <- Critical_spdf_file_2020@data %>%
  left_join(Critical_2020_by_Code, c("ZIPCODE" = "zipcode"))

Critical_spdf_file_2021 <- spdf_file
Critical_spdf_file_2021@data <- Critical_spdf_file_2021@data %>%
  left_join(Critical_2021_by_Code, c("ZIPCODE" = "zipcode"))

critical_violations <- list(Critical_spdf_file_2019, Critical_spdf_file_2020, Critical_spdf_file_2021, Critical_spdf_file_2022)
names(critical_violations) <- c("2019", "2020", "2021", "2022")



###=====total violations========
Total_violation <- df %>%
  group_by(zipcode) %>%
  summarize(Total = n())

Total_2019_by_Code <- df %>%
  filter(year == 2019) %>%
  group_by(zipcode) %>%
  summarize(Total = n())

Total_2020_by_Code <- df %>%
  filter(year == 2020) %>%
  group_by(zipcode) %>%
  summarize(Total = n())

Total_2021_by_Code <- df %>%
  filter(year == 2021) %>%
  group_by(zipcode) %>%
  summarize(Total = n())

Total_2022_by_Code <- df %>%
  filter(year == 2022) %>%
  group_by(zipcode) %>%
  summarize(Total = n())

Total_spdf_file_2022 <- spdf_file
Total_spdf_file_2022@data <- Total_spdf_file_2022@data %>%
  left_join(Total_2022_by_Code, c("ZIPCODE" = "zipcode"))

Total_spdf_file_2019 <- spdf_file
Total_spdf_file_2019@data <- Total_spdf_file_2019@data %>%
  left_join(Total_2019_by_Code, c("ZIPCODE" = "zipcode"))

Total_spdf_file_2020 <- spdf_file
Total_spdf_file_2020@data <- Total_spdf_file_2020@data %>%
  left_join(Total_2020_by_Code, c("ZIPCODE" = "zipcode"))

Total_spdf_file_2021 <- spdf_file
Total_spdf_file_2021@data <- Total_spdf_file_2021@data %>%
  left_join(Total_2021_by_Code, c("ZIPCODE" = "zipcode"))

total_violation <- list(Total_spdf_file_2019, Total_spdf_file_2020, Total_spdf_file_2021, Total_spdf_file_2022)
names(total_violation) <- c("2019", "2020", "2021", "2022")

##combine total and critical for the violation map
violations <- list(total_violation,critical_violations)
names(violations) <- c("Number of Total Violations","Number of Crital Violations")
#save processed data for violation map
saveRDS(violations,file="../data/violations.Rda")

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









