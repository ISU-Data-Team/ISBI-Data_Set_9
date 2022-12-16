# This scripts calculates percentage of tillage types for whole state of IA

library(readxl)
library(tidyverse)
library(googledrive)


# read all tillage data
data <- read_csv('Input_Data/OpTIS/IA/Tillage_Map_Full_Data_data_IA_2005-2018.csv')

names(data) <- names(data) %>% str_replace_all(' ', '_')


# calculate percentages of tillage each year
data %>%
  mutate(Area_Acres = parse_number(Area_Acres),
         LOD_Map_Acres = parse_number(LOD_Map_Acres),
         Pct_of_Row_Crop_Acres = parse_number(Pct_of_Row_Crop_Acres)) %>%
  group_by(Year, Tillage_Method, Crd_Name) %>%
  summarise(Area_Acres = sum(Area_Acres),
            LOD_Map_Acres = mean(LOD_Map_Acres),
            Area_Percent = sum(Pct_of_Row_Crop_Acres)) %>%
  mutate(Area_Acres_est = LOD_Map_Acres * Area_Percent / 100) %>%
  group_by(Year, Tillage_Method) %>%
  summarise(Area_Acres = sum(Area_Acres),
            Area_Acres_est = sum(Area_Acres_est),
            LOD_Map_Acres = sum(LOD_Map_Acres)) %>%
  group_by(Year) %>%
  mutate(Area_Percent = Area_Acres/LOD_Map_Acres * 100,
         Area_Percent_est = Area_Acres_est/LOD_Map_Acres * 100) %>%
  ungroup() ->
  tillage_percent


# calculated percentages matches with those from the OpTIS websites (compared with summary values)
# but for simplicity of the method use the ones that comes from the website
tillage_percent %>%
  mutate(State = "IA",
         Tillage_Acres = Area_Acres * 10^6,
         Tillage_Acres_estimated = Area_Acres_est * 10^6,
         Row_Crop_Acres = LOD_Map_Acres * 10^6,
         Tillage_Percent = round(Area_Percent, 2),
         Tillage_Percent_estimated = round(Area_Percent_est, 2)) %>%
  select(State, Year, Tillage_Method, Tillage_Acres, Tillage_Acres_estimated,
         Row_Crop_Acres, Tillage_Percent, Tillage_Percent_estimated) %>%
  write_csv('Output_Data/OpTIS_tillage_STATE.csv')


