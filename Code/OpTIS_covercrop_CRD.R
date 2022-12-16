# This scripts calculates percentage of cover crop for whole state of IA

library(readxl)
library(tidyverse)
library(googledrive)


# read all cover crop data
data <- read_csv('Input_Data/OpTIS/IA/CoverCrop_Map_Full_Data_data_IA_2005-2018.csv')

names(data) <- names(data) %>% str_replace_all(' ', '_')


# calculate percentages of cover crop each year
data %>%
  filter(Winter_Cover == 'Cover Crop') %>%
  select(Year, Winter_Cover, Crd_Name, Crop, Area_Acres, LOD_Map_Acres, Pct_of_Crop_Acres) %>%
  mutate(Area_Acres = parse_number(Area_Acres),
         LOD_Map_Acres = parse_number(LOD_Map_Acres),
         Pct_of_Crop_Acres = parse_number(Pct_of_Crop_Acres)) %>%
  group_by(Year, Winter_Cover, Crd_Name) %>%
  summarise(Area_Acres = sum(Area_Acres),
            LOD_Map_Acres = mean(LOD_Map_Acres),
            Area_Percent = sum(Pct_of_Crop_Acres)) %>%
  mutate(Area_Acres_est = LOD_Map_Acres * Area_Percent / 100) %>%
  group_by(Year, Winter_Cover) %>%
  summarise(Area_Acres = sum(Area_Acres),
            Area_Acres_est = sum(Area_Acres_est),
            LOD_Map_Acres = sum(LOD_Map_Acres)) %>%
  # group_by(Year) %>%
  mutate(Area_Percent = Area_Acres/LOD_Map_Acres * 100,
         Area_Percent_est = Area_Acres_est/LOD_Map_Acres * 100) %>%
  ungroup() ->
  covercrop_percent

# calculated percentages matches with those from the OpTIS websites (compared with summary values)
# but for simplicity of the method use the ones that comes from the website
covercrop_percent %>%
  mutate(State = "IA",
         Cover_Crop_Acres = Area_Acres * 10^6,
         Cover_Crop_Acres_estimated = Area_Acres_est * 10^6,
         Row_Crop_Acres = LOD_Map_Acres * 10^6,
         Cover_Crop_Percent = round(Area_Percent, 2),
         Cover_Crop_Percent_estimated = round(Area_Percent_est, 2)) %>%
  select(State, Year, Winter_Cover, Cover_Crop_Acres, Cover_Crop_Acres_estimated,
         Row_Crop_Acres, Cover_Crop_Percent, Cover_Crop_Percent_estimated) %>%
  write_csv('Output_Data/OpTIS_covercrop_STATE.csv')


