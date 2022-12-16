# Combine cover crop and tillage data and upload to Google Drive
# This script can be run after OpTIS_covercrop.R and OpTIS_tillage.R

library(readxl)
library(tidyverse)
library(googledrive)



# HUC8 scale --------------------------------------------------------------

# read processed data
covercrop <- read_csv('Output_Data/OpTIS_covercrop.csv')
tillage <- read_csv('Output_Data/OpTIS_tillage.csv')

# transformed the data
tillage %>%
  select(ID_HUC8:Tillage_Acres) %>%
  mutate(cols = paste('Acres', word(Tillage_Method, 1, 2)),
         cols = str_replace_all(cols, ' ', "_")) %>%
  select(-Tillage_Method) %>%
  spread(cols, Tillage_Acres) -> tillage_transformed

covercrop %>%
  select(ID_HUC8:Crop, Acres_Cover_Crop = Cover_Crop_Acres) %>%
  full_join(tillage_transformed) %>%
  arrange(ID_HUC8, Year, Crop) %>%
  write_csv('Output_Data/OpTIS_all_HUC8.csv')


# this is to compare with cost share numbers
cover_crop_HUC8_OpTIS <-
  read_csv('Output_Data/OpTIS_covercrop.csv') %>%
  group_by(ID_HUC8, Year) %>%
  summarise(Cover_Crop_HUC8_OpTIS = sum(Cover_Crop_Acres))

cover_crop_HUC8_costshare <- 
  read_csv('../Data_Set_18/Output_Data/DN_18.csv') %>%
  filter(str_detect(practiceName, 'Cover Crop')) %>%
  mutate(ID_HUC8 = str_sub(huc12Code, 1, 8)) %>%
  group_by(ID_HUC8, Year = appliedYear) %>%
  summarise(Cover_Crop_HUC8_Cost_Share = sum(appliedAmountTotal))

cover_crop_HUC8_OpTIS %>%
  left_join(cover_crop_HUC8_costshare) %>%
  # data from 2018 are available for all 8 watersheds
  filter(Year %in% 2012:2017) %>%
  ungroup() %>%
  ggplot(aes(Cover_Crop_HUC8_OpTIS, Cover_Crop_HUC8_Cost_Share)) +
  geom_point(aes(col = ID_HUC8))



# State scale -------------------------------------------------------------

# Croped area in IA used by OpTIS
Crop_Acres_IA <- 24140000

# read OpTIS summary for cover crop percentages
read_csv('Input_Data/OpTIS/IA/CoverCrop_Map_Summary_Data_data_IA_2005-2018.csv') %>%
  set_names('Year', 'Winter_Cover', 'Cover_Crop_Percent') %>%
  mutate(Cover_Crop_Percent = parse_number(Cover_Crop_Percent),
         Acres_Cover_Crop = Crop_Acres_IA * Cover_Crop_Percent / 100) %>%
  filter(Winter_Cover == 'Cover Crop') -> covercrop_state

# read OpTIS summary for tillage percentages
read_csv('Input_Data/OpTIS/IA/Tillage_Map_Summary_Data_data_IA_2005-2018.csv') %>%
  set_names('Year', 'Tillage_Method', 'Tillage_Percent') %>%
  mutate(Tillage_Percent = parse_number(Tillage_Percent),
         Acres_Tillage = Crop_Acres_IA * Tillage_Percent / 100) %>%
  # combine Tillage categories
  mutate(Tillage_Method = ifelse(str_detect(Tillage_Method, '50%'), 
                                 'Conservation Tillage (>30% Residue)', 
                                 Tillage_Method),
         Tillage_Method = factor(Tillage_Method, levels = c('Conventional Tillage (<15% Residue)',
                                                            'Reduced Tillage (15-30% Residue)',
                                                            'Conservation Tillage (>30% Residue)'))) %>%
  group_by(Year, Tillage_Method) %>%
  summarise(Acres_Tillage = sum(Acres_Tillage),
            Tillage_Percent = sum(Tillage_Percent)) %>%
  ungroup() ->
  tillage_state

tillage_state %>%
  mutate(cols = paste('Acres', word(Tillage_Method, 1, 2)),
         cols = str_replace_all(cols, ' ', "_")) %>%
  select(-Tillage_Method, -Tillage_Percent) %>%
  spread(cols, Acres_Tillage) %>%
  full_join(covercrop_state) %>%
  select(Year, Acres_Cover_Crop, Acres_Conventional_Tillage, Acres_Reduced_Tillage, Acres_Conservation_Tillage) %>%
  ungroup() -> data_state


data_state %>%
  write_csv('Output_Data/OpTIS_all_STATE.csv')


# upload data to ISBI google drive
drive_upload('Output_Data/OpTIS_all_STATE.csv',
             as_id('1y-2WDLa0LIZAceFbC0JjodoEPTg1NBum'),
             overwrite = TRUE) 

