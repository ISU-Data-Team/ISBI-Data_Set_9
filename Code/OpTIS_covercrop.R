library(readxl)
library(tidyverse)
library(googledrive)


# list all files with cover crop data
files_covercrop <- list.files('Input_Data/OpTIS/', 
                            pattern = 'CoverCrop_Full', 
                            full.names = TRUE)


# read all cover crop data
data <- vector('list', length(files_covercrop))

for (i in seq_along(files_covercrop)) {
  COL_NAMES <- read_csv(files_covercrop[i], n_max = 1) %>%
    names() %>%
    str_replace_all(' ', '_')
  data[[i]] <- read_csv(files_covercrop[i],
                        col_names = COL_NAMES,
                        skip = 1)
}

# combine and select data of interest
data %>%
  map(~ .x %>% mutate(Huc8 = as.character(Huc8))) %>%
  bind_rows() %>% 
  select(Huc8, Huc8_Name, Crop, Winter_Cover, Year, 
         # NOTE: LOD_Map_Acres is the total crop-land acres within watershed
         Area_Acres, LOD_Map_Acres, Pct_of_Row_Crop_Acres) %>%  
  # choose only cover crop data
  filter(Winter_Cover == 'Cover Crop') %>%
  # combine Small Grain with Other
  mutate(Crop = ifelse(Crop == 'Small Grain', 'Other', Crop),
         LOD_Map_Acres = parse_number(LOD_Map_Acres)*10^6,
         Pct_of_Row_Crop_Acres = parse_number(Pct_of_Row_Crop_Acres)) %>%
  group_by(Huc8, Crop, Winter_Cover, Year) %>%
  summarise(Area_Acres = sum(Area_Acres),
            LOD_Map_Acres = mean(LOD_Map_Acres),
            Pct_of_Row_Crop_Acres = sum(Pct_of_Row_Crop_Acres)) %>%
  mutate(Pct_of_Row_Crop_Acres_estimated = Area_Acres/LOD_Map_Acres*100) %>%
  arrange(Huc8, Year, Winter_Cover) %>% 
  ungroup() -> 
  data_covercrop


# save data
data_covercrop %>%
  select(ID_HUC8 = Huc8, Year, Crop, 
         Cover_Crop_Acres = Area_Acres,
         Row_Crop_Acres = LOD_Map_Acres,
         Cover_Crop_Percent = Pct_of_Row_Crop_Acres_estimated) %>%
  mutate(Cover_Crop_Percent = round(Cover_Crop_Percent, 2)) %>%
  write_csv('Output_Data/OpTIS_covercrop.csv')


