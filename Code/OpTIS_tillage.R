library(readxl)
library(tidyverse)
library(googledrive)


# list all files with tillage data
files_tillage <- list.files('Input_Data/OpTIS/', 
                            pattern = 'Tillage_Full', 
                            full.names = TRUE)


# read all tillage data
data <- vector('list', length(files_tillage))

for (i in seq_along(files_tillage)) {
  COL_NAMES <- read_csv(files_tillage[i], n_max = 1) %>%
    names() %>%
    str_replace_all(' ', '_')
  data[[i]] <- read_csv(files_tillage[i],
                        col_names = COL_NAMES,
                        skip = 1)
}

# combine and select data of interest
data %>%
  map(~ .x %>% mutate(Huc8 = as.character(Huc8))) %>%
  bind_rows() %>% 
  select(Huc8, Huc8_Name, Crop, Tillage_Method, Year, Area_Acres,
         LOD_Map_Acres, Pct_of_Row_Crop_Acres) %>%
  # combine Small Grain with Other
  mutate(Crop = ifelse(Crop == 'Small Grain', 'Other', Crop),
         LOD_Map_Acres = parse_number(LOD_Map_Acres)*10^6,
         Pct_of_Row_Crop_Acres = parse_number(Pct_of_Row_Crop_Acres)) %>%
  group_by(Huc8, Crop, Tillage_Method, Year) %>%
  summarise(Area_Acres = sum(Area_Acres),
            LOD_Map_Acres = mean(LOD_Map_Acres),
            Pct_of_Row_Crop_Acres = sum(Pct_of_Row_Crop_Acres)) %>%
  ungroup() -> 
  data_inter

# save the original tillage categories in Google Drive
data_inter %>%
  select(HUC8 = Huc8, Crop:Area_Acres, Pct_of_Row_Crop_Acres) %>%
  googlesheets4::write_sheet(
    data = .,
    ss = googlesheets4::as_sheets_id('1VICC8-EJTwNmqAyMJ2huThXXAt4siNTnyW7OgmZ5z8s'),
    sheet = 'HUC8_Original_Tillage'
    )

data_inter %>%
  # combine Tillage categories
  mutate(Tillage_Method = ifelse(str_detect(Tillage_Method, '50%'), 
                                 'Conservation Tillage (>30% Residue)', 
                                 Tillage_Method),
         Tillage_Method = factor(Tillage_Method, levels = c('Conventional Tillage (<15% Residue)',
                                                            'Reduced Tillage (15-30% Residue)',
                                                            'Conservation Tillage (>30% Residue)'))) %>%
  group_by(Huc8, Crop, Tillage_Method, Year) %>%
  summarise(Area_Acres = sum(Area_Acres),
            LOD_Map_Acres = mean(LOD_Map_Acres),
            Pct_of_Row_Crop_Acres = sum(Pct_of_Row_Crop_Acres)) %>%
  mutate(Pct_of_Row_Crop_Acres_estimated = Area_Acres/LOD_Map_Acres*100) %>%
  arrange(Huc8, Year, Tillage_Method, Crop) %>%
  ungroup() -> 
  data_tillage


# save data
data_tillage %>%
  select(ID_HUC8 = Huc8, Year, Tillage_Method, Crop, 
         Tillage_Acres = Area_Acres,
         Row_Crop_Acres = LOD_Map_Acres,
         Tillage_Percent = Pct_of_Row_Crop_Acres,
         Tillage_Percent_estimated = Pct_of_Row_Crop_Acres_estimated) %>%
  mutate(Tillage_Percent_estimated = round(Tillage_Percent_estimated, 2)) %>%
  write_csv('Output_Data/OpTIS_tillage.csv')



# Calculate percentage of tillage for model input
data_inter %>%
  filter(Crop %in% c('Corn', 'Bean')) %>%
  group_by(Huc8, Tillage_Method, Year) %>%
  summarise(Area_Acres = sum(Area_Acres),
            LOD_Map_Acres = mean(LOD_Map_Acres),
            Pct_of_Row_Crop_Acres = sum(Pct_of_Row_Crop_Acres)) %>%
  mutate(Pct_of_Row_Crop_Acres_estimated = Area_Acres/LOD_Map_Acres*100) %>%
  filter(str_detect(Tillage_Method, '50')) %>%
  # mutate(Tillage_Method = ifelse())
  arrange(Huc8, Year, Tillage_Method) %>%
  ungroup()
