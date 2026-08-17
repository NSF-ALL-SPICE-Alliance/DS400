library(here)
library(janitor)
library(readr)
library(tidyverse)
library(tidycensus)
library(data.table)
library(sf)


# loading wildfire data
thisfile = here("datasets/mapdataall.csv")

cal_fires = read_csv(thisfile) %>% clean_names() %>% 
  filter(incident_county != 'NA') %>% 
  filter(incident_acres_burned > 0) %>%
  dplyr::select(incident_id, incident_county, incident_acres_burned)


# loading county data and making values numeric 
thisfile = here("datasets/List_of_counties_in_California_1.csv")

cal_counties = read_csv(thisfile) %>% clean_names() 

cal_counties <- cal_counties %>% separate(area, c('new_area', 'ignore'), sep = 'sq')

cal_counties$new_area <- gsub(',', '', cal_counties$new_area)
cal_counties$county <- gsub(' County', '', cal_counties$county)
cal_counties <- cal_counties %>% 
  mutate(new_area = str_trim(new_area), 
         num_area = as.numeric(new_area),
         acres = num_area * 640, 
         pop_dens = population_2022/acres) %>% #population density in people/acre
  dplyr::select(county, fips_code, acres, pop_dens)


# adding the fips code to the fires dataset based on county name
fips <- cal_fires %>%
  rownames_to_column() %>%
  mutate(incident_county = strsplit(incident_county, ", ")) %>%
  unnest(incident_county) %>%
  left_join(cal_counties, by = c("incident_county" = "county")) %>%
  group_by(rowname) %>%
  summarise(fips_code = paste(fips_code, collapse = ", ")) %>%
  mutate(rowname = as.numeric(rowname)) %>%
  arrange(rowname)

cal_fips_fires <- cal_fires %>%
  mutate(fips["fips_code"])


# Splitting up rows by county for fires that were in multiple counties. 
# and adding the correct fips_code
cal_fips_fires2 <- cal_fips_fires %>% separate_rows(fips_code)


# Merging fire data with county data by county fips code.
cal_county_fires <- left_join(cal_fips_fires2, cal_counties, by = c("fips_code")) %>%
  dplyr::select(-incident_county)


# Adding a column for total acres of all counties that each fire took place in, then using that # to create a column with an estimate of acres burned = (county_acres / sum_county_acres) 
# acres_burned

cal_county_fires2 <- cal_county_fires %>%
  group_by(incident_id) %>%
  mutate(total_acres = sum(acres))

cal_county_fires2 <- cal_county_fires2 %>% mutate(acres_burned_county = (acres / total_acres) * (incident_acres_burned)) %>%
  dplyr::select(-total_acres) 


# filtering out NAs
cal_county_fires3 <- cal_county_fires2 %>% drop_na()




# creating percentage of the acres burned to make the data readable and adding 
# the county name to the data
cal_county_fires4 <- cal_county_fires3 %>%
  mutate(fire_dens = (acres_burned_county / acres) * 100,
         fourth_root_fire_dens = fire_dens ^ (1 / 4),
         log_fire_dens = log(fire_dens)  )

cal_county_fires4 <-
  left_join(cal_county_fires4, cal_counties[, c("fips_code")], by = "fips_code")


write.csv(cal_county_fires4, "./datasets/anova_data.csv")

# creating a new dataset that is grouped by county to find the mean percentage 
# area burned for each county per fire.
county_means <- cal_county_fires4 %>% 
  group_by(county) %>% 
  summarize(fire_dens = mean(fire_dens), 
            fourth_root_fire_dens = mean(fourth_root_fire_dens),
            log_fire_dens = mean(log_fire_dens),
            pop_dens = mean(pop_dens),
            fips_code = max(fips_code))

county_means <- county_means %>% mutate(GEOID = paste0('06', fips_code))






# Reading and labeling max_temperature data. Filtering out placeholder values of -99.9
max_temperatures <- read_table("datasets/climdiv-tmaxcy-v1.0.0-20231106.txt", 
                               col_names = FALSE) %>% 
  dplyr::select(-"X14") %>%
  mutate(
    state_code = substr(X1, 1, 2),
    county_fips = substr(X1, 3, 5),
    element = substr(X1, 6, 7),
    year = substr(X1, 8, 11)) %>%
  rename(
    "jan" = "X2",
    "feb" = "X3",
    "mar" = "X4",
    "apr" = "X5",
    "may" = "X6",
    "jun" = "X7",
    "jul" = "X8",
    "aug" = "X9",
    "sep" = "X10",
    "oct" = "X11",
    "nov" = "X12",
    "dec" = "X13"
  ) %>%
  dplyr::select(-"X1") %>%
  pivot_longer(cols = jan:dec,
               names_to = "month",
               values_to = "temperature") %>%
  filter(temperature != -99.9)



# Scraping and cleaning NOAA's state code data.
county_readme <- read_table("https://www.ncei.noaa.gov/pub/data/cirs/climdiv/county-readme.txt", 
                            col_names = FALSE) %>%
  dplyr::select(X1, X2, X3, X4, X5) %>%
  slice(-(1:61)) %>%
  slice(-(28:70)) %>%
  mutate(X4 = if_else(is.na(X5), X4, paste0(X4, " ", X5))) %>%
  mutate(X2 = if_else(!is.na(X3) & nchar(X3) > 2, paste0(X2, " ", X3), X2)) %>%
  dplyr::select(-X5)

temp <- county_readme %>%
  dplyr::select(X3, X4) %>%
  slice(1:22) %>%
  rename("X1" = "X3",
         "X2" = "X4") 

county_readme <- county_readme %>%
  dplyr::select(-X3, -X4)

county_readme <- rbind(county_readme, temp)


# merging max_temperatures data with NOAAs county data to add a column for state name. 
# Dropping the incorrect state code.

max_temperatures_state <- merge(max_temperatures, county_readme, by.x = "state_code", by.y = "X1") %>% 
  rename(state_name = X2) %>%
  dplyr::select(-state_code)


# getting correct state codes
correct_state_codes <- get_acs(geography = "county", variables = c("B01003_001E"), geometry = FALSE) %>%
  separate(NAME, into = c("county", "state"), sep = ", ", extra = "merge") %>%
  spread(key = variable, value = estimate) %>%
  mutate(state_code = substr(GEOID, 1, 2)) %>%
  dplyr::select(state, state_code) %>%
  distinct()


# merging max_temperatures_state with the correct state codes to add a column 
# with the correct state code.

max_temperatures_correct <- merge(max_temperatures_state, correct_state_codes, by.x = "state_name", by.y = "state") %>%
  mutate(GEOID = paste0(state_code, county_fips))


# filtering to focus on california
cal_max_temps <- max_temperatures_correct %>% filter(state_code == "06")


# Reading and labeling average precipitation data. Filtering out placeholder values of -9.99
avg_precip <- read_table("datasets/climdiv-pcpncy-v1.0.0-20231106.txt", 
                         col_names = FALSE) %>% 
  dplyr::select(-"X14") %>%
  mutate(
    state_code = substr(X1, 1, 2),
    county_fips = substr(X1, 3, 5),
    element = substr(X1, 6, 7),
    year = substr(X1, 8, 11)) %>%
  rename(
    "jan" = "X2",
    "feb" = "X3",
    "mar" = "X4",
    "apr" = "X5",
    "may" = "X6",
    "jun" = "X7",
    "jul" = "X8",
    "aug" = "X9",
    "sep" = "X10",
    "oct" = "X11",
    "nov" = "X12",
    "dec" = "X13"
  ) %>%
  dplyr::select(-"X1") %>%
  pivot_longer(cols = jan:dec,
               names_to = "month",
               values_to = "precipitation") %>%
  filter(precipitation != -9.99)


# merging average precipitation data with NOAAs county data to add a column for state name. 
# Dropping the incorrect state code.
avg_precip_state <- merge(avg_precip, county_readme, by.x = "state_code", by.y = "X1") %>% 
  rename(state_name = X2) %>%
  dplyr::select(-state_code)


# merging max_temperatures_state with the correct state codes to add a 
# column with the correct state code.
avg_precip_correct <- merge(avg_precip_state, correct_state_codes, by.x = "state_name", by.y = "state") %>%
  mutate(GEOID = paste0(state_code, county_fips))


# filtering to focus on california
cal_avg_precip <- avg_precip_correct %>% filter(state_code == "06")


# Combining temperature and precipitation data
cal_temp_precip <- cbind(cal_max_temps, cal_avg_precip$precipitation) %>% 
  rename("precipitation" = "cal_avg_precip$precipitation") %>% 
  mutate(ID = row_number())


# Creating a new dataframe using the average temperature and precipitation of the top ten 
# highest

top_temps <- cal_temp_precip %>%
  group_by(GEOID) %>%
  top_n(n = 10, wt = temperature) %>%
  summarise(avg_temperature_topten = mean(temperature))

top_precip <- cal_temp_precip %>%
  group_by(GEOID) %>%
  top_n(n = 10, wt = precipitation) %>%
  summarise(avg_precipitation_topten = mean(precipitation))

top_precip_and_temp <- cbind(top_temps, top_precip$avg_precipitation_topten) %>%
  rename("avg_precipitation_topten" = "top_precip$avg_precipitation_topten") 


# Loading and combining individual county emissions data
zip_file_path <- "./datasets/emissions_by_county.zip"

# Unzip the file to a temporary directory
temp_dir <- tempdir()
csv_files_list <- unzip(zip_file_path, temp_dir, list = TRUE)$Name

# Empty data frame
combined_data <- data.table()

# Loop through the .csv files, read, and combine them
for (csv_file in csv_files_list) {
  
  file_path <- unz(zip_file_path, csv_file)
  data <- read.csv(file_path)
  
  data2 <- data %>%
    filter(EICSUMN != "WILDFIRES") %>%
    group_by(AREA) %>%
    summarise(TOG = sum(TOG), ROG = sum(ROG), COT = sum(COT),
              NOX = sum(NOX), SOX = sum(SOX), PM = sum(PM),
              PM10 = sum(PM10), PM2_5 = sum(PM2_5)) %>%
    mutate(fips_code = as.character(csv_file))
  
  combined_data <- rbind(combined_data, data2)
}

new_emissions_data <- combined_data %>%
  mutate(fips_code = gsub("emissions_by_county/emseic", "", fips_code),
         fips_code = gsub(".csv", "", fips_code)) %>%
  dplyr::select(-AREA)


# combining emission and fire data
combined_df1 <- left_join(county_means, new_emissions_data, by = c("fips_code"))


# reading and cleaning elevation data
thisfile = here("datasets/elevation_cal.csv")

elevation = read_csv(thisfile) %>% 
  mutate(fips_code = as.character(fips_code),
         fips_code = if_else(nchar(fips_code) == 1, paste0("00", fips_code), fips_code),
         fips_code = if_else(nchar(fips_code) == 2, paste0("0", fips_code), fips_code),
         elevation_variation = (max_elevation - min_elevation)/acres_area,
         GEOID = paste0("06", fips_code)) %>%
  dplyr::select(GEOID, elevation_variation, avg_elevation)


# Incorporating elevation into dataset
combined_df2 <- left_join(combined_df1, elevation, by = c("GEOID"))


# Incorporating temperature and precipitation into data set
combined_df3 <- left_join(combined_df2, top_precip_and_temp, by = c("GEOID"))


# Reading spatial data for counties and filtering for california.
us_counties <- st_read("https://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_050_00_20m.json")

sf_cal_counties <- us_counties %>% 
  filter(STATE == '06') %>%
  dplyr::select(COUNTY, geometry) %>%
  rename(fips_code = COUNTY)


combined_df4 <- left_join(combined_df3, sf_cal_counties, by = c("fips_code"))
st_write(combined_df4, "./datasets/final_data.geojson")












