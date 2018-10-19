library(readr)
library(dplyr)
library(tidyr)
library(rgdal)

#### Configuration of API url, destination file paths & names
land.area.api <- 'https://data.gov.sg/dataset/e0397cd9-9fb6-4de4-83d9-f9dba2591ead/download'
hdb.units.api <- 'https://data.gov.sg/dataset/f6cbe631-c9cf-4276-84ac-6d95697d32ef/download'
towns.map.api <- 'https://data.gov.sg/dataset/70c818b4-2d1b-4400-90cb-21e0456eeb3f/download'
rail.lines.api <- 'https://data.gov.sg/dataset/fd3ab0ac-8972-4c12-a26e-355ca24af53e/download'
hdb.units.raw.zip <- './data/hdb_units.zip'
hdb.units.file <- './data/hdb_units/dwelling-units-under-hdbs-management-by-town-and-flat-type.csv'
land.area.raw.zip <- './data/land_area.zip'
land.area.file <- './data/land_area/land-area-and-dwelling-units-by-town.csv'
towns.map.raw.zip <- "./data/map.zip"
towns.map.kml.zip <- "./data/map/planning-area-census2010-kml.zip"
towns.map.file <- './data/map/Planning_Area_Census2010.kml'
rail.lines.zip <- './data/rail_lines.zip'
rail.lines.kml.zip <- './data/rail_lines/mp08-rail-line-kml.zip'
rail.lines.file <- './data/rail_lines/G_MP08_RAIL_LI.kml'

#### Create output directory if doesn't exist
data.directory <- "./data"
if (!file.exists(data.directory)){
  dir.create(data.directory)
}

#### Download and unzip files
download.file(hdb.units.api, "wget", destfile = hdb.units.raw.zip)
unzip(hdb.units.raw.zip, exdir = strsplit(hdb.units.raw.zip, ".zip")[[1]])
download.file(land.area.api, "wget", destfile = land.area.raw.zip)
unzip(land.area.raw.zip, exdir = strsplit(land.area.raw.zip, ".zip")[[1]])
download.file(towns.map.api, "wget", destfile = towns.map.raw.zip)
unzip(towns.map.raw.zip, exdir = strsplit(towns.map.raw.zip, ".zip")[[1]])
unzip(towns.map.kml.zip, exdir =strsplit(towns.map.raw.zip, ".zip")[[1]])
download.file(rail.lines.api, "wget", destfile = rail.lines.zip)
unzip(rail.lines.zip, exdir = strsplit(rail.lines.zip, ".zip")[[1]])
unzip(rail.lines.kml.zip, exdir = strsplit(rail.lines.zip, ".zip")[[1]])

#### Data Cleaning of HDB units data
# Rename columns / remove spaces in column names where possible
hdb.units.raw <- read_csv(hdb.units.file) %>% rename(town = town_or_estate)
hdb.units.raw <- hdb.units.raw %>% filter(town != "Central Area", flat_type != "HUDC")
hdb.units.raw[hdb.units.raw=="Studio Apartment"] <- "Studio"
hdb.units.raw[hdb.units.raw=="Rental Units"] <- "rental_units"
hdb.units.raw[hdb.units.raw=="Sold Units"] <- "sold_units"
hdb.units.raw$town <- sapply(as.list(hdb.units.raw$town), toupper)
# Spread data (2 times) from long to wide (for data table display)
hdb.units.load <- spread(hdb.units.raw, key = sold_or_rental, value = no_of_dwelling_units)
hdb.units.load[is.na(hdb.units.load)] <- 0
hdb.units.load <- hdb.units.load %>% mutate(total_units = rental_units + sold_units)
hdb.units.spread <- hdb.units.load %>% select(financial_year, town, flat_type, total_units) %>% 
  spread(key = flat_type, value = total_units) %>% 
  mutate(total_units = rowSums(.[3:length(.)]))

#### Data Cleaning of Land Area data
land.area.raw <- read_csv(land.area.file) %>% select(financial_year, town, total_land_area) %>%
  mutate(total_land_area = as.numeric(total_land_area))
# Rename columns so that town names are capitalized, consistent with other datasets
land.area.raw$town <- sapply(as.list(land.area.raw$town), toupper)

#### Merge HDB units and Land Area data (for data table display)
data.load <- merge(hdb.units.spread, land.area.raw, by = c("financial_year", "town")) %>%
  mutate(density = total_units / total_land_area)

#### Read geospatial files
polygons.load <- readOGR(towns.map.file, layer = "Planning_Area_Census2010")
rail.lines.load <- readOGR(rail.lines.file)