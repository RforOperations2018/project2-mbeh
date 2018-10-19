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

#### Read geospatial files
polygons.load <- readOGR(towns.map.file, layer = "Planning_Area_Census2010")
rail.lines.load <- readOGR(rail.lines.file)