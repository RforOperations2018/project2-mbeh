library(readr)
library(dplyr)
library(tidyr)
library(rgdal)

#### Configuration of API url, destination file paths & names
land.area.api <- 'https://data.gov.sg/dataset/e0397cd9-9fb6-4de4-83d9-f9dba2591ead/download'
hdb.units.api <- 'https://data.gov.sg/dataset/f6cbe631-c9cf-4276-84ac-6d95697d32ef/download'
towns.map.api <- 'https://data.gov.sg/dataset/70c818b4-2d1b-4400-90cb-21e0456eeb3f/download'
rail.lines.api <- 'https://data.gov.sg/dataset/fd3ab0ac-8972-4c12-a26e-355ca24af53e/download'
