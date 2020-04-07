library(geogrid)
library(sf)
library(tidyverse)

#download.file("https://www.gis-support.pl/downloads/Wojewodztwa.zip", "Wojewodztwa.zip")
#unzip("Wojewodztwa.zip")
granice = read_sf("Wojew˘dztwa.shp", stringsAsFactors=FALSE) %>%
  st_transform(crs = 2180) %>%
  st_combine() 

pres_gmin = read_sf("pres_gmin.gpkg", stringsAsFactors=FALSE) 