## Kod na podstawie:
## https://github.com/Nowosad/spDataLarge/blob/master/data-raw/08_pol_pres15.R (autor: Roger Bivand)

## Wybory prezydenckie z 2015 roku, pierwsza oraz druga tura (podział dla gmin)


# Pierwsza tura -----------------------------------------------------------


library(tidyverse)
library(stringr)
library(readxl)

# Pobranie oraz wczytanie danych z pierwszej tury
temp <- tempfile()
download.file("https://wybory.gov.pl/prezydent20200628/data/1/csv/wyniki_gl_na_kand_po_gminach_csv.zip", temp)
unzip(temp, files="wyniki_gl_na_kand_po_gminach_utf8.csv", exdir = "dane/pobrane/tura1")
tura1 = read.csv2("dane/pobrane/tura1/wyniki_gl_na_kand_po_gminach_utf8.csv", header=TRUE, encoding="UTF-8", stringsAsFactors=FALSE)

# Czyszczenie oraz agregowanie danych
tura1[[2]] = formatC(tura1[[2]], width=6, format="d", flag="0")
tura1[[2]] = str_sub(tura1[[2]], 1, 6) 
tura1 = select(tura1, c(2, 8, 23, 28, 31, 37))
names(tura1)[names(tura1) == 'Kod.TERYT'] <- 'TERYT'
tura1 = filter(tura1, between(TERYT, 300000, 309999))


# Druga tura -----------------------------------------------------------


# Pobranie oraz wczytanie danych z drugiej tury
download.file("https://wybory.gov.pl/prezydent20200628/data/2/csv/wyniki_gl_na_kand_po_gminach_csv.zip", temp)
unzip(temp, files="wyniki_gl_na_kand_po_gminach_utf8.csv", exdir = "dane/pobrane/tura2")
tura2 = read.csv2("dane/pobrane/tura2/wyniki_gl_na_kand_po_gminach_utf8.csv", header=TRUE, encoding="UTF-8", stringsAsFactors=FALSE)

# Czyszczenie oraz agregowanie danych
tura2[[2]] = formatC(tura2[[2]], width=6, format="d", flag="0")
tura2 = select(tura2, c(2, 8, 23, 28:30))
names(tura2)[names(tura2) == 'Kod.TERYT'] <- 'TERYT'
tura2 = filter(tura2, between(TERYT, 300000, 309999))


# Obie tury ---------------------------------------------------------------


# Przypisanie prefiksów do kolumn w turach 
names(tura1) = paste("t1_", names(tura1), sep="")
names(tura2) = paste("t2_", names(tura2), sep="")

obie_tury = merge(tura1, tura2, by.x="t1_TERYT", by.y="t2_TERYT")

write_xlsx(obie_tury, path = "dane/wybory_gmin_proc.xlsx")



# Jednostki ewidencyjne ---------------------------------------------------


library(sf)
library(rmapshaper)

# Pobranie oraz wczytanie danych wektorowych, ustalenie układu współrzędnych
download.file("https://www.gis-support.pl/downloads/Gminy.zip", temp)
unzip(temp, exdir = "dane/pobrane")
wlkp = read_sf("dane/pobrane/Gminy.shp", stringsAsFactors=FALSE) %>%
  st_transform(crs = 2180) %>% 
  select(-c(1, 4:29))

# Uproszczenie geometrii i zapisanie pliku w formacie geopackage (tutaj mały bajzel jest)
wlkp_simp = ms_simplify(wlkp, keep_shapes = TRUE, keep = 0.2, explode = FALSE, snap = TRUE) 
wlkp$geometry = wlkp_simp$geometry
write_sf(wlkp, dsn = "dane/temp/wlkp.gpkg")
wlkp = read_sf("dane/temp/wlkp.gpkg", stringsAsFactors=FALSE)
wlkp[[1]] = str_sub(wlkp[[1]], 1, 6)
wlkp = filter(wlkp, between(JPT_KOD_JE, 300000, 309999))


# Łączenie danych ---------------------------------------------------------


# Łączenie danych z geometrią
wlkp_merge = merge(wlkp, obie_tury, by.x="JPT_KOD_JE", by.y="t1_TERYT")

wlkp_merge$t1_frek = wlkp_merge$t1_Liczba.kart.ważnych * 100 / wlkp_merge$t1_Liczba.wyborców.uprawnionych.do.głosowania
wlkp_merge$t2_frek = wlkp_merge$t2_Liczba.kart.ważnych * 100 / wlkp_merge$t2_Liczba.wyborców.uprawnionych.do.głosowania
wlkp_merge$t1_duda = wlkp_merge$t1_Andrzej.Sebastian.DUDA * 100 / wlkp_merge$t1_Liczba.głosów.ważnych.oddanych.łącznie.na.wszystkich.kandydatów
wlkp_merge$t1_trzaskowski = wlkp_merge$t1_Rafał.Kazimierz.TRZASKOWSKI * 100 / wlkp_merge$t1_Liczba.głosów.ważnych.oddanych.łącznie.na.wszystkich.kandydatów
wlkp_merge$t2_duda = wlkp_merge$t2_Andrzej.Sebastian.DUDA * 100 / wlkp_merge$t2_Liczba.głosów.ważnych.oddanych.łącznie.na.wszystkich.kandydatów
wlkp_merge$t2_trzaskowski = wlkp_merge$t2_Rafał.Kazimierz.TRZASKOWSKI * 100 / wlkp_merge$t2_Liczba.głosów.ważnych.oddanych.łącznie.na.wszystkich.kandydatów
wlkp_merge$t2_roznica_glosow = wlkp_merge$t2_Andrzej.Sebastian.DUDA - wlkp_merge$t2_Rafał.Kazimierz.TRZASKOWSKI
wlkp_merge$t1_roznica_glosow = wlkp_merge$t1_Andrzej.Sebastian.DUDA - wlkp_merge$t1_Rafał.Kazimierz.TRZASKOWSKI
wlkp_merge$roznica_frek = wlkp_merge$t2_frek - wlkp_merge$t1_frek


write_sf(wlkp_merge, dsn = "dane/wielkopolska.gpkg")
