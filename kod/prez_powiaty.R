## Kod na podstawie:
## https://github.com/Nowosad/spDataLarge/blob/master/data-raw/08_pol_prez15.R (autor: Roger Bivand)

## Wybory prezydenckie z 2015 roku, pierwsza oraz druga tura (podział dla powiatów)


# Pierwsza tura -----------------------------------------------------------


library(tidyverse)
library(readxl)
library(writexl)

# Pobranie oraz wczytanie danych z pierwszej tury
temp <- tempfile()
download.file("https://wybory.gov.pl/prezydent20200628/data/1/csv/wyniki_gl_na_kand_po_powiatach_proc_csv.zip", temp)
unzip(temp, files="wyniki_gl_na_kand_po_powiatach_proc_utf8.csv", exdir = "dane/pobrane/tura1")
tura1 = read.csv2("dane/pobrane/tura1/wyniki_gl_na_kand_po_powiatach_proc_utf8.csv", header=TRUE, encoding = "UTF-8", stringsAsFactors=FALSE)

# Czyszczenie oraz agregowanie danych
tura1[[2]] = formatC(tura1[[2]], width=6, format="d", flag="0")
tura1[[2]] = str_sub(tura1[[2]], 1, 4) 
tura1 = select(tura1, -c(1, 4, 6:10, 22))
names(tura1)[names(tura1) == 'Kod.TERYT'] <- 'TERYT'

# Druga tura -----------------------------------------------------------


# Pobranie oraz wczytanie danych z pierwszej tury
download.file("https://wybory.gov.pl/prezydent20200628/data/2/csv/wyniki_gl_na_kand_po_powiatach_proc_csv.zip", temp)
unzip(temp, files="wyniki_gl_na_kand_po_powiatach_proc_utf8.csv", exdir = "dane/pobrane/tura2")
tura2 = read.csv2("dane/pobrane/tura2/wyniki_gl_na_kand_po_powiatach_proc_utf8.csv", header=TRUE, encoding = "UTF-8", stringsAsFactors=FALSE)

# Czyszczenie oraz agregowanie danych
tura2[[2]] = formatC(tura2[[2]], width=6, format="d", flag="0")
tura2[[2]] = str_sub(tura2[[2]], 1, 4) 
tura2 = select(tura2, -c(1, 4, 6:10, 13))
names(tura2)[names(tura2) == 'Kod.TERYT'] <- 'TERYT'


# Obie tury ---------------------------------------------------------------


# Przypisanie prefiksów do kolumn w turach 
names(tura1) = paste("t1_", names(tura1), sep="")
names(tura2) = paste("t2_", names(tura2), sep="")

obie_tury = merge(tura1, tura2, by.x="t1_TERYT", by.y="t2_TERYT")

write_xlsx(obie_tury, path = "dane/wybory_powiaty_proc.xlsx")


# Jednostki ewidencyjne ---------------------------------------------------


library(sf)
library(rmapshaper)
library(lwgeom)

# Pobranie oraz wczytanie danych wektorowych, ustalenie układu współrzędnych
download.file("https://www.gis-support.pl/downloads/Powiaty.zip", "dane/temp/Powiaty.zip")
unzip("dane/temp/Powiaty.zip", exdir = "dane/pobrane")
powiat = read_sf("dane/pobrane/Powiaty.shp", stringsAsFactors=FALSE) %>%
  st_transform(crs = 2180) %>% 
  select(-c(4:29))

# Uproszczenie geometrii i zapisanie pliku w formacie geopackage (tutaj mały bajzel jest)
powiat_simp = ms_simplify(powiat, keep_shapes = TRUE, method = "vis", keep = 0.1) 
powiat$geometry = powiat_simp$geometry
write_sf(powiat, dsn = "dane/temp/powiat.gpkg", driver = "GPKG")
powiat = read_sf("dane/temp/powiat.gpkg", stringsAsFactors=FALSE) 
powiat = select(powiat, -1)

#isnt valid, próbowałam użyć st_make_valid ale nie działa


# Łączenie danych ---------------------------------------------------------


# Łączenie danych z geometrią
prez_powiat = merge(powiat, obie_tury, by.x="JPT_KOD_JE", by.y="t1_TERYT") %>%
  select(-c(2, 16))

write_sf(prez_powiat, dsn = "dane/prez_powiaty.gpkg")