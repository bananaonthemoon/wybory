## Kod na podstawie:
## https://github.com/Nowosad/spDataLarge/blob/master/data-raw/08_pol_prez15.R (autor: Roger Bivand)

## Wybory prezydenckie z 2015 roku, pierwsza oraz druga tura (podział dla powiatów)


# Pierwsza tura -----------------------------------------------------------


library(tidyverse)
library(readxl)

# Pobranie oraz wczytanie danych z pierwszej tury
download.file("https://prezydent2015.pkw.gov.pl/prezydent_2015_tura1.zip", "prezydent_2015_tura1.zip")
unzip("prezydent_2015_tura1.zip", files="prezydent_2015_tura1.csv")
tura1 = read.csv2("prezydent_2015_tura1.csv", header=TRUE, fileEncoding="CP1250", stringsAsFactors=FALSE)

# Czyszczenie oraz agregowanie danych
tura1[[3]] = formatC(tura1[[3]], width=6, format="d", flag="0")
tura1$kod4 = str_sub(tura1$TERYT.gminy, 1, 4)
tura1 = tura1 %>% select(-c(4:6, 8:24))
tura1 = aggregate(tura1[, 4:16], list(tura1$kod4), sum)


# Druga tura -----------------------------------------------------------


# Pobranie oraz wczytanie danych z pierwszej tury
download.file("https://prezydent2015.pkw.gov.pl/wyniki_tura2.zip", "wyniki_tura2.zip")
unzip("wyniki_tura2.zip", files="wyniki_tura2.xls")
# readxl::read_excel() niepoprawna kolumna "TERYT gminy"
# https://github.com/tidyverse/readxl/issues/565
# przekonwertować do CSV z poziomu Excela, zostawić kodowanie CP1250
tura2 = read.csv2("wyniki_tura2.csv", header=TRUE, fileEncoding="CP1250", stringsAsFactors=FALSE)

# Czyszczenie oraz agregowanie danych
tura2[[3]] = formatC(tura2[[3]], width=6, format="d", flag="0")
tura2$kod4 = str_sub(tura2$TERYT.gminy, 1, 4)
tura2 = tura2 %>% select(-c(4, 6:22))
tura2 = aggregate(tura2[, 4:7], list(tura2$kod4), sum)


# Obie tury ---------------------------------------------------------------


# Przypisanie prefiksów do kolumn w turach 
names(tura1) = paste("t1_", names(tura1), sep="")
names(tura2) = paste("t2_", names(tura2), sep="")

obie_tury = merge(tura1, tura2, by.x="t1_Group.1", by.y="t2_Group.1")


# Jednostki ewidencyjne ---------------------------------------------------


library(sf)
library(rmapshaper)

# Pobranie oraz wczytanie danych wektorowych, ustalenie układu współrzędnych
download.file("https://www.gis-support.pl/downloads/Powiaty.zip", "Powiaty.zip")
unzip("Powiaty.zip")
powiat = read_sf("Powiaty.shp", stringsAsFactors=FALSE) %>%
  st_transform(crs = 2180) %>% 
  select(-c(4:29))

# Uproszczenie geometrii i zapisanie pliku w formacie geopackage (tutaj mały bajzel jest)
powiat_simp = ms_simplify(powiat, keep_shapes = TRUE, method = "vis", keep = 0.1) 
powiat$geometry = powiat_simp$geometry
write_sf(powiat, dsn = "powiat.gpkg", driver = "GPKG")
powiat = read_sf("powiat.gpkg", stringsAsFactors=FALSE)


# Czyszczenie danych ------------------------------------------------------


# Porządkowanie obszarów administracyjnych
powiat$kod4 = str_sub(powiat$JPT_KOD_JE, 1, 4) 
powiat_agg = aggregate(powiat, list(powiat$kod4), head, n=1)
powiat1 = powiat_agg[, c("kod4", "JPT_NAZWA_", attr(powiat_agg, "sf_column"))]
powiat1$Nazwa = toupper(powiat1$JPT_NAZWA_)


# Łączenie danych ---------------------------------------------------------


# Łączenie danych z geometrią
prez_powiat = merge(powiat1, obie_tury, by.x="kod4", by.y="t1_Group.1") %>%
  select(-c(1:3))

# Obliczenie frekfencji oraz wyników kandydatów
prez_powiat$"1_frekw" = with(prez_powiat, t1_Liczba.głosów.ważnych / t1_Liczba.wyborców.uprawnionych.do.głosowania * 100)
prez_powiat$"2_frekw" = with(prez_powiat, t2_Liczba.głosów.ważnych / t2_Liczba.wyborców.uprawnionych.do.głosowania * 100)
prez_powiat$f1.duda = with(prez_powiat, t1_Andrzej.Sebastian.Duda / t1_Liczba.głosów.ważnych * 100)
prez_powiat$f2.duda = with(prez_powiat, t2_Andrzej.Sebastian.Duda / t2_Liczba.głosów.ważnych * 100)
prez_powiat$f1.komo = with(prez_powiat,  t1_Bronisław.Maria.Komorowski / t1_Liczba.głosów.ważnych * 100)
prez_powiat$f2.komo = with(prez_powiat,  t2_Bronisław.Maria.Komorowski / t2_Liczba.głosów.ważnych * 100)
prez_powiat$f1.braun = with(prez_powiat,  t1_Grzegorz.Michał.Braun / t1_Liczba.głosów.ważnych * 100)
prez_powiat$f1.jarubas = with(prez_powiat,  t1_Adam.Sebastian.Jarubas / t1_Liczba.głosów.ważnych * 100)
prez_powiat$f1.korwin = with(prez_powiat,  t1_Janusz.Ryszard.Korwin.Mikke / t1_Liczba.głosów.ważnych * 100)
prez_powiat$f1.kowalski = with(prez_powiat,  t1_Marian.Janusz.Kowalski / t1_Liczba.głosów.ważnych * 100)
prez_powiat$f1.kukiz = with(prez_powiat,  t1_Paweł.Piotr.Kukiz / t1_Liczba.głosów.ważnych * 100)
prez_powiat$f1.ogorek = with(prez_powiat,  t1_Magdalena.Agnieszka.Ogórek / t1_Liczba.głosów.ważnych * 100)
prez_powiat$f1.palikot = with(prez_powiat,  t1_Janusz.Marian.Palikot / t1_Liczba.głosów.ważnych * 100)
prez_powiat$f1.tanajo = with(prez_powiat,  t1_Paweł.Jan.Tanajno / t1_Liczba.głosów.ważnych * 100)
prez_powiat$f1.Wilk = with(prez_powiat,  t1_Jacek.Wilk / t1_Liczba.głosów.ważnych * 100)


write_sf(prez_powiat, dsn = "prez_powiaty.gpkg")
