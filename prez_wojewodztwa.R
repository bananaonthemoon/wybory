## Kod na podstawie:
## https://github.com/Nowosad/spDataLarge/blob/master/data-raw/08_pol_prez15.R (autor: Roger Bivand)

## Wybory prezydenckie z 2015 roku, pierwsza oraz druga tura (podział dla województw)


# Pierwsza tura -----------------------------------------------------------


library(tidyverse)
library(readxl)

# Pobranie oraz wczytanie danych z pierwszej tury
download.file("https://prezydent2015.pkw.gov.pl/prezydent_2015_tura1.zip", "prezydent_2015_tura1.zip")
unzip("prezydent_2015_tura1.zip", files="prezydent_2015_tura1.csv")
tura1 = read.csv2("prezydent_2015_tura1.csv", header=TRUE, fileEncoding="CP1250", stringsAsFactors=FALSE)

# Czyszczenie oraz agregowanie danych
tura1[[3]] = formatC(tura1[[3]], width=6, format="d", flag="0")
tura1$kod2 = str_sub(tura1$TERYT.gminy, 1, 2)
tura1 = aggregate(tura1[, 7:37], list(tura1$kod2), sum)


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
tura2$kod2 = str_sub(tura2$TERYT.gminy, 1, 2)
tura2 = aggregate(tura2[, 5:25], list(tura2$kod2), sum)


# Obie tury ---------------------------------------------------------------


# Przypisanie prefiksów do kolumn w turach 
names(tura1) = paste("t1_", names(tura1), sep="")
names(tura2) = paste("t2_", names(tura2), sep="")

obie_tury = merge(tura1, tura2, by.x="t1_Group.1", by.y="t2_Group.1")


# Jednostki ewidencyjne ---------------------------------------------------


library(sf)
library(rmapshaper)

# Pobranie oraz wczytanie danych wektorowych, ustalenie układu współrzędnych
download.file("https://www.gis-support.pl/downloads/Wojewodztwa.zip", "Wojewodztwa.zip")
unzip("Wojewodztwa.zip")
woj = read_sf("Wojew˘dztwa.shp", stringsAsFactors=FALSE) %>%
  st_transform(crs = 2180) %>% 
  select(-c(4:29))

# Uproszczenie geometrii i zapisanie pliku w formacie geopackage (tutaj mały bajzel jest)
woj_simp = ms_simplify(woj, keep_shapes = TRUE, method = "vis", keep = 0.1) 
woj$geometry = woj_simp$geometry
write_sf(woj, dsn = "woj.gpkg", driver = "GPKG")
woj = read_sf("woj.gpkg", stringsAsFactors=FALSE)


# Czyszczenie danych ------------------------------------------------------


# Porządkowanie obszarów administracyjnych
woj$kod2 = str_sub(woj$JPT_KOD_JE, 1, 2) 
woj_agg = aggregate(woj, list(woj$kod2), head, n=1)
woj1 = woj_agg[, c("kod2", "JPT_NAZWA_", attr(woj_agg, "sf_column"))]
woj1$Nazwa = toupper(woj1$JPT_NAZWA_)


# Łączenie danych ---------------------------------------------------------


# Łączenie danych z geometrią
prez_woj = merge(woj1, obie_tury, by.x="kod2", by.y="t1_Group.1")

# Obliczenie frekfencji oraz wyników kandydatów
prez_woj$"1_frekw" = with(prez_woj, t1_Liczba.głosów.ważnych / t1_Liczba.wyborców.uprawnionych.do.głosowania * 100)
prez_woj$"2_frekw" = with(prez_woj, t2_Liczba.głosów.ważnych / t2_Liczba.wyborców.uprawnionych.do.głosowania * 100)
prez_woj$f1.duda = with(prez_woj, t1_Andrzej.Sebastian.Duda / t1_Liczba.głosów.ważnych * 100)
prez_woj$f2.duda = with(prez_woj, t2_Andrzej.Sebastian.Duda / t2_Liczba.głosów.ważnych * 100)
prez_woj$f1.komo = with(prez_woj,  t1_Bronisław.Maria.Komorowski / t1_Liczba.głosów.ważnych * 100)
prez_woj$f2.komo = with(prez_woj,  t2_Bronisław.Maria.Komorowski / t2_Liczba.głosów.ważnych * 100)
prez_woj$f1.braun = with(prez_woj,  t1_Grzegorz.Michał.Braun / t1_Liczba.głosów.ważnych * 100)
prez_woj$f1.jarubas = with(prez_woj,  t1_Adam.Sebastian.Jarubas / t1_Liczba.głosów.ważnych * 100)
prez_woj$f1.korwin = with(prez_woj,  t1_Janusz.Ryszard.Korwin.Mikke / t1_Liczba.głosów.ważnych * 100)
prez_woj$f1.kowalski = with(prez_woj,  t1_Marian.Janusz.Kowalski / t1_Liczba.głosów.ważnych * 100)
prez_woj$f1.kukiz = with(prez_woj,  t1_Paweł.Piotr.Kukiz / t1_Liczba.głosów.ważnych * 100)
prez_woj$f1.ogorek = with(prez_woj,  t1_Magdalena.Agnieszka.Ogórek / t1_Liczba.głosów.ważnych * 100)
prez_woj$f1.palikot = with(prez_woj,  t1_Janusz.Marian.Palikot / t1_Liczba.głosów.ważnych * 100)
prez_woj$f1.tanajo = with(prez_woj,  t1_Paweł.Jan.Tanajno / t1_Liczba.głosów.ważnych * 100)
prez_woj$f1.Wilk = with(prez_woj,  t1_Jacek.Wilk / t1_Liczba.głosów.ważnych * 100)

write_sf(prez_woj, dsn = "prez_woj.gpkg")
