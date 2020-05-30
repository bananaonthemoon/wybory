## Kod na podstawie:
## https://github.com/Nowosad/spDataLarge/blob/master/data-raw/08_pol_pres15.R (autor: Roger Bivand)

## Wybory prezydenckie z 2015 roku, pierwsza oraz druga tura (podział dla gmin w Wielkopolsce)


# Pierwsza tura -----------------------------------------------------------


library(tidyverse)
library(readxl)

# Pobranie oraz wczytanie danych z pierwszej tury
download.file("https://prezydent2015.pkw.gov.pl/prezydent_2015_tura1.zip", "dane/temp/prezydent_2015_tura1.zip")
unzip("dane/temp/prezydent_2015_tura1.zip", files="prezydent_2015_tura1.csv", exdir = "dane/pobrane")
tura1 = read.csv2("dane/pobrane/prezydent_2015_tura1.csv", header=TRUE, fileEncoding="CP1250", stringsAsFactors=FALSE)

# Czyszczenie oraz agregowanie danych
tura1[[3]] = formatC(tura1[[3]], width=6, format="d", flag="0")
tura1 = tura1 %>%
  filter(TERYT.gminy != "149901" & TERYT.gminy != "229901") %>%
  group_by(TERYT.gminy) %>%
  summarise_if(is.numeric, sum) %>%
  select(-c(2, 4:20))


# Druga tura -----------------------------------------------------------


# Pobranie oraz wczytanie danych z drugiej tury
download.file("https://prezydent2015.pkw.gov.pl/wyniki_tura2.zip", "dane/temp/wyniki_tura2.zip")
unzip("dane/temp/wyniki_tura2.zip", files="wyniki_tura2.xls", exdir = "dane/pobrane")
# readxl::read_excel() niepoprawna kolumna "TERYT gminy"
# https://github.com/tidyverse/readxl/issues/565
# przekonwertować do CSV z poziomu Excela, zostawić kodowanie CP1250
tura2 = read.csv2("dane/pobrane/wyniki_tura2.csv", header=TRUE, fileEncoding="CP1250", stringsAsFactors=FALSE)

# Czyszczenie oraz agregowanie danych
tura2[[3]] = formatC(tura2[[3]], width=6, format="d", flag="0")
tura2 = tura2 %>%
  filter(TERYT.gminy != "149901" & TERYT.gminy != "229901") %>%
  group_by(TERYT.gminy) %>%
  summarise_if(is.numeric, sum) %>%
  select(-c(2, 4:20))


# Obie tury ---------------------------------------------------------------


# Przypisanie prefiksów do kolumn w turach 
names(tura1) = paste("t1_", names(tura1), sep="")
names(tura2) = paste("t2_", names(tura2), sep="")

obie_tury = merge(tura1, tura2, by.x="t1_TERYT.gminy", by.y="t2_TERYT.gminy") %>% 
  filter(str_detect(t1_TERYT.gminy, "^30"))


# Jednostki ewidencyjne ---------------------------------------------------


library(sf)
library(rmapshaper)

# Pobranie oraz wczytanie danych wektorowych, ustalenie układu współrzędnych
download.file("https://www.gis-support.pl/downloads/Jednostki_ewidencyjne.zip", "dane/temp/Jednostki_ewidencyjne.zip")
unzip("dane/temp/Jednostki_ewidencyjne.zip", exdir = "dane/pobrane")
j_ewid = read_sf("dane/pobrane/Jednostki_ewidencyjne.shp", stringsAsFactors=FALSE) %>%
  st_transform(crs = 2180) %>% 
  select(-c(4:29)) %>%
  filter(str_detect(JPT_KOD_JE, "^30"))

# Uproszczenie geometrii i zapisanie pliku w formacie geopackage (tutaj mały bajzel jest)
j_ewid_simp = ms_simplify(j_ewid, keep_shapes = TRUE, method = "vis", keep = 0.1) 
j_ewid$geometry = j_ewid_simp$geometry
write_sf(j_ewid, dsn = "dane/temp/j_ewid_wlkp.gpkg", driver = "GPKG")
j_ewid = read_sf("dane/temp/j_ewid_wlkp.gpkg", stringsAsFactors=FALSE)


# Czyszczenie danych ------------------------------------------------------


# Porządkowanie obszarów administracyjnych
j_ewid$kod6 = str_sub(j_ewid$JPT_KOD_JE, 1, 6) 
j_ewid_agg = aggregate(j_ewid, list(j_ewid$kod6), head, n=1)
j_ewid_1 = j_ewid_agg[, c("kod6", "JPT_NAZWA_", attr(j_ewid_agg, "sf_column"))]

# Ujednolicenie nazw miejscowości
j_ewid_1$Nazwa = toupper(j_ewid_1$JPT_NAZWA_)
j_ewid_1$Nazwa = sub("N/", "NAD ", sub("( |-)OB.{1,}", "", sub(" (G|M)$", "", sub("[(]W[)]|[(]M[)]", "", sub(" MIASTO$", "",  sub("^MIASTO ", "",  sub("-(G|GM)$", "", sub("-M$", "",  sub("M[.]", "", sub("-MIASTO", "",  sub("GM.{1,4}", "", sub("-GM.{1,}", "", sub("- .{1,}", "", j_ewid_1$Nazwa)))))))))))))
j_ewid_1$Nazwa = str_trim(j_ewid_1$Nazwa, side = c("both", "left", "right"))


# Rodzaje gmin ---------------------------------------------------------------


# Agregowanie jednostek ewidencyjnych do poziomu gmin
# Szczegółnie 4_5 i 5_4
j_ewid_k8 = substring(j_ewid$JPT_KOD_JE, 8, 8)
j_ewid_k8_agg = aggregate(j_ewid_k8, list(j_ewid$kod6), paste, collapse="_")
j_ewid_k8_agg$x[which(j_ewid_k8_agg$x =="5_4")] = "3"
j_ewid_k8_agg$x[which(j_ewid_k8_agg$x =="4_5")] = "3"
names(j_ewid_k8_agg) = c("TERYT", "Rodzaj.gminy")

# Przypisanie rodzaju gmin do jednostek
Rodzaj.gminy = rep("Gmina wiejska", length(j_ewid_k8_agg$Rodzaj.gminy))
Rodzaj.gminy[grep("1", j_ewid_k8_agg$Rodzaj.gminy)] = "Gmina miejska"
Rodzaj.gminy[grep("3", j_ewid_k8_agg$Rodzaj.gminy)] = "Gmina miejsko-wiejska"
Rodzaj.gminy[grep("8", j_ewid_k8_agg$Rodzaj.gminy)] = "Gmina miejska"
Rodzaj.gminy[grep("9", j_ewid_k8_agg$Rodzaj.gminy)] = "Gmina miejska" #delegacje
j_ewid_k8_agg$Rodzaj.gminy = factor(Rodzaj.gminy)


# Łączenie danych ---------------------------------------------------------


# Łączenie danych z geometrią
j_ewid_1b = merge(j_ewid_1, j_ewid_k8_agg, by.x="kod6", by.y="TERYT")
prez_wlkp = merge(j_ewid_1, obie_tury, by.x="kod6", by.y="t1_TERYT.gminy")
prez_wlkp = select(prez_wlkp,-c(1:3))

# Obliczenie frekfencji oraz wyników kandydatów
prez_wlkp$"1_frekw" = with(prez_wlkp, t1_Liczba.głosów.ważnych / t1_Liczba.wyborców.uprawnionych.do.głosowania * 100)
prez_wlkp$"2_frekw" = with(prez_wlkp, t2_Liczba.głosów.ważnych / t2_Liczba.wyborców.uprawnionych.do.głosowania * 100)
prez_wlkp$f1.duda = with(prez_wlkp, t1_Andrzej.Sebastian.Duda / t1_Liczba.głosów.ważnych * 100)
prez_wlkp$f2.duda = with(prez_wlkp, t2_Andrzej.Sebastian.Duda / t2_Liczba.głosów.ważnych * 100)
prez_wlkp$f1.komo = with(prez_wlkp,  t1_Bronisław.Maria.Komorowski / t1_Liczba.głosów.ważnych * 100)
prez_wlkp$f2.komo = with(prez_wlkp,  t2_Bronisław.Maria.Komorowski / t2_Liczba.głosów.ważnych * 100)
prez_wlkp$f1.braun = with(prez_wlkp,  t1_Grzegorz.Michał.Braun / t1_Liczba.głosów.ważnych * 100)
prez_wlkp$f1.jarubas = with(prez_wlkp,  t1_Adam.Sebastian.Jarubas / t1_Liczba.głosów.ważnych * 100)
prez_wlkp$f1.korwin = with(prez_wlkp,  t1_Janusz.Ryszard.Korwin.Mikke / t1_Liczba.głosów.ważnych * 100)
prez_wlkp$f1.kowalski = with(prez_wlkp,  t1_Marian.Janusz.Kowalski / t1_Liczba.głosów.ważnych * 100)
prez_wlkp$f1.kukiz = with(prez_wlkp,  t1_Paweł.Piotr.Kukiz / t1_Liczba.głosów.ważnych * 100)
prez_wlkp$f1.ogorek = with(prez_wlkp,  t1_Magdalena.Agnieszka.Ogórek / t1_Liczba.głosów.ważnych * 100)
prez_wlkp$f1.palikot = with(prez_wlkp,  t1_Janusz.Marian.Palikot / t1_Liczba.głosów.ważnych * 100)
prez_wlkp$f1.tanajo = with(prez_wlkp,  t1_Paweł.Jan.Tanajno / t1_Liczba.głosów.ważnych * 100)
prez_wlkp$f1.wilk = with(prez_wlkp,  t1_Jacek.Wilk / t1_Liczba.głosów.ważnych * 100)

write_sf(prez_wlkp, dsn = "dane/prez_wlkp.gpkg")

prez_wlkp_demo = prez_gminy %>% select(-c(1:2,4:21))

write_sf(prez_wlkp_demo, dsn = "dane/prez_wlkp_demo.gpkg")
