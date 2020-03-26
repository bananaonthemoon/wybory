## Kod na podstawie:
## https://github.com/Nowosad/spDataLarge/blob/master/data-raw/08_pol_pres15.R (autor: Roger Bivand)

## Wybory prezydenckie z 2015 roku, pierwsza oraz druga tura (podział dla gmin)


# Pierwsza tura -----------------------------------------------------------


library(tidyverse)
library(readxl)

# Pobranie oraz wczytanie danych z pierwszej tury
download.file("https://prezydent2015.pkw.gov.pl/prezydent_2015_tura1.zip", "prezydent_2015_tura1.zip")
unzip("prezydent_2015_tura1.zip", files="prezydent_2015_tura1.csv")
tura1 = read.csv2("prezydent_2015_tura1.csv", header=TRUE, fileEncoding="CP1250", stringsAsFactors=FALSE)

# Czyszczenie oraz agregowanie danych
tura1[[3]] = formatC(tura1[[3]], width=6, format="d", flag="0")
tura1 = tura1 %>%
  filter(TERYT.gminy != "149901" & TERYT.gminy != "229901") %>%
  group_by(TERYT.gminy) %>%
  summarise_if(is.numeric, sum) %>%
  select(-c(2, 4:20))


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
tura2 = tura2 %>%
  filter(TERYT.gminy != "149901" & TERYT.gminy != "229901") %>%
  group_by(TERYT.gminy) %>%
  summarise_if(is.numeric, sum) %>%
  select(-c(2, 4:20))


# Obie tury ---------------------------------------------------------------


# przypisanie prefiksów do kolumn w turach 
names(tura1) = paste("t1_", names(tura1), sep="")
names(tura2) = paste("t2_", names(tura2), sep="")

obie_tury = merge(tura1, tura2, by.x="t1_TERYT.gminy", by.y="t2_TERYT.gminy")


# Jednostki ewidencyjne ---------------------------------------------------


library(sf)
library(rmapshaper)

# pobranie oraz wczytanie danych wektorowych, ustalenie układu współrzędnych
download.file("https://www.gis-support.pl/downloads/Jednostki_ewidencyjne.zip", "Jednostki_ewidencyjne.zip")
unzip("Jednostki_ewidencyjne.zip", files="Jednostki_ewidencyjne.shp")
unzip("Jednostki_ewidencyjne.zip", files="Jednostki_ewidencyjne.shx")
unzip("Jednostki_ewidencyjne.zip", files="Jednostki_ewidencyjne.dbf")
unzip("Jednostki_ewidencyjne.zip", files="Jednostki_ewidencyjne.prj")
j_ewid = st_read("Jednostki_ewidencyjne.shp", stringsAsFactors=FALSE)
j_ewid = st_transform(j_ewid, crs = 2180)

# uproszczenie geometrii
j_ewid_simp = ms_simplify(j_ewid, keep_shapes = TRUE, method = "dp", keep = 0.2) 
j_ewid$geometry = j_ewid_simp$geometry

# usunięcie zbędnych kolumn
j_ewid = select(j_ewid,-c(4:29))


# Czyszczenie danych ------------------------------------------------------


# porządkowanie obszarów administracyjnych
j_ewid$kod6 = str_sub(j_ewid$JPT_KOD_JE, 1, 6) 
j_ewid_agg = aggregate(j_ewid, list(j_ewid$kod6), head, n=1)
j_ewid1 = j_ewid_agg[, c("kod6", "JPT_NAZWA_", attr(j_ewid_agg, "sf_column"))]

# poprawienie kodowania dla Łodzi oraz Krakowa
lodz_krakow = j_ewid1$kod6
lodz_krakow[substring(lodz_krakow, 1, 4) %in% "1061"] = "106101"
lodz_krakow[substring(lodz_krakow, 1, 4) %in% "1261"] = "126101"
j_ewid_1a = aggregate(j_ewid1, list(kod6a = lodz_krakow), head, n = 1)

# ujednolicenie nazw miejscowości
j_ewid_1a$Nazwa = toupper(j_ewid_1a$JPT_NAZWA_)
j_ewid_1a$Nazwa = sub("ZDR[.]", "- ZDRÓJ", sub("N/", "NAD ", sub("( |-)OB.{1,}", "", sub(" (G|M)$", "", sub("[(]W[)]|[(]M[)]", "", sub(" MIASTO$", "",  sub("^MIASTO ", "",  sub("-(G|GM)$", "", sub("-M$", "",  sub("M[.]", "", sub("-MIASTO", "",  sub("GM.{1,4}", "", sub("-GM.{1,}", "", sub("- .{1,}", "", j_ewid_1a$Nazwa))))))))))))))
j_ewid_1a$Nazwa = str_trim(j_ewid_1a$nazwa, side = c("both", "left", "right"))
j_ewid_1a$Nazwa[grep("1061", j_ewid_1a$kod6a)] = "ŁÓDŹ"
j_ewid_1a$Nazwa[grep("1261", j_ewid_1a$kod6a)] = "KRAKÓW"
j_ewid_1a$Nazwa[grep("040504", j_ewid_1a$kod6a)] = "KOWALEWO POMORSKIE"
j_ewid_1a$Nazwa[grep("121803", j_ewid_1a$kod6a)] = "KALWARIA ZEBRZYDOWSKA"


# Rodzaje gmin ---------------------------------------------------------------


# Agregowanie jednostek ewidencyjnych do poziomu gmin
#szczegółnie 4_5 i 5_4
j_ewid_k8 = substring(j_ewid$JPT_KOD_JE, 8, 8)
j_ewid_k8_agg = aggregate(j_ewid_k8, list(j_ewid$kod6), paste, collapse="_")
j_ewid_k8_agg1 = aggregate(j_ewid_k8_agg, list(kod6a = lodz_krakow), paste, collapse="_")

# jedna poprawka (ok, to jest kompletnie niepotrzebne, ale trzeba poprawić Kraków i Łódź) 
# -- tu w sumie też średnio ogarniam co sie dzieje
j_ewid_k8_agg1$x[which(j_ewid_k8_agg1$x =="9_9_9_9_9")] = "9"
j_ewid_k8_agg1$x[which(j_ewid_k8_agg1$x =="9_9_9_9")] = "9"
j_ewid_k8_agg1$x[which(j_ewid_k8_agg1$x =="5_4")] = "3"
j_ewid_k8_agg1$x[which(j_ewid_k8_agg1$x =="4_5")] = "3"
names(j_ewid_k8_agg1) = c("kod6a", "TERYT", "Rodzaj.gminy")

# przypisanie rodzaju gmin do jednostek
Rodzaj.gminy = rep("Gmina wiejska", length(j_ewid_k8_agg1$Rodzaj.gminy))
Rodzaj.gminy[grep("1", j_ewid_k8_agg1$Rodzaj.gminy)] = "Gmina miejska"
Rodzaj.gminy[grep("3", j_ewid_k8_agg1$Rodzaj.gminy)] = "Gmina miejsko-wiejska"
Rodzaj.gminy[grep("8", j_ewid_k8_agg1$Rodzaj.gminy)] = "Gmina miejska"
Rodzaj.gminy[grep("9", j_ewid_k8_agg1$Rodzaj.gminy)] = "Gmina miejska" #delegacje
j_ewid_k8_agg1$Rodzaj.gminy = factor(Rodzaj.gminy)


# Łączenie danych ---------------------------------------------------------


#  łączenie danych z geometrią
j_ewid_1b = merge(j_ewid_1a, j_ewid_k8_agg1, by.x="kod6a", by.y="kod6a")
pres_gmin = merge(j_ewid_1b, obie_tury, by.x="kod6a", by.y="t1_TERYT.gminy")
pres_gmin = select(pres_gmin,-c(1:3))

# obliczenie frekfencji oraz wyników kandydatów
pres_gmin$"1_frekw" = with(pres_gmin, t1_Liczba.głosów.ważnych / t1_Liczba.wyborców.uprawnionych.do.głosowania * 100)
pres_gmin$"2_frekw" = with(pres_gmin, t2_Liczba.głosów.ważnych / t2_Liczba.wyborców.uprawnionych.do.głosowania * 100)
pres_gmin$f1.duda = with(pres_gmin, t1_Andrzej.Sebastian.Duda / t1_Liczba.głosów.ważnych * 100)
pres_gmin$f2.duda = with(pres_gmin, t2_Andrzej.Sebastian.Duda / t2_Liczba.głosów.ważnych * 100)
pres_gmin$f1.komo = with(pres_gmin,  t1_Bronisław.Maria.Komorowski / t1_Liczba.głosów.ważnych * 100)
pres_gmin$f2.komo = with(pres_gmin,  t2_Bronisław.Maria.Komorowski / t2_Liczba.głosów.ważnych * 100)
pres_gmin$f1.braun = with(pres_gmin,  t1_Grzegorz.Michał.Braun / t1_Liczba.głosów.ważnych * 100)
pres_gmin$f1.jarubas = with(pres_gmin,  t1_Adam.Sebastian.Jarubas / t1_Liczba.głosów.ważnych * 100)
pres_gmin$f1.korwin = with(pres_gmin,  t1_Janusz.Ryszard.Korwin.Mikke / t1_Liczba.głosów.ważnych * 100)
pres_gmin$f1.kowalski = with(pres_gmin,  t1_Marian.Janusz.Kowalski / t1_Liczba.głosów.ważnych * 100)
pres_gmin$f1.kukiz = with(pres_gmin,  t1_Paweł.Piotr.Kukiz / t1_Liczba.głosów.ważnych * 100)
pres_gmin$f1.ogorek = with(pres_gmin,  t1_Magdalena.Agnieszka.Ogórek / t1_Liczba.głosów.ważnych * 100)
pres_gmin$f1.palikot = with(pres_gmin,  t1_Janusz.Marian.Palikot / t1_Liczba.głosów.ważnych * 100)
pres_gmin$f1.tanajo = with(pres_gmin,  t1_Paweł.Jan.Tanajno / t1_Liczba.głosów.ważnych * 100)
pres_gmin$f1.Wilk = with(pres_gmin,  t1_Jacek.Wilk / t1_Liczba.głosów.ważnych * 100)

write_sf(pres_gmin, dsn = "pres_gmin.gpkg")
