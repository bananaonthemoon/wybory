## kod na podstawie https://github.com/Nowosad/spDataLarge/blob/master/data-raw/08_pol_pres15.R autorstwa Rogera Bivanda

## Wybory prezydenckie z 2015 roku, pierwsza oraz druga tura

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
  select(-Numer.obwodu)

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
  select(-Numer.obwodu)

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

# WRESZCIEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE 
j_ewid_1a$nazwa = toupper(j_ewid_1a$JPT_NAZWA_)
j_ewid_1a$nazwa = sub("ZDR[.]", "- ZDRÓJ", sub("ZEB[.]", "ZEBRZYDOWSKA", sub("N/", "NAD ", sub("KOWALEWO POMORSKIEG", "KOWALEWO POMORSKIE", sub("( |-)OB.{1,}", "", sub(" (G|M)$", "", sub("[(]W[)]|[(]M[)]", "", sub(" MIASTO$", "",  sub("^MIASTO ", "",  sub("-(G|GM)$", "", sub("-M$", "",  sub("M[.]", "", sub("-MIASTO", "",  sub("GM.{1,4}", "", sub("-GM.{1,}", "", sub("- .{1,}", "", j_ewid_1a$nazwa))))))))))))))))
j_ewid_1a$nazwa = str_trim(j_ewid_1a$nazwa, side = c("both", "left", "right"))
view(j_ewid_1a$nazwa)

#---------------------------- kontynuować tutej

j_ewid_1a$nazwa[grep("1061", j_ewid_1a$kod6a)] = "ŁÓDŹ"
j_ewid_1a$nazwa[grep("1261", j_ewid_1a$kod6a)] = "KRAKÓW"

# rodzaje obszarow administracyjnych
j_ewid_k8 = substring(j_ewid$JPT_KOD_JE, 8, 8)
j_ewid_k8_agg = aggregate(j_ewid_k8_agg, list(j_ewid$kod6), paste, collapse="_")
j_ewid_k8_agg1 = aggregate(j_ewid_k8_agg, list(kod6a = lodz_krakow), paste, collapse="_")

# jedna poprawka
j_ewid_k8_agg1$x[which(j_ewid_k8_agg1$x =="2_5_5_4")] = "5_5_4"
names(j_ewid_k8_agg1) = c("kod6a", "uni_gm", "types")

# tworzenie typów
Types = rep("Obszar wiejski", length(j_ewid_k8_agg1$types))
Types[grep("1", j_ewid_k8_agg1$types)] = "Obszar miejski"
Types[grep("5", j_ewid_k8_agg1$types)] = "Obszar miejsko-wiejski"
Types[grep("8", j_ewid_k8_agg1$types)] = "Dzielnice Warszawy"
Types[grep("9", j_ewid_k8_agg1$types)] = "Obszar miejski" #ŁÓDŹ and KRAKÓW boroughs
j_ewid_k8_agg1$types = factor(Types)


# Łączenie danych ---------------------------------------------------------

# połączenie typów z geometriami
j_ewid_1b = merge(j_ewid_1a, j_ewid_k8_agg1, by.x="kod6a", by.y="kod6a")

#  łączenie danych z geometrią
tury_geom = merge(j_ewid_1b, obie_tury, by.x="kod6a", by.y="t1_TERYT.gminy")

#sa bledy w tabeli tura2, utf-8 nie dziala
#dodać innych kandydatów w 1 turze?
tury_geom$t1_frekw = with(tury_geom, t1_Liczba.głosów.ważnych / 
                             t1_Liczba.wyborców.uprawnionych.do.głosowania)
tury_geom$t2_frekw = with(tury_geom, t2_Liczba.głosów.ważnych / 
                              t2_Liczba.wyborców.uprawnionych.do.głosowania)
tury_geom$t1_duda = with(tury_geom, t1_Andrzej.Sebastian.Duda / 
                            t1_Liczba.głosów.ważnych)
tury_geom$t2_duda = with(tury_geom, t2_Andrzej.Sebastian.Duda / 
                             t2_Liczba.głosów.ważnych)
tury_geom$t1_komo = with(tury_geom,  t1_Bronisław.Maria.Komorowski / 
                            t1_Liczba.głosów.ważnych)
tury_geom$t2_komo = with(tury_geom,  t2_Bronisław.Maria.Komorowski / 
                             t2_Liczba.głosów.ważnych)
