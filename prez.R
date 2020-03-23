library(tidyverse)
library(readxl)

# PIERWSZA TURA
# pobranie i wczytanie danych

#download.file("https://prezydent2015.pkw.gov.pl/prezydent_2015_tura1.zip", "prezydent_2015_tura1.zip")
#unzip("prezydent_2015_tura1.zip", files="prezydent_2015_tura1.csv")
tura1 <- read.csv2("prezydent_2015_tura1.csv", header=TRUE, fileEncoding="CP1250", stringsAsFactors=FALSE)

#poprawienie formatu kodu TERYT
tura1[[3]] <- formatC(tura1[[3]], width=6, format="d", flag="0")

# filtrowanie obwodow zagranicznych + zsumowanie wynikow do kazdego terytu
tura1 = tura1 %>%
  filter(TERYT.gminy != "149901" & TERYT.gminy != "229901") %>%
  group_by(TERYT.gminy) %>%
  summarise_if(is.numeric, sum) %>%
  select(-Numer.obwodu)

# DRUGA TURA
# pobranie i wczytanie danych

download.file("https://prezydent2015.pkw.gov.pl/wyniki_tura2.zip", "wyniki_tura2.zip")
unzip("wyniki_tura2.zip", files="wyniki_tura2.xls")
#xls = read_excel("wyniki_tura2.xls") #teryt 0??
#write.csv2(xls,"wyniki_tura2.csv")
#plik jest niepoprawnie zapisany, zapisane od nowa z poziomu excela
tura2 = read.csv2("wyniki_tura2.csv", header=TRUE, fileEncoding="CP1250", stringsAsFactors=FALSE)

#poprawienie formatu kodu TERYT
tura2[[3]] <- formatC(tura2[[3]], width=6, format="d", flag="0")

# filtrowanie obwodow zagranicznych + zsumowanie wynikow do kazdego terytu
tura2 = tura2 %>%
  filter(TERYT.gminy != "149901" & TERYT.gminy != "229901") %>%
  group_by(TERYT.gminy) %>%
  summarise_if(is.numeric, sum) %>%
  select(-Numer.obwodu)

#dopisanie do kolumn t1_/t2_
names(tura1) <- paste("t1_", names(tura1), sep="")
names(tura2) <- paste("t2_", names(tura2), sep="")

obie_tury = merge(tura1, tura2, by.x="t1_TERYT.gminy", by.y="t2_TERYT.gminy") # po tym jak poprawiłam znaki to wychodzi błąd

#____________________________
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

#uproszczenie geometrii
j_ewid_simp = ms_simplify(j_ewid, keep_shapes = TRUE, method = "dp", keep = 0.2) 
#ładowanie tego zajmuje LATA
#problem z encoding 
#użyta inna funkcja

#to jest bardziej ogarnięta wersja
#generalnie to chciałam jak najmniej kodu kopiować, tylko zrobić to samo używając np. tidyverse,
#ale raczej słabo poszło (np. takie aggregate), bo funkcje z base są często prostsze i zajmują mniej miejsca, tak więc wyszło jak wyszło.

# porządkowanie obszarów administracyjnych
j_ewid_simp$kod6 = str_sub(j_ewid_simp$JPT_KOD_JE, 1, 6) 
# grupowanie wg terytu
j_ewid_simp_agg <- aggregate(j_ewid_simp, list(j_ewid_simp$kod6), head, n=1)
# usuwa wiekszosc kolumn #o co chodzi z sf_coludmn?, nie wystarczy geometry?
je1 <- j_ewid_simp_agg[, c("kod6", "JPT_NAZWA_", attr(j_ewid_simp_agg, "sf_column"))]

# to możnaby jakoś inaczej przedstawić, ale ciągle jakieś błędy wychodzily
lodz_krakow <- je1$kod6
lodz_krakow[substring(lodz_krakow, 1, 4) %in% "1061"] <- "106101"
lodz_krakow[substring(lodz_krakow, 1, 4) %in% "1261"] <- "126101"
je1a <- aggregate(je1, list(kod6a = lodz_krakow), head, n = 1)

# jest sens użyć str_replace? #m koscierzyna trzeba będzie poprawić
je1a$name <- sub("98-220 ", "", sub("\\-GMINA$", "", sub("\\-G$", "", sub(" W GMINIE MIEJSKO-WIEJSKIEJ$", "", sub("OBSZAR WIE$", "", sub(" \\(W\\)$", "", sub("-$", "", sub("WIEŚ$", "", sub(" \\(M\\)$", "", sub("_GM$", "", sub("^GM. ", "", sub("^M_", "M. ", sub("^GM_", "", sub("^ ", "", sub(" $", "", sub(" -", "", sub(" - ", "", sub(" GMINA", "", sub("GMINA ", "", sub("MIASTO", "", sub("OBSZAR WIEJSKI", "", toupper(je1a$JPT_NAZWA_))))))))))))))))))))))
je1a$name[grep("1061", je1a$kod6a)] <- "ŁÓDŹ"
je1a$name[grep("1261", je1a$kod6a)] <- "KRAKÓW"

# rodzaje obszarow administracyjnych
je_k8 <- substring(j_ewid_simp$JPT_KOD_JE, 8, 8)
je_k8_agg <- aggregate(je_k8, list(j_ewid_simp$kod6), paste, collapse="_")
je_k8_agg1 <- aggregate(je_k8_agg, list(kod6a = lodz_krakow), paste, collapse="_")

# jedna poprawka
je_k8_agg1$x[which(je_k8_agg1$x =="2_5_5_4")] <- "5_5_4"
names(je_k8_agg1) <- c("kod6a", "uni_gm", "types")

# tworzenie typów
Types <- rep("Obszar wiejski", length(je_k8_agg1$types))
Types[grep("1", je_k8_agg1$types)] <- "Obszar miejski"
Types[grep("5", je_k8_agg1$types)] <- "Obszar miejsko-wiejski"
Types[grep("8", je_k8_agg1$types)] <- "Dzielnice Warszawy"
Types[grep("9", je_k8_agg1$types)] <- "Obszar miejski" #ŁÓDŹ and KRAKÓW boroughs
je_k8_agg1$types <- factor(Types)

# polaczenie typow z geometriami
je1b <- merge(je1a, je_k8_agg1, by.x="kod6a", by.y="kod6a")

#  łączenie danych z geometrią
tury_geom <- merge(je1b, obie_tury, by.x="kod6a", by.y="t1_TERYT.gminy")

#sa bledy w tabeli tura2, utf-8 nie dziala
#dodać innych kandydatów w 1 turze?
tury_geom$t1_frekw <- with(tury_geom, t1_Liczba.głosów.ważnych / 
                             t1_Liczba.wyborców.uprawnionych.do.głosowania)
tury_geom$t2_frekw <- with(tury_geom, t2_Liczba.głosów.ważnych / 
                              t2_Liczba.wyborców.uprawnionych.do.głosowania)
tury_geom$t1_duda <- with(tury_geom, t1_Andrzej.Sebastian.Duda / 
                            t1_Liczba.głosów.ważnych)
tury_geom$t2_duda <- with(tury_geom, t2_Andrzej.Sebastian.Duda / 
                             t2_Liczba.głosów.ważnych)
tury_geom$t1_komo <- with(tury_geom,  t1_Bronisław.Maria.Komorowski / 
                            t1_Liczba.głosów.ważnych)
tury_geom$t2_komo <- with(tury_geom,  t2_Bronisław.Maria.Komorowski / 
                             t2_Liczba.głosów.ważnych)
