# wyniki senat

# pobieranie plików z danymi
download.file("https://sejmsenat2019.pkw.gov.pl/sejmsenat2019/data/csv/wyniki_gl_na_kand_po_obwodach_senat_csv.zip", "wyniki_gl_na_kand_po_obwodach_senat_csv.zip")
unzip("wyniki_gl_na_kand_po_obwodach_senat_csv.zip", exdir = "wyniki_senat")

install.packages("data.table")
library(data.table)
filenames = list.files("wyniki_senat", pattern="*.csv", full.names=TRUE)
data <- rbindlist(lapply(filenames,fread))

# problemy do rozwiązania
# - usuniecie z kazdego csv 6 ostatnich wierszy (w każdym pliku są takie same + puste)
# stworzenie funkcji która wyliczy i wybierze z ostatnich kolumn zwyciezce
# ostatnie kolumny dotycza kandydatow, trzeba bedzie zebrac liste kazdydatow z kazdej partii i 
# results_obwody = read.csv2("wyniki_senat/wyniki_gl_na_kand_po_obwodach_senat_csv", header=TRUE, fileEncoding="UTF-8", stringsAsFactors=FALSE)


# pobranie okręgów wyborczych do senatu
download.file("http://envirosolutions.pl/dane/OKWSenat.zip", "OKWSenat.zip")
unzip("OKWSenat.zip", files="OKWSenat/senat_2019.shp")
unzip("OKWSenat.zip", files="OKWSenat/senat_2019.dbf")
unzip("OKWSenat.zip", files="OKWSenat/senat_2019.prj")
unzip("OKWSenat.zip", files="OKWSenat/senat_2019.qpj")
unzip("OKWSenat.zip", files="OKWSenat/senat_2019.shx")

# moge poprobowac skrocic czas jeszcze bardziej ???????????/
# https://gis.stackexchange.com/questions/62292/how-to-speed-up-the-plotting-of-polygons-in-r

#wczytywanie granic obrębów wyborczych
library(sf)
install.packages("rmapshaper")
library(rmapshaper)
obwody = st_read("OKWSenat/senat_2019.shp", stringsAsFactors=FALSE) #takes a long time to load
# obwody = st_simplify(okregi, dTolerance = 1000)  # 2000 m
obwody_simp = ms_simplify(obwody, keep_shapes = TRUE, method = "dp")  # 2000 m

#agregacja wynikow wg obwodow
# obwody_agg = aggregate(je, list(je), head, n=1)
# merged <- merge(wyniki_obwody, obwody, by.x="kod6a", by.y="kod6a")