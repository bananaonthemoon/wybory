library(mapdeck)
library(tidyverse)

key = 'pk.eyJ1IjoibWFya3JvOCIsImEiOiJja2QwODBzdGEwOGpwMnpvMThwdmFqZjNyIn0.BltIhc4-ZufZxHhOyUo30g'

mapdeck(token = key)

url <- 'https://raw.githubusercontent.com/bananaonthemoon/wybory/master/dane/pobrane/tura1/wyniki_gl_na_kand_po_wojewodztwach_proc_utf8.csv'
dane_wybory <- read.csv2(url, encoding = "UTF-8")
flights$info <- paste0("<b>",flights$airport1, " - ", flights$airport2, "</b>")

mapdeck(token = key, style = mapdeck_style('dark')) %>%
  add_arc(
    data = flights
    , origin = c("start_lon", "start_lat")
    , destination = c("end_lon", "end_lat")
    , stroke_from = "airport1"
    , stroke_to = "airport2"
    , tooltip = "info"
    , layer_id = 'arclayer'
  )

tinytex::install_tinytex()
