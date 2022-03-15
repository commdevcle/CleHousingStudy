#: https://walker-data.com/census-r/mapping-census-data-with-r.html


tm_shape(hennepin_black, 
         projection = sf::st_crs(26915)) + 
  tm_polygons(col = "percent",
              style = "jenks",
              n = 5,
              palette = "Purples",
              title = "ACS estimate",
              legend.hist = TRUE) + 
  tm_layout(title = "Percent Black\nby Census tract",
            frame = FALSE,
            legend.outside = TRUE,
            bg.color = "grey70",
            legend.hist.width = 5,
            fontfamily = "Verdana")