library(tidycensus)
library(tidyverse)
library(plotly)
library(censusapi)
library(censusGeography)
library(ggplot2)
library(DT)
library(dplyr)

#install.packages("palmerpenguins")
library(palmerpenguins)


install.packages(c("here", "sf", "psych", "mclust", "rmapshaper", "reactable", "magrittr","leaflet"))
install.packages("GPArotation")

library(here)
library(sf)
library(psych)
library(mclust)
library(rmapshaper)
library(reactable)
library(magrittr)
library(leaflet)
library(GPArotation)
# install.packages("ggrepel")
library(ggrepel)
# install.packages("patchwork")
library(patchwork)


# my census API key: "75789d63464567dcba5b8edb7500fe6148ff3440"

# Add key to .Renviron
Sys.setenv(CENSUS_KEY="75789d63464567dcba5b8edb7500fe6148ff3440")

# Reload .Renviron
readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console

Sys.getenv("CENSUS_KEY")


apis <- listCensusApis()
View(apis)

#------------- cleaning City 
# %>% mutate(NAME = str_remove(NAME, ", Cuyahoga County, Ohio")
# cnty_cityrace = cnty_cityrace[cnty_cityrace$GEOID > "3903500000",]


