# My Census Analysis Code

v20 <- load_variables(2019, "acs5", cache = TRUE)
view(v20)


myvar <- "B01001_001"

## ----query-by-county-tract ------------------------------------------------------------
cnty_tr <- get_acs(
  geography = "tract", 
  variables = myvar, 
  state = "OH", 
  county = "Cuyahoga",
  year = 2019
)

cnty_tr

## ----query-by-county-city ------------------------------------------------------------

cnty_city <- get_acs(
  geography = "county subdivision", 
  variables = myvar, 
  state = "OH", 
  county = "Cuyahoga",
  year = 2019,
  output = "wide"
)

cnty_city

cnty_city15 <- get_acs(
  geography = "county subdivision", 
  variables = myvar, 
  state = "OH", 
  county = "Cuyahoga",
  year = 2015,
  output = "wide"
)

cnty_city







# Table reference

# B01001_001 Estimate!!Total:
# B25026_001 Estimate!!Total population in occupied housing units:
# B26101_214 Estimate!!Total:!!Household population
# B26106_010 Estimate!!Total:!!Group quarters population:!!Bachelor's degree or higher
# B10059_002 Estimate!!Total:!!Income in the past 12 months below poverty level:



# B25002_003 vacancy
# geography = "tract", 
# B07013_008 Estimate!!Total:!!Moved within same county:!!Householder lived in owner-occupied housing units
#	B07013_009 Estimate!!Total:!!Moved within same county:!!Householder lived in renter-occupied housing units

#B06011_001 Estimate!!Median income in the past 12 months --!!Total:


# B07013_011 Estimate!!Total:!!Moved from different county within same state:!!Householder lived in owner-occupied housing unitsGEOGRAPHICAL MOBILITY IN THE PAST YEAR BY TENURE FOR CURRENT RESIDENCE IN THE UNITED STATES
# B07013_012 Estimate!!Total:!!Moved from different county within same state:!!Householder lived in renter-occupied housing units
# B07013_014 Estimate!!Total:!!Moved from different state:!!Householder lived in owner-occupied housing units

tvar = "B06011_001"








# https://www.rdocumentation.org/packages/tidycensus/versions/1.1/topics/get_acs 
get_acs(
  geography,
  variables = NULL,
  table = NULL,
  cache_table = FALSE,
  year = 2019,
  endyear = NULL,
  output = "tidy",
  state = NULL,
  county = NULL,
  zcta = NULL,
  geometry = FALSE,
  keep_geo_vars = FALSE,
  shift_geo = FALSE,
  summary_var = NULL,
  key = NULL,
  moe_level = 90,
  survey = "acs5",
  show_call = FALSE,
  ...
)



# NOT RUN {
library(tidycensus)
library(tidyverse)


install.packages("viridis")
library(viridis)

tarr <- get_acs(geography = "tract", variables = "B19013_001",
                state = "OH", county = "CUYAHOGA", geometry = TRUE)

ggplot(tarr, aes(fill = estimate, color = estimate)) +
  geom_sf() +
  coord_sf(crs = 26914) +
  scale_fill_viridis(option = "magma") +
  scale_color_viridis(option = "magma")


Oh <- get_acs(geography = "county subdivision", variables = "B19013_001", state = "OH", county = "Cuyahoga")%>%
  mutate(NAME = gsub(", Cuyahoga County, Ohio", "", NAME))
Oh


myplot <-  ggplot(oh, aes(x = estimate, y = reorder(NAME, estimate))) +
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(color = "red", size = 3) +
  labs(title = "Household income by county in Cuyahoga County, Ohio",
       subtitle = "2015-2019 American Community Survey",
       y = "",
       x = "ACS estimate (bars represent margin of error)")

myplot

# }



## ----summary-variable-----------------------------------------------------------
race_vars <- c(
  White = "B03002_003",
  Black = "B03002_004",
  Native = "B03002_005",
  Asian = "B03002_006",
  HIPI = "B03002_007",
  Hispanic = "B03002_012"
)

OH_race <- get_acs(
  geography = "county subdivision",
  state = "OH",
  county ="CuYAHOGA",
  variables = race_vars,
  summary_var = "B03002_001"
)


## ----view-summary-variable------------------------------------------------------
OH_race


## ----maine-data-----------------------------------------------------------------
maine_income <- get_acs(
  state = "Ohio",
  geography = "county subdivision",
  variables = c(hhincome = "B19013_001")
) %>%
  mutate(NAME = str_remove(NAME, " County, Maine"))



## ----view-maine-data------------------------------------------------------------
maine_income %>% arrange(desc(moe))


total_population_20 <- get_decennial(
  geography = "state", 
  variables = "P1_005N",
  year = 2020
)

total_population_20



## ----maine-data-----------------------------------------------------------------
# B19013_001 Estimate!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars)
# B10059_002 Estimate!!Total:!!Income in the past 12 months below poverty level:

vart = "B10059_002"

## ----maine-data-----------------------------------------------------------------
# B19013_001 Estimate!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars)

maine_income <- get_acs(
  geography = "county subdivision",
  variables = c(hhincome = "B19013_001"),
  state = "Ohio",
  county = "Cuyahoga",
  year = 2019
)  %>%
  mutate(NAME = str_remove(NAME, ", Cuyahoga County, Ohio"))

maine_income16 <- get_acs(
  geography = "county subdivision",
  variables = c(hhincome = "B19013_001"),
  state = "Ohio",
  county = "Cuyahoga",
  year = 2016
)  %>%
  mutate(NAME = str_remove(NAME, ", Cuyahoga County, Ohio"))


maine_income
maine_income16

## ----view-maine-data------------------------------------------------------------
#estimate
maine_income %>% arrange(desc(estimate))
maine_income16 %>% arrange(desc(estimate))
# maine_income %>% arrange(desc(moe))

maine_income
maine_income16


## ----moe-plot-----------------------------------------------------
tplot <- ggplot(maine_income, aes(x = estimate, y = reorder(NAME, estimate))) +
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(data=maine_income, size = 3, color = "red") +
  geom_errorbarh(data=maine_income16, aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(data=maine_income16, size = 3, color = "darkgreen") +
  labs(title = "Median household income",
       subtitle = "Cities in Cuyahoga County, Ohio",
       x = "ACS 2016 (green)  ACS 2019 (red) estimate with margin of error",
       y = "") +
  scale_x_continuous(labels = scales::dollar)
tplot


# B25037_001 Estimate!!Median year structure built --!!Total:MEDIAN YEAR STRUCTURE BUILT BY TENURE
# B25037_003 Estimate!!Median year structure built --!!Renter occupied MEDIAN YEAR STRUCTURE BUILT BY TENURE
# B25037_002 Estimate!!Median year structure built --!!Owner occupied MEDIAN YEAR STRUCTURE BUILT BY TENURE
# B25035_001 Estimate!!Median year structure built MEDIAN YEAR STRUCTURE BUILT

maine_income <- get_acs(
  geography = "county subdivision",
  variables = c(mystructurbuilt = "B25037_001"),
  state = "Ohio",
  county = "Cuyahoga",
  year = 2019
)  %>%
  mutate(NAME = str_remove(NAME, ", Cuyahoga County, Ohio"))

maine_income16 <- get_acs(
  geography = "county subdivision",
  variables = c(mystructurbuilt = "B25037_001"),
  state = "Ohio",
  county = "Cuyahoga",
  year = 2016
)  %>%
  mutate(NAME = str_remove(NAME, ", Cuyahoga County, Ohio"))


maine_income
maine_income16

## ----view-maine-data------------------------------------------------------------
#estimate
maine_income %>% arrange(desc(estimate))
maine_income16 %>% arrange(desc(estimate))
# maine_income %>% arrange(desc(moe))

maine_income
maine_income16


## ----moe-plot-----------------------------------------------------
tplot <- ggplot(aes(x = estimate, y = reorder(NAME))) +
  # geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(data=maine_income, size = 3, color = "red") +
  geom_errorbarh(data=maine_income16, aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(data=maine_income16, size = 3, color = "darkgreen") +
  labs(title = "Median household income",
       subtitle = "Cities in Cuyahoga County, Ohio",
       x = "ACS 2016 (green)  ACS 2019 (red) estimate with margin of error",
       y = "") +
  scale_x_continuous(labels = scales::dollar)
tplot



utah <- get_estimates(
  geography = "county",
  product = "characteristics",
  breakdown = c("SEX", "AGEGROUP"),
  breakdown_labels = TRUE,
  state = "OH",
  year = 2019
) 

utah



## ----prep-utah-data-------------------------------------------------------------
utah_filtered <- filter(utah, str_detect(AGEGROUP, "^Age"), 
                        SEX != "Both sexes") %>%
  mutate(value = ifelse(SEX == "Male", -value, value))

utah_filtered


## ----first-pyramid------------------------------------------------
ggplot(utah_filtered, aes(x = value, y = AGEGROUP, fill = SEX)) +
  geom_col()


## ----formatted-pyramid--------------------------------------------
utah_pyramid <- ggplot(utah_filtered, aes(x = value, y = AGEGROUP, fill = SEX)) +
  geom_col(width = 0.95, alpha = 0.75) +
  theme_minimal(base_family = "Verdana") +
  scale_x_continuous(labels = function(y) paste0(abs(y / 1000), "k")) +
  scale_y_discrete(labels = function(x) gsub("Age | years", "", x)) +
  scale_fill_manual(values = c("darkred", "navy")) +
  labs(x = "",
       y = "2019 Census Bureau population estimate",
       title = "Population structure in Utah",
       fill = "",
       caption = "Data source: US Census Bureau population estimates & tidycensus R package")

utah_pyramid


## I need this 
## ----beeswarm---------------------------------------
install.packages("ggbeeswarm")

library(ggbeeswarm)

ny_race_income <- get_acs(
  geography = "tract",
  state = "OH",
  county = "Cuyahoga",
  variables = c(White = "B03002_003",
                Black = "B03002_004",
                Asian = "B03002_006",
                Hispanic = "B03002_012"),
  summary_var = "B19013_001"
) %>%
  group_by(GEOID) %>%
  filter(estimate == max(estimate, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(estimate != 0)

ggplot(ny_race_income, aes(x = variable, y = summary_est, color = summary_est)) +
  geom_quasirandom(alpha = 0.5) +
  coord_flip() +
  theme_minimal() +
  scale_color_viridis_c(guide = FALSE) +
  scale_y_continuous(labels = scales::dollar) +
  labs(x = "Largest group in Census tract",
       y = "Median household income",
       title = "Household income distribution by largest racial/ethnic group",
       subtitle = "Census tracts, New York City",
       caption = "Data source: 2015-2019 ACS")


## ----geofacet-pyramids------------------------------
install.packages("geofacet")

library(geofacet)

us_pyramid_data <- get_estimates(
  geography = "state",
  product = "characteristics",
  breakdown = c("SEX", "AGEGROUP"),
  breakdown_labels = TRUE,
  year = 2019
) %>%
  filter(str_detect(AGEGROUP, "^Age"),
         SEX != "Both sexes") %>%
  group_by(NAME) %>%
  mutate(prop = value / sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(prop = ifelse(SEX == "Male", -prop, prop))

ggplot(us_pyramid_data, aes(x = prop, y = AGEGROUP, fill = SEX)) +
  geom_col(width = 1) +
  theme_minimal() +
  scale_fill_manual(values = c("darkred", "navy")) +
  facet_geo(~NAME, grid = "us_state_with_DC_PR_grid2",
            label = "code") +
  theme(axis.text = element_blank(),
        strip.text.x = element_text(size = 8)) +
  labs(x = "",
       y = "",
       title = "Population structure by age and sex",
       fill = "",
       caption = "Data source: US Census Bureau population estimates & tidycensus R package")


