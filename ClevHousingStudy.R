# My Census Analysis Code

v20 <- load_variables(2019, "acs5", cache = TRUE)
view(v20)


# 1 population 2015 to 2019/2020 - percent change past five years in Cities in the Cuyahoga County
# population density 
# B01001_001 Estimate!!Total:

cnty_citypop <- get_acs(
  geography = "county subdivision", 
  variables = c(totalpop="B01001_001"), 
  state = "OH", 
  county = "Cuyahoga",
  year = 2019,
  output = "wide"
)%>%
  mutate(NAME = str_remove(NAME, ", Cuyahoga County, Ohio"))

cnty_citypop
cnty_citypop = cnty_citypop[cnty_citypop$GEOID > "3903500000",]
cnty_citypop


cnty_citypop15 <- get_acs(
  geography = "county subdivision", 
  variables = c(totalpop15="B01001_001"), 
  state = "OH", 
  county = "Cuyahoga",
  year = 2015,
  output = "wide"
)%>%
  mutate(NAME = str_remove(NAME, ", Cuyahoga County, Ohio"))

cnty_citypop15

#---------------- how to compare two ACH --------------------------------------------------
mypop <- merge(cnty_citypop15, cnty_citypop, by = "GEOID") # NA's match, so 6 rows
mypop$popchange <-  



# 2 Race distribution


# 3 Low moderate income family - house hold income, family income, per capita by city
# median house hold income
# B19013_001 Estimate!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars)

myvar = B19013_001

cnty_city <- get_acs(
  geography = "county subdivision", 
  variables = B19013_001, 
  state = "OH", 
  county = "Cuyahoga",
  year = 2019
  )%>%
  mutate(NAME = str_remove(NAME, ", Cuyahoga County, Ohio"))

cnty_city

ggplot(cnty_city, aes(x = estimate, y = reorder(NAME, estimate))) + 
  geom_point(size = 3, color = "darkgreen") + 
  labs(title = "Median household income", 
       subtitle = "Cities in Cuyahoga", 
       x = "", 
       y = "ACS estimate") + 
  theme_minimal(base_size = 12.5) + 
  scale_x_continuous(labels = scales::dollar)


# 4 Low mod income cencentation in latest layer HUD layer??


# 5 Housing stock 
#  age


# 6 historic district and building





