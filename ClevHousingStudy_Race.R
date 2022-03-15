# My Census Analysis Code
library(ggplot2)

v20 <- load_variables(2019, "acs5", cache = TRUE)
view(v20)

dv20 <- load_variables(2020, "pl", cache = TRUE)
view(dv20)

# 1 Race 2015 to 2019/2020 - percent change past five years in Cities in the Cuyahoga County
#-------------------------- 2 Race distribution ----------------------------------


# B03002_007 Estimate!!Total:!!Not Hispanic or Latino:!!Native Hawaiian and Other Pacific Islander alone 
# HISPANIC OR LATINO ORIGIN BY RACE

race_vars <- c(
  White = "B03002_003",
  Black = "B03002_004",
  Native = "B03002_005",
  Asian = "B03002_006",
  NotHLPI = "B03002_007",
  Hispanic = "B03002_012"
)

OH_race <- get_acs(
  geography = "county subdivision",
  state = "OH",
  county =  'Cuyahoga',
  variables = race_vars,
  summary_var = "B03002_001"
)%>%
  mutate(NAME = str_remove(NAME, ", Cuyahoga County, Ohio"))


## ----view-summary-variable------------------------------------------------------
OH_race
OH_race = OH_race[OH_race$GEOID > "3903500000",]



## ----mutate-and-select----------------------------------------------------------
OH_race_percent_cle <- OH_race %>%
  filter(GEOID == "3903516000") %>%
  mutate(percent = 100 * (estimate / summary_est)) %>%
  select(NAME, variable, percent)
OH_race_percent_cle


OH_race_percent <- OH_race %>%
  mutate(percent = 100 * (estimate / summary_est)) %>%
  select(NAME, variable, percent)


## ----view-percent---------------------------------------------------------------
OH_race_percent


## ----largest-group--------------------------------------------------------------
largest_group <- OH_race_percent %>%
  group_by(NAME) %>%
  filter(percent == max(percent))


## ----view-largest-group---------------------------------------------------------
largest_group


#----- time trend ----------------------------------------------------------------

years <- 2010:2019
names(years) <- years
years

race_vars <- c(
  White = "B03002_003",
  Black = "B03002_004",
  Native = "B03002_005",
  Asian = "B03002_006",
  NonHIPI = "B03002_007",
  Hispanic = "B03002_012"
)

cnty_value <- map_dfr(years, ~{
  get_acs(
    geography = "county",
    variables = race_vars,
    summary_var = "B03002_001",
    state = "OH",
    county = "Cuyahoga",
    year = .x,
    survey = "acs5"
  ) %>%
    mutate(percent = round(100 * (estimate / summary_est),2))
}, .id = "year")

#cnty_value%>% 
#  mutate(year = as.factor(year))
cnty_value

g1 <- ggplot(cnty_value, aes(x=year, y= percent, colour=variable)) + 
  geom_line(aes(group = variable))+
  geom_text(data = cnty_value, aes(x= year,label= percent), size=3, color="black")+
  theme_minimal(base_size = 10) + 
  scale_y_continuous(labels = scales::number) + 
  labs(title = "Race Distribution in Cuyahoga Couty, Ohio from 2010 to 2019",
       x = "Year",
       y = "ACS estimate (%)",
       caption = "Source: ACH 5 Year Estimae from 2010 to 2019")
g1


cle_value <- map_dfr(years, ~{
  get_acs(
    geography = "county subdivision",
    variables = race_vars,
    summary_var = "B03002_001",
    state = "OH",
    county = "Cuyahoga",
    year = .x,
    survey = "acs5"
  )%>% 
    filter (GEOID == "3903516000")%>% 
    mutate(NAME = str_remove(NAME, ", Cuyahoga County, Ohio"))%>%
    mutate(percent = round(100 * (estimate / summary_est),2)) 
}, .id = "year")

cle_value

g2 <- ggplot(cle_value, aes(x=year, y= percent, colour=variable)) + 
  geom_line(aes(group = variable))+
  geom_text(data = cle_value, aes(x= year,label= percent), size=3, color="black")+
  theme_minimal(base_size = 10) + 
  scale_y_continuous(labels = scales::number) + 
  labs(title = "Race Distribution in Cleveland, Cuyahoga Couty, Ohio from 2010 to 2019",
       x = "Year",
       y = "ACS estimate (%)",
       caption = "Source: ACH 5 Year Estimae from 2010 to 2019")

g2

g3 <- ggplot(cle_value, aes(x=year, y= estimate, colour=variable)) + 
  geom_line(aes(group = variable))+
  geom_text(data = cle_value, aes(x= year,label= estimate), size=3, color="black")+
  theme_minimal(base_size = 10) + 
  scale_y_continuous(labels = scales::number) + 
  labs(title = "Race Distribution in Cleveland, Cuyahoga Couty, Ohio from 2010 to 2019",
       x = "Year",
       y = "ACS estimate",
       caption = "Source: ACH 5 Year Estimae from 2010 to 2019")


g3

#install.packages("patchwork")
#library(patchwork)
g2 +g3

