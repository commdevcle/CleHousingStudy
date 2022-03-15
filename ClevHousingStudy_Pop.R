# My Census Analysis Code
library(ggplot2)


v20 <- load_variables(2019, "acs5", cache = TRUE)
view(v20)

#------------------------------------------------------------------------------------------------------
# 1 population 2015 to 2019/2020 - percent change past five years in Cities in the Cuyahoga County
# population density 
# B01001_001 Estimate!!Total:

myvar = "B01001_001"

cnty_citypop <- get_acs(
  geography = "county subdivision", 
  variables = c(totalpop=myvar), 
  state = "OH", 
  county = "Cuyahoga",
  year = 2019,
  output = "wide"
)%>%
  mutate(NAME = str_remove(NAME, ", Cuyahoga County, Ohio"))

cnty_citypop
cnty_citypop = cnty_citypop[cnty_citypop$GEOID > "3903500000",]
cnty_citypop


cnty_citypop10 <- get_acs(
  geography = "county subdivision", 
  variables = c(totalpop10=myvar), 
  state = "OH", 
  county = "Cuyahoga",
  year = 2010,
  output = "wide"
)

cnty_citypop10 =select(cnty_citypop10, 1, 3:4)
cnty_citypop10

cnty_citypop15 <- get_acs(
  geography = "county subdivision", 
  variables = c(totalpop15=myvar), 
  state = "OH", 
  county = "Cuyahoga",
  year = 2015,
  output = "wide"
)

cnty_citypop15 =select(cnty_citypop15, 1, 3:4)
cnty_citypop15


#---------------- how to compare two ACH --------------------------------------------------
mypop <- merge(cnty_citypop15, cnty_citypop, by = "GEOID") # NA's match, so 6 rows
mypop

mypop$popchange <-round((mypop$totalpopE-mypop$totalpop15E)/mypop$totalpopE * 100, 2)
mypop


mypop10 <- merge(cnty_citypop10, cnty_citypop, by = "GEOID") # NA's match, so 6 rows
mypop10

mypop10$popchange <-round((mypop10$totalpopE-mypop10$totalpop10E)/mypop$totalpopE * 100, 2)
mypop10



myplot <- ggplot(mypop, aes(x = popchange, y = reorder(NAME, popchange))) + 
  geom_col() + 
  labs(title = "Population Percent Change from 2015 to 2019", 
       subtitle = "Cities in Cuyahoga", 
       x = "Percentage Change (%)", 
       y = "") + 
  theme_minimal(base_size = 8)+
  annotate("text", x=-1.38, y = "Cleveland City",colour = "red", label = "Cleveland City -1.38%")+
  geom_hline(yintercept="Cleveland City", color="orange", size=1) 

myplot


myplot10 <- ggplot(mypop10, aes(x = popchange, y = reorder(NAME, popchange))) + 
  geom_col() + 
  labs(title = "Population Percent Change from 2010 to 2019", 
       subtitle = "Cities in Cuyahoga", 
       x = "Percentage Change (%)", 
       y = "") + 
  theme_minimal(base_size = 8)+
  geom_vline(xintercept=-6.21, color="orange", size=1) 

myplot10




mytable <- merge(mypop, mypop10, by = "GEOID") 

#-------------------- select fields ------------------------------------------------
#dplyr


mytable <- mytable %>%
  select(NAME.x, totalpop10E, totalpop15E, totalpopE.y, popchange.y, popchange.x)

names(mytable)[1] <- 'City'
names(mytable)[2] <- 'TotalPop2010'
names(mytable)[3] <- 'TotalPop2015'
names(mytable)[4] <- 'TotalPop2019'
names(mytable)[5] <- '%Change10-19'
names(mytable)[6] <- '%Change15-19'


#---------------------------------------------------------------------------------------------- 
# Visualizing over time population total population

total_population_10 <- get_decennial(
  geography = "county subdivision", 
  variables = c(totalpop="P001001"),
  state = "OH",
  county = "Cuyahoga",
  year = 2010
) %>% 
  filter (GEOID == "3903516000")%>% 
  mutate(NAME = str_remove(NAME, ", Cuyahoga County, Ohio"))


total_population_10


years <- 2010:2019
names(years) <- years
years
cnty_value <- map_dfr(years, ~{
  get_acs(
    geography = "county",
    variables = "B01001_001",
    state = "OH",
    county = "Cuyahoga",
    year = .x,
    survey = "acs5"
  )
}, .id = "year")


g1 <- ggplot(cnty_value, aes(x = year, y = estimate, group = 1)) + 
  geom_line() + 
  geom_point()+
  geom_text(data = cnty_value, aes(x= year,label= estimate), color="black")+
  theme_minimal(base_size = 10) + 
  scale_y_continuous(labels = scales::number) + 
  labs(title = "Populaton change in Cuyahoga Couty, Ohio from 2010 to 2019",
       x = "Year",
       y = "ACS 5 year estimate",
       caption = "Source: ACH 5 Year Estimae from 2010 to 2019")


cle_value <- map_dfr(years, ~{
  get_acs(
    geography = "county subdivision",
    variables = "B01001_001",
    state = "OH",
    county = "Cuyahoga",
    year = .x,
    survey = "acs5"
  )%>% 
    filter (GEOID == "3903516000")%>% 
    mutate(NAME = str_remove(NAME, ", Cuyahoga County, Ohio")) 
}, .id = "year")

cle_value

g2 <- ggplot(cle_value, aes(x = year, y = estimate, group = 1)) + 
  geom_line() + 
  geom_point()+
  geom_text(data = cle_value, aes(x= year,label= estimate), color="black")+
  theme_minimal(base_size = 10) + 
  scale_y_continuous(labels = scales::number) + 
  labs(title = "Populaton change in Cleveland, Cuyahoga Couty, Ohio from 2010 to 2019",
       x = "Year",
       y = "ACS 5 year estimate",
       caption = "Source: ACH 5 Year Estimae from 2010 to 2019")


#--- combine two data in one plot

g3 <- ggplot(NULL, aes(x= year, y = estimate, group = 1)) +    
  geom_line(data = cnty_value, col = "red") + 
  geom_point(data = cnty_value, col = "black")+
  #geom_text(data = cnty_value, aes(x= year,label= estimate), color="black")+
  geom_line(data = cle_value, col = "blue")+ 
  geom_point(data = cle_value, col = "black") +
  #geom_text(data = cle_value, aes(x= year,label= estimate), color="black")+
  theme_minimal(base_size = 10) + 
  scale_y_continuous(labels = scales::number) + 
  labs(title = "Populaton change in Cleveland and Cuyahoga County, Ohio from 2010 to 2019",
       x = "Year",
       y = "ACS estimate",
       caption = "Source: ACH 5 Year Estimae from 2010 to 2019")

g3         

#install.packages("patchwork")
library(patchwork)

g2 + g1

g3 | (g1 / g2)



#----------------------------------------------------------------------------------------------------------
## 2 median age
#------- median age 	B01002_001
years <- 2010:2019
names(years) <- years
years

cnty_value <- map_dfr(years, ~{
  get_acs(
    geography = "county",
    variables = "B01002_001",
    state = "OH",
    county = "Cuyahoga",
    year = .x,
    survey = "acs1"
  )
}, .id = "year")

cnty_value

g1 <- ggplot(cnty_value, aes(x = year, y = estimate, group = 1)) + 
  geom_line() + 
  geom_point()+
  geom_text(data = cnty_value, aes(x= year,label= estimate), color="black")+
  theme_minimal(base_size = 10) + 
  scale_y_continuous(labels = scales::number) + 
  labs(title = "Median Age in Cuyahoga Couty, Ohio from 2010 to 2019",
       x = "Year",
       y = "ACS 1 year estimate-median age",
       caption = "Source: ACH 1 Year Estimae from 2010 to 2019")


cle_value <- map_dfr(years, ~{
  get_acs(
    geography = "county subdivision",
    variables = "B01002_001",
    state = "OH",
    county = "Cuyahoga",
    year = .x,
    survey = "acs1"
  )%>% 
    filter (GEOID == "3903516000")%>% 
    mutate(NAME = str_remove(NAME, ", Cuyahoga County, Ohio")) 
}, .id = "year")

cle_value

g2 <- ggplot(cle_value, aes(x = year, y = estimate, group = 1)) + 
  geom_line() + 
  geom_point()+
  geom_text(data = cle_value, aes(x= year,label= estimate), color="black")+
  theme_minimal(base_size = 10) + 
  scale_y_continuous(labels = scales::number) + 
  labs(title = "Median Age in Cleveland, Cuyahoga Couty, Ohio from 2010 to 2019",
       x = "Year",
       y = "ACS 1 year estimate- median age",
       caption = "Source: ACH 1 Year Estimae from 2010 to 2019")


#--- combine two data in one plot

g3 <- ggplot(NULL, aes(x= year, y = estimate, group = 1)) +    
  geom_line(data = cnty_value, col = "red") + 
  geom_point(data = cnty_value, col = "black")+
  #geom_text(data = cnty_value, aes(x= year,label= estimate), color="black")+
  geom_line(data = cle_value, col = "blue")+ 
  geom_point(data = cle_value, col = "black") +
  #geom_text(data = cle_value, aes(x= year,label= estimate), color="black")+
  theme_minimal(base_size = 10) + 
  scale_y_continuous(labels = scales::number) + 
  labs(title = "Median Age in Cleveland and Cuyahoga County, Ohio from 2010 to 2019",
       x = "Year",
       y = "ACS 1 year estimate-median age",
       caption = "Source: ACH 1 Year Estimae from 2010 to 2019")

g3         


#install.packages("patchwork")
#library(patchwork)
#g3 | (g2 / g1)


#----------------------------------------------------------------------------------
# 3 Race distribution

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



#----------------------------------------------------------------------------------
## 4 median income

# 4.1 median family income
# median house hold income
# B19013_001 Estimate!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars)

# B19127_001 Estimate!!Aggregate family income in the past 12 months (in 2019 inflation-adjusted dollars)
# AGGREGATE FAMILY INCOME IN THE PAST 12 MONTHS (IN 2019 INFLATION-ADJUST


# B10010_001 Estimate!!Median family income in the past 12 months--!!Total:
# B19113_001 Estimate!!Median family income in the past 12 months (in 2019 inflation-adjusted dollars)
# B19126_001 Estimate!!Median family income in the past 12 months (in 2019 inflation-adjusted dollars) --!!Total: ** this is the right one


years <- 2010:2019
names(years) <- years
years

cnty_value <- map_dfr(years, ~{
  get_acs(
    geography = "county",
    variables = "B19126_001",
    state = "OH",
    county = "Cuyahoga",
    year = .x,
    survey = "acs5"
  )
}, .id = "year")

cnty_value

g1 <- ggplot(cnty_value, aes(x = year, y = estimate, group = 1)) + 
  geom_line() + 
  geom_point()+
  geom_text(data = cnty_value, aes(x= year,label= estimate), color="black")+
  theme_minimal(base_size = 10) + 
  scale_y_continuous(labels = scales::dollar) + 
  labs(title = "Median family income in Cuyahoga Couty, Ohio from 2010 to 2019",
       x = "Year",
       y = "ACS 1 year estimate-median age",
       caption = "Source: ACH 1 Year Estimae from 2010 to 2019")
g1

cle_value <- map_dfr(years, ~{
  get_acs(
    geography = "county subdivision",
    variables = "B19126_001",
    state = "OH",
    county = "Cuyahoga",
    year = .x,
    survey = "acs5"
  )%>% 
    filter (GEOID == "3903516000")%>% 
    mutate(NAME = str_remove(NAME, ", Cuyahoga County, Ohio")) 
}, .id = "year")

cle_value

g2 <- ggplot(cle_value, aes(x = year, y = estimate, group = 1)) + 
  geom_line() + 
  geom_point()+
  geom_text(data = cle_value, aes(x= year,label= estimate), color="black")+
  theme_minimal(base_size = 10) + 
  scale_y_continuous(labels = scales::dollar) + 
  labs(title = "Median Family Income in Cleveland, Cuyahoga Couty, Ohio from 2010 to 2019",
       x = "Year",
       y = "ACS 1 year estimate- median age",
       caption = "Source: ACH 1 Year Estimae from 2010 to 2019")

g2

#--- combine two data in one plot

g3 <- ggplot(NULL, aes(x= year, y = estimate, group = 1)) +    
  geom_line(data = cnty_value, col = "red") + 
  geom_point(data = cnty_value, col = "black")+
  #geom_text(data = cnty_value, aes(x= year,label= estimate), color="black")+
  geom_line(data = cle_value, col = "blue")+ 
  geom_point(data = cle_value, col = "black") +
  #geom_text(data = cle_value, aes(x= year,label= estimate), color="black")+
  theme_minimal(base_size = 10) + 
  scale_y_continuous(labels = scales::dollar) + 
  labs(title = "Median Family Income in Cleveland and Cuyahoga County, Ohio from 2010 to 2019",
       x = "Year",
       y = "ACS 1 year estimate-median age",
       caption = "Source: ACH 1 Year Estimae from 2010 to 2019")

g3         


#install.packages("patchwork")
#library(patchwork)
g3 | (g2 / g1)

#----------------------------
# 4-2 median household income 
# B19049_001 Estimate!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars) --!!Total:**
# B22008_001 Estimate!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars)--!!Total:
# B25119_002 Estimate!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars) --!!Total:!!Owner occupied (dollars)
# B25119_003 Estimate!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars) --!!Total:!!Renter occupied (dollars)
# B19013_001 Estimate!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars) ?? **
years <- 2010:2019
names(years) <- years
years

cnty_value <- map_dfr(years, ~{
  get_acs(
    geography = "county",
    variables = "B19013_001",
    state = "OH",
    county = "Cuyahoga",
    year = .x,
    survey = "acs5"
  )
}, .id = "year")

cnty_value

g1 <- ggplot(cnty_value, aes(x = year, y = estimate, group = 1)) + 
  geom_line() + 
  geom_point()+
  geom_text(data = cnty_value, aes(x= year,label= estimate), color="black")+
  theme_minimal(base_size = 10) + 
  scale_y_continuous(labels = scales::dollar) + 
  labs(title = "Median Household income in Cuyahoga Couty, Ohio from 2010 to 2019",
       x = "Year",
       y = "ACS 5 year estimate",
       caption = "Source: ACH 1 Year Estimae from 2010 to 2019")
g1

cle_value <- map_dfr(years, ~{
  get_acs(
    geography = "county subdivision",
    variables = "B19049_001",
    state = "OH",
    county = "Cuyahoga",
    year = .x,
    survey = "acs5"
  )%>% 
    filter (GEOID == "3903516000")%>% 
    mutate(NAME = str_remove(NAME, ", Cuyahoga County, Ohio")) 
}, .id = "year")

cle_value

g2 <- ggplot(cle_value, aes(x = year, y = estimate, group = 1)) + 
  geom_line() + 
  geom_point()+
  geom_text(data = cle_value, aes(x= year,label= estimate), color="black")+
  theme_minimal(base_size = 10) + 
  scale_y_continuous(labels = scales::dollar) + 
  labs(title = "Median Household Income in Cleveland, Cuyahoga Couty, Ohio from 2010 to 2019",
       x = "Year",
       y = "ACS 5 year estimate",
       caption = "Source: ACH 5 Year Estimae from 2010 to 2019")

g2

#--- combine two data in one plot

g3 <- ggplot(NULL, aes(x= year, y = estimate, group = 1)) +    
  geom_line(data = cnty_value, col = "red") + 
  geom_point(data = cnty_value, col = "black")+
  #geom_text(data = cnty_value, aes(x= year,label= estimate), color="black")+
  geom_line(data = cle_value, col = "blue")+ 
  geom_point(data = cle_value, col = "black") +
  #geom_text(data = cle_value, aes(x= year,label= estimate), color="black")+
  theme_minimal(base_size = 10) + 
  scale_y_continuous(labels = scales::dollar) + 
  labs(title = "Median Household Income in Cleveland and Cuyahoga County, Ohio from 2010 to 2019",
       x = "Year",
       y = "ACS 5 year estimate",
       caption = "Source: ACH 5 Year Estimae from 2010 to 2019")

g3         


#install.packages("patchwork")
#library(patchwork)
g3 | (g2 / g1)



#---- 4.3 Median nonfamily household income
# B19215_001 Estimate!!Median nonfamily household income in the past 12 months (in 2019 inflation-adjusted dollars) --!!Total (dollars):

years <- 2010:2019
names(years) <- years
years

cnty_value <- map_dfr(years, ~{
  get_acs(
    geography = "county",
    variables = "B19215_001",
    state = "OH",
    county = "Cuyahoga",
    year = .x,
    survey = "acs5"
  )
}, .id = "year")

cnty_value

g1 <- ggplot(cnty_value, aes(x = year, y = estimate, group = 1)) + 
  geom_line() + 
  geom_point()+
  geom_text(data = cnty_value, aes(x= year,label= estimate), color="black")+
  theme_minimal(base_size = 10) + 
  scale_y_continuous(labels = scales::dollar) + 
  labs(title = "Median Nonfamily Household income in Cuyahoga Couty, Ohio from 2010 to 2019",
       x = "Year",
       y = "ACS 5 year estimate",
       caption = "Source: ACH 1 Year Estimae from 2010 to 2019")
g1

cle_value <- map_dfr(years, ~{
  get_acs(
    geography = "county subdivision",
    variables = "B19215_001",
    state = "OH",
    county = "Cuyahoga",
    year = .x,
    survey = "acs5"
  )%>% 
    filter (GEOID == "3903516000")%>% 
    mutate(NAME = str_remove(NAME, ", Cuyahoga County, Ohio")) 
}, .id = "year")

cle_value

g2 <- ggplot(cle_value, aes(x = year, y = estimate, group = 1)) + 
  geom_line() + 
  geom_point()+
  geom_text(data = cle_value, aes(x= year,label= estimate), color="black")+
  theme_minimal(base_size = 10) + 
  scale_y_continuous(labels = scales::dollar) + 
  labs(title = "Median Nonfamily Household Income in Cleveland, Cuyahoga Couty, Ohio from 2010 to 2019",
       x = "Year",
       y = "ACS 5 year estimate",
       caption = "Source: ACH 5 Year Estimae from 2010 to 2019")

g2

#--- combine two data in one plot

g3 <- ggplot(NULL, aes(x= year, y = estimate, group = 1)) +    
  geom_line(data = cnty_value, col = "red") + 
  geom_point(data = cnty_value, col = "black")+
  #geom_text(data = cnty_value, aes(x= year,label= estimate), color="black")+
  geom_line(data = cle_value, col = "blue")+ 
  geom_point(data = cle_value, col = "black") +
  #geom_text(data = cle_value, aes(x= year,label= estimate), color="black")+
  theme_minimal(base_size = 10) + 
  scale_y_continuous(labels = scales::dollar) + 
  labs(title = "Median Nonfamily Household Income in Cleveland and Cuyahoga County, Ohio from 2010 to 2019",
       x = "Year",
       y = "ACS 5 year estimate",
       caption = "Source: ACH 5 Year Estimae from 2010 to 2019")

g3         


#install.packages("patchwork")
#library(patchwork)
g3 | (g2 / g1)

#-------------------------------------------------------------------------------
# median house hold income map
## I need this 
## ----beeswarm---------------------------------------
#install.packages("ggbeeswarm")

library(ggbeeswarm)

race_income <- get_acs(
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

race_income_plot<- ggplot(race_income, aes(x = variable, y = summary_est, color = summary_est)) +
  geom_quasirandom(alpha = 0.5) +
  coord_flip() +
  theme_minimal() +
  scale_color_viridis_c(guide = FALSE) +
  scale_y_continuous(labels = scales::dollar) +
  labs(x = "Largest group in Census tract",
       y = "Median household income",
       title = "Household income distribution by largest racial/ethnic group",
       subtitle = "Census tracts, Cleveland City",
       caption = "Data source: 2015-2019 ACS")

race_income_plot

