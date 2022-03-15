# For a data frame with polygon spatial attributes, make a new data frame containing x rows per polygon, 1 for each neighboring polygon, with variables you want to summarise- e.g. for each block group in Cuyahoga County, calculate the median sales price of all neighboring block groups

# df: a data frame (or variant, e.g. tibble, etc) with class "sf". The geometry of the spatial object is a polygon
# geom_col: the name of the column containing the points for plotting the spatial elements; in 99%+ cases, this should be "geometry", but I came across an example where it was "geom"

adj_poly <- function(df, idvar,group_var = NULL, keepers ){
  
  sf_col <- purrr::map(df, class) %>% enframe() %>% filter(str_detect(value, "sf"))
  sf_col <- sf_col$name
  
  df_no_sf <- df %>%
    as.data.frame() %>%
    select(-c(sf_col)) %>%
    rownames_to_column() %>%
    mutate(row.id = as.integer(rowname)) %>%
    select(row.id, idvar) 
  
  # get the geometry column
  
  
  
  # attach the "real" id- e.g., block group, to the product of st_touches 
  
  
  neighbors <- st_touches(df, df) %>%
    as.data.frame()
  
  neighbors <- neighbors %>%
    left_join(df_no_sf)
  
  vars <- df %>%
    as.data.frame() %>%
    select(-(all_of(sf_col))) %>%
    rownames_to_column(var = "col.id") %>%
    select(col.id, all_of(keepers), group_var) %>%
    mutate(col.id = as.integer(col.id))
  
  j1 <- neighbors %>%
    left_join(vars)
  
  return(j1)
  
}



sk <- function(df, var){
  paste0("var = ", var, "; skew = ", round(skewness(df[[var]], na.rm = T), 2), "; kurt = ", round(kurtosis(df[[var]], na.rm = T),2))
}
