## Organize data: 
# This analysis examines the neighborhood characteristics and housing market conditions 
# in Cuyahoga County's 1161 block groups between 2017 and 2019. 
# We began by gathering parcel- and address-level data from several sources. 
# Property records obtained from the Cuyahoga County Fiscal Officer are primary data source; 
# we also incorporated information from HUD, the Cuyahoga Land Bank, and other sources. 
# At first blush, it might seem to make the most sense to examine housing markets for a single year. 
# We investigated this possibility, and found that many block groups, even those composed primarily of residential 
# properties, there was simply too little sales activity to obtain stable and meaningful estimates of current market conditions.
# In any given year there were many block groups with between zero and five arms length property transfers. 
# By combining information from three consecutive years, we are able to get a more full and accurate sense of market conditions in Cuyahoga County. 

dat = read.csv("input_dat_final_allyrs.csv")
cities <- readRDS("cities.rds")
mapx <- readRDS("bg_map.rds")

dat <- dat %>% 
  select(-c(TC1:cert)) %>% 
  mutate(blockgr10 = as.character(blockgr10)) %>% 
  relocate(yrs, .after = blockgr10)

str(dat$blockgr10)


# --------- what is gt1_alsales?
str(dat$gt1_alsales)

#---- PCA analysis

dat2 <- dat %>%
  filter(gt1_alsales >= 5) %>% 
  mutate(
    ct_rs2 = ct_singlefam + ct_multifam + ct_condo,
    res_density = ct_rs2 / bg_sqmi,
    pct_foreclosed = round(ct_foreclose / ct_own_occ * 100, 2),
    pct_w_sales = round(gt1_alsales / ct_rs2 * 100, 2),
    pct_comm = round(ct_comm / ct_parcel * 100, 2), 
    comm_density = ct_comm / bg_sqmi,
    pct_pv = round(ct_postal_vac / ct_rs * 100, 2), 
    pct_vac_land = round(ct_res_vac_land / ct_rs *100, 2), 
    pct_subsidized = round(ct_hcv / ct_rental_units * 100, 2), 
    hcv_density = ct_hcv / bg_sqmi,
    demo_density = ct_demos / bg_sqmi, 
    hcv_per_par = ct_hcv / ct_parcel
  ) %>% 
  mutate(
    pct_subsidized = if_else(is.na(pct_subsidized), 0, pct_subsidized)
  )

# diff: pct_w_sales, pct_foreclosed, pct_pv, 
vars <- dat2 %>% 
  # filter(yrs == 1719) %>% 
  select(
    blockgr10, 
    yrs,
    mn_alpricesft_adj,
    pct_w_sales,
    # mn_sales_per_par,
    var_alpricesft, 
    pct_foreclosed, 
    pct_pv, 
    res_density, 
    pct_own_occ, 
    # pct_subsidized, 
    # med_nei_al_persqft_adj, 
    med_yrbuilt, 
    med_sqft_per_unit, 
    mn_bath_perunit, 
    mn_beds_perunit, 
    pct_sf, 
    pct_mf, 
    pct_good,
    ct_demos, 
    comm_density,
    hcv_density
    
  )

vars
str(vars)

# produce all the basic descriptives for all input variables- min, mean, max, range, iqr, skew, kurtosis, etc
asdf <- describe(vars[-c(1:2)]) 

# %>% rownames_to_column() not working 

asdf

# for those
asdf_sqrt <- asdf %>% filter(min == 0)
asdf_log <- asdf %>% filter(min >0)

asdf_sqrt$rowname
asdf_log

vars2 <- vars %>% 
  mutate_if(names(.) %in% asdf_sqrt$rowname, ~sqrt(.)) %>% 
  mutate_if(names(.) %in% asdf_log$rowname, ~log(.))
vars2

asdf2 <- describe(vars2[-1])
asdf2

newnames <- names(asdf2)
asdf2

names(asdf2) <- c("vars",paste0(newnames[-1], "_new"))

asdf3 <- asdf %>% left_join(asdf2)

asdf3


asdf3 <- asdf3 %>%
  mutate(
    skew_better = if_else(abs(skew_new) < abs(skew), 1,0), 
    kurt_better = if_else(abs(kurtosis_new) < abs(kurtosis), 1,0), 
    both_better = if_else(skew_better == 1 & kurt_better == 1, 1,0)
    
    
  )
asdf_sqrt2 <- asdf3 %>% 
  filter(both_better==1 & min == 0)

asdf_log2 <- asdf3 %>% 
  filter(both_better==1 & min > 0)


vars3 <- vars %>% 
  mutate_at(vars(pct_foreclosed, pct_pv,  pct_good, ct_demos, comm_density, hcv_density), sqrt) %>% 
  mutate_at(vars(var_alpricesft,res_density, med_sqft_per_unit, mn_bath_perunit ), log) 

vars3

rm(asdf, asdf2, asdf3, asdf_log, asdf_log2, asdf_sqrt, asdf_sqrt2)

## Principal Component and Cluster Analysis

#The next step was to use these four neighborhood dimensions 
# to categorize block groups into different and meaningful 
# neighborhood 'types.'
#This analysis identified 9 neighborhood types.

asdf4 <- principal(vars3[vars3$yrs == 1719,3:19], 4, rotate = "oblimin")

scores <- asdf4$scores %>% as.data.frame()

c1 <- vars %>%  filter(yrs == 1719) %>% bind_cols(scores)

c2 <- Mclust(c1[,20:23])
summary(c2)

# how to add this MClust result to data table 











#--- PCA sampler
# NOT RUN {
#Four principal components of the Harman 24 variable problem
#compare to a four factor principal axes solution using factor.congruence
pc <- principal(Harman74.cor$cov,4,rotate="varimax")
mr <- fa(Harman74.cor$cov,4,rotate="varimax")  #minres factor analysis
pa <- fa(Harman74.cor$cov,4,rotate="varimax",fm="pa")  # principal axis factor analysis
round(factor.congruence(list(pc,mr,pa)),2)

pc2 <- principal(Harman.5,2,rotate="varimax")
pc2
round(cor(Harman.5,pc2$scores),2)  #compare these correlations to the loadings 
#now do it for unstandardized scores, and transform obliquely
pc2o <- principal(Harman.5,2,rotate="promax",covar=TRUE)
pc2o
round(cov(Harman.5,pc2o$scores),2) 
pc2o$Structure    #this matches the covariances with the scores
biplot(pc2,main="Biplot of the Harman.5 socio-economic variables",labels=paste0(1:12))

#For comparison with SPSS  (contributed by Gottfried Helms)
pc2v <- principal(iris[1:4],2,rotate="varimax",normalize=FALSE,eps=1e-14)
print(pc2v,digits=7)
pc2V <- principal(iris[1:4],2,rotate="Varimax",eps=1e-7)
p <- print(pc2V,digits=7)
round(p$Vaccounted,2)   # the amount of variance accounted for is returned as an object of print
# }