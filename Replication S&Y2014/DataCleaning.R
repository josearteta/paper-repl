#------------------------------------------------------------------------------------------------------------------------
#                                                     LIBRARIES
#------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(plm)
library(systemfit)
library(estimatr)
library(modelsummary)
library(fixest)
library(stargazer)
library(haven)
library(lmtest)
library(AER)
library(sandwich)
library(car)
library(tibble)
library(recipes)
library(multiwayvcov)
library(fastDummies)

#------------------------------------------------------------------------------------------------------------------------
#                                                       DATA
#------------------------------------------------------------------------------------------------------------------------
cens1960ext <- readRDS("cens1960ext_aer.rds") # Extract of IPUMS 1960 Census 1% sample microdata.
cens1970ext <- readRDS("cens1970ext_aer.rds") # Extract of both IPUMS 1970 Census 1% state samples microdata.
cens1980ext <- readRDS("cens1980ext_aer.rds") # Extract of IPUMS 1980 Census 5% sample microdata.

schooling_laws_aa <- readRDS("schooling_laws_aa.rds") # Acemoglu and Angrist (2000) schooling law variables.
schooling_laws_sy <- readRDS("schooling_laws_sy.rds") # Schooling law created by the researchers.

ck_school_quality_revised <- readRDS("ck_school_quality_revised.rds") # Card and Krueguer school quality measures aggregated as by birth cohort and extended to earlier and later cohorts.

#------------------------------------------------------------------------------------------------------------------------
#                                                       First Graph
#------------------------------------------------------------------------------------------------------------------------
# Creating the CCA measure that corrects the CA
schooling_laws_aa <- schooling_laws_aa %>% # Foot note 12
  mutate(cca  = ifelse(req_sch > 0,pmin(drop_age - enrolage,req_sch,na.rm = T),drop_age - enrolage))
schooling_laws_aa$cca <- ifelse(is.na(schooling_laws_aa$cca),0,schooling_laws_aa$cca) # Adding 0s instead of NAs

# Cleaning the 1960 census data
cens1960clean <- cens1960ext %>%
  mutate(yob = ifelse(birthqtr == 1, year - age,year - age - 1), # Relevant year of birth
         weeks = case_when(wkswork2 == 0 ~ 0, wkswork2 == 1 ~ 8.5 , wkswork2 == 2 ~ 21.9, wkswork2 == 3 ~ 33.2,
                           wkswork2 == 4 ~ 42.6, wkswork2 == 5 ~ 48.3, wkswork2 == 6 ~ 51.8),
         incwage = case_when(incwage > 13500 & incwage != NA ~ 20250, TRUE ~ incwage),
         gradcap = case_when(higrade >=0 & higrade <= 3 ~ 0,higrade >= 4 & higrade <= 20 ~ higrade - 3,
                             higrade >= 21 & higrade <= 23 ~ 17, TRUE ~ higrade)) %>%
  filter(yob >= 1905 & yob <= 1934, # Desired age range
         bpl != 15, bpl != 2,bpl <= 56 & bpl >= 1,
         statefip != 15, statefip != 2,statefip <= 56 & statefip >= 1,
         incwage > 0) %>%
  mutate(lnwkwage = log(incwage/weeks),
         lnoccscore = log(occscore),
         male = ifelse(sex == 1,1,0),
         white = ifelse(race == 1,1,0),
         yearat14 = year - age + 14,
         black = ifelse(race == 2,1,0),
         othrace = ifelse(race > 2,1,0),
         bplg = bpl) %>%
  select(serial,pernum,year,bplg,statefip,age,birthqtr,yob,gradcap,slwt,lnoccscore,empstat,gqtype,marst,yearat14,
         lnwkwage,male,black,othrace)

# Cleaning the 1970 census data
cens1970clean <- cens1970ext %>%
  mutate(yob = ifelse(birthqtr == 1, year - age,year - age - 1), # Relevant year of birth
         weeks = case_when(wkswork2 == 0 ~ 0, wkswork2 == 1 ~ 8.4 , wkswork2 == 2 ~ 21.4, wkswork2 == 3 ~ 33.1,
                           wkswork2 == 4 ~ 42.6, wkswork2 == 5 ~ 48.3, wkswork2 == 6 ~ 51.8),
         incwage = case_when(incwage > 24850 & incwage != NA ~ 37275, TRUE ~ incwage),
         gradcap = case_when(higrade >=0 & higrade <= 3 ~ 0,higrade >= 4 & higrade <= 20 ~ higrade - 3,
                             higrade >= 21 & higrade <= 23 ~ 17, TRUE ~ higrade),
         slwt = case_when(slwt != 0 ~ slwt/2, TRUE ~ slwt)) %>%
  filter(yob >= 1915 & yob <= 1944, # Desired age range
         bpl != 15, bpl != 2,bpl <= 56,
         statefip != 15, statefip != 2,statefip <= 56,
         incwage > 0) %>%
  mutate(lnwkwage = log(incwage/weeks),
         lnoccscore = log(occscore),
         male = ifelse(sex == 1,1,0),
         white = ifelse(race == 1,1,0),
         yearat14 = year - age + 14,
         black = ifelse(race == 2,1,0),
         othrace = ifelse(race > 2,1,0),
         bplg = bpl) %>%
  select(serial,pernum,year,bplg,statefip,age,birthqtr,yob,gradcap,slwt,lnoccscore,empstat,gqtype,marst,yearat14,
         lnwkwage,male,black,othrace)


# Cleaning the 1980 census data
cens1980clean <- cens1980ext %>%
  mutate(yob = ifelse(birthqtr == 1, year - age,year - age - 1), # Relevant year of birth
         weeks = wkswork1,
         incwage = case_when(incwage > 50005 & incwage != NA ~ 75007.5, TRUE ~ incwage),
         gradcap = case_when(higrade >=0 & higrade <= 3 ~ 0,higrade >= 4 & higrade <= 20 ~ higrade - 3,
                             higrade >= 21 & higrade <= 23 ~ 17, TRUE ~ higrade)) %>%
  filter(yob >= 1925 & yob <= 1954, # Desired age range
         bpl != 15, bpl != 2,bpl <= 56,
         statefip != 15, statefip != 2,statefip <= 56,
         incwage > 0, weeks > 0) %>% 
  mutate(lnwkwage = log(incwage/weeks),
         lnoccscore = log(occscore),
         male = ifelse(sex == 1,1,0),
         white = ifelse(race == 1,1,0),
         yearat14 = year - age + 14,
         black = ifelse(race == 2,1,0),
         othrace = ifelse(race > 2,1,0),
         bplg = bpl) %>%
  select(serial,pernum,year,bplg,statefip,age,birthqtr,yob,gradcap,slwt,lnoccscore,empstat,gqtype,marst,yearat14,
         lnwkwage,male,black,othrace)

# Removing raw censuses to clean memory
rm("cens1960ext","cens1970ext","cens1980ext")
gc()

# Stacking all the censuses
census <- rbind(cens1960clean,cens1970clean,cens1980clean)

# Adding the RS measure to the census data
census <- left_join(census,schooling_laws_sy,by = c("bplg","yob"))

# Adding the CA,CL and CCA measure to the census data
census <- left_join(census,schooling_laws_aa[,c(1,2,8:10)],by = c("bplg","yearat14"))


# Adding the school quality measure to the data
census <- left_join(census,ck_school_quality_revised,by = c("bplg","yob"))


# Removing the clean versions and other data to save memory
rm("cens1960clean","cens1970clean","cens1980clean","ck_school_quality_revised","schooling_laws_aa","schooling_laws_sy")
gc()

# Creating the schooling law dummies and cluster variables
census <- census %>%
  mutate(cl7 = ifelse(cl == 7,1,0),
         cl8 = ifelse(cl == 8,1,0),
         cl9 = ifelse(cl >= 9,1,0),
         
         ca9 = ifelse(ca == 9,1,0),
         ca10 = ifelse(ca == 10,1,0),
         ca11 = ifelse(ca >= 11,1,0),
         
         corr_ca8 = ifelse(cca == 8,1,0),
         corr_ca9 = ifelse(cca == 9,1,0),
         corr_ca10 = ifelse(cca >= 10,1,0),
         
         ny7 = ifelse(numlawyears == 7,1,0),
         ny8 = ifelse(numlawyears == 8,1,0),
         ny9 = ifelse(numlawyears >= 9,1,0),
         
         yob_bplg = (yob - 1900)*100 + bplg,
         
         census70 = ifelse(year == 1970,1,0),
         census80 = ifelse(year == 1980,1,0),
         age2 = age^2,
         age3 = age^3,
         age4 = age^4)

# Creating the dummies for the yob and for the states of birth
census <- dummy_cols(census,select_columns = "bplg")
census <- dummy_cols(census,select_columns = "yob")
census <- census %>%
  select(-c(bplg_56,yob_1954))


# Region dummy variables added to the cesus data
census <- census %>%
  mutate(southern_born = ifelse(bplg_10 == 1 | bplg_11 == 1 | bplg_12 == 1 | bplg_13 == 1 |bplg_24 == 1 |
                                  bplg_37 == 1 | bplg_45 == 1 |bplg_51 == 1 | bplg_54 == 1| bplg_1 == 1 | bplg_21 ==1 |
                                  bplg_28 == 1 | bplg_47 == 1 | bplg_5 == 1 | bplg_22 == 1 | bplg_40 == 1 |bplg_48 ==1,1,0),
         midwestern_born = ifelse(bplg_18 == 1 | bplg_17 == 1 | bplg_26 == 1|bplg_39 == 1|bplg_55 == 1|bplg_19 ==1|
                                    bplg_20 == 1|bplg_27==1|bplg_29==1|bplg_31==1|bplg_38==1|bplg_46==1,1,0),
         northeastern_born = ifelse(bplg_9==1|bplg_23==1|bplg_25==1|bplg_33==1|bplg_44==1|bplg_50==1|bplg_34==1|
                                      bplg_36==1|bplg_42==1,1,0),
         western_born = ifelse(southern_born == 0 & midwestern_born == 0 & northeastern_born == 0,1,0))
census <- census %>%
  mutate(region = case_when(northeastern_born == 1 ~ 1,
                            southern_born == 1 ~ 2,
                            midwestern_born == 1 ~ 3,
                            western_born == 1 ~ 4))

# Creating the interaction dummy variables
census <- census %>%
  mutate(
    south_05 = yob_1905*southern_born,
    south_06 = yob_1906*southern_born,
    south_07 = yob_1907*southern_born,
    south_08 = yob_1908*southern_born,
    south_09 = yob_1909*southern_born,
    south_10 = yob_1910*southern_born,
    south_11 = yob_1911*southern_born,
    south_12 = yob_1912*southern_born,
    south_13 = yob_1913*southern_born,
    south_14 = yob_1914*southern_born,
    south_15 = yob_1915*southern_born,
    south_16 = yob_1916*southern_born,
    south_17 = yob_1917*southern_born,
    south_18 = yob_1918*southern_born,
    south_19 = yob_1919*southern_born,
    south_20 = yob_1920*southern_born,
    south_21 = yob_1921*southern_born,
    south_22 = yob_1922*southern_born,
    south_23 = yob_1923*southern_born,
    south_24 = yob_1924*southern_born,
    south_25 = yob_1925*southern_born,
    south_26 = yob_1926*southern_born,
    south_27 = yob_1927*southern_born,
    south_28 = yob_1928*southern_born,
    south_29 = yob_1929*southern_born,
    south_30 = yob_1930*southern_born,
    south_31 = yob_1931*southern_born,
    south_32 = yob_1932*southern_born,
    south_33 = yob_1933*southern_born,
    south_34 = yob_1934*southern_born,
    south_35 = yob_1935*southern_born,
    south_36 = yob_1936*southern_born,
    south_37 = yob_1937*southern_born,
    south_38 = yob_1938*southern_born,
    south_39 = yob_1939*southern_born,
    south_40 = yob_1940*southern_born,
    south_41 = yob_1941*southern_born,
    south_42 = yob_1942*southern_born,
    south_43 = yob_1943*southern_born,
    south_44 = yob_1944*southern_born,
    south_45 = yob_1945*southern_born,
    south_46 = yob_1946*southern_born,
    south_47 = yob_1947*southern_born,
    south_48 = yob_1948*southern_born,
    south_49 = yob_1949*southern_born,
    south_50 = yob_1950*southern_born,
    south_51 = yob_1951*southern_born,
    south_52 = yob_1952*southern_born,
    south_53 = yob_1953*southern_born,
    
    west_05 = yob_1905*western_born,
    west_06 = yob_1906*western_born,
    west_07 = yob_1907*western_born,
    west_08 = yob_1908*western_born,
    west_09 = yob_1909*western_born,
    west_10 = yob_1910*western_born,
    west_11 = yob_1911*western_born,
    west_12 = yob_1912*western_born,
    west_13 = yob_1913*western_born,
    west_14 = yob_1914*western_born,
    west_15 = yob_1915*western_born,
    west_16 = yob_1916*western_born,
    west_17 = yob_1917*western_born,
    west_18 = yob_1918*western_born,
    west_19 = yob_1919*western_born,
    west_20 = yob_1920*western_born,
    west_21 = yob_1921*western_born,
    west_22 = yob_1922*western_born,
    west_23 = yob_1923*western_born,
    west_24 = yob_1924*western_born,
    west_25 = yob_1925*western_born,
    west_26 = yob_1926*western_born,
    west_27 = yob_1927*western_born,
    west_28 = yob_1928*western_born,
    west_29 = yob_1929*western_born,
    west_30 = yob_1930*western_born,
    west_31 = yob_1931*western_born,
    west_32 = yob_1932*western_born,
    west_33 = yob_1933*western_born,
    west_34 = yob_1934*western_born,
    west_35 = yob_1935*western_born,
    west_36 = yob_1936*western_born,
    west_37 = yob_1937*western_born,
    west_38 = yob_1938*western_born,
    west_39 = yob_1939*western_born,
    west_40 = yob_1940*western_born,
    west_41 = yob_1941*western_born,
    west_42 = yob_1942*western_born,
    west_43 = yob_1943*western_born,
    west_44 = yob_1944*western_born,
    west_45 = yob_1945*western_born,
    west_46 = yob_1946*western_born,
    west_47 = yob_1947*western_born,
    west_48 = yob_1948*western_born,
    west_49 = yob_1949*western_born,
    west_50 = yob_1950*western_born,
    west_51 = yob_1951*western_born,
    west_52 = yob_1952*western_born,
    west_53 = yob_1953*western_born,
    
    midwest_05 = yob_1905*midwestern_born,
    midwest_06 = yob_1906*midwestern_born,
    midwest_07 = yob_1907*midwestern_born,
    midwest_08 = yob_1908*midwestern_born,
    midwest_09 = yob_1909*midwestern_born,
    midwest_10 = yob_1910*midwestern_born,
    midwest_11 = yob_1911*midwestern_born,
    midwest_12 = yob_1912*midwestern_born,
    midwest_13 = yob_1913*midwestern_born,
    midwest_14 = yob_1914*midwestern_born,
    midwest_15 = yob_1915*midwestern_born,
    midwest_16 = yob_1916*midwestern_born,
    midwest_17 = yob_1917*midwestern_born,
    midwest_18 = yob_1918*midwestern_born,
    midwest_19 = yob_1919*midwestern_born,
    midwest_20 = yob_1920*midwestern_born,
    midwest_21 = yob_1921*midwestern_born,
    midwest_22 = yob_1922*midwestern_born,
    midwest_23 = yob_1923*midwestern_born,
    midwest_24 = yob_1924*midwestern_born,
    midwest_25 = yob_1925*midwestern_born,
    midwest_26 = yob_1926*midwestern_born,
    midwest_27 = yob_1927*midwestern_born,
    midwest_28 = yob_1928*midwestern_born,
    midwest_29 = yob_1929*midwestern_born,
    midwest_30 = yob_1930*midwestern_born,
    midwest_31 = yob_1931*midwestern_born,
    midwest_32 = yob_1932*midwestern_born,
    midwest_33 = yob_1933*midwestern_born,
    midwest_34 = yob_1934*midwestern_born,
    midwest_35 = yob_1935*midwestern_born,
    midwest_36 = yob_1936*midwestern_born,
    midwest_37 = yob_1937*midwestern_born,
    midwest_38 = yob_1938*midwestern_born,
    midwest_39 = yob_1939*midwestern_born,
    midwest_40 = yob_1940*midwestern_born,
    midwest_41 = yob_1941*midwestern_born,
    midwest_42 = yob_1942*midwestern_born,
    midwest_43 = yob_1943*midwestern_born,
    midwest_44 = yob_1944*midwestern_born,
    midwest_45 = yob_1945*midwestern_born,
    midwest_46 = yob_1946*midwestern_born,
    midwest_47 = yob_1947*midwestern_born,
    midwest_48 = yob_1948*midwestern_born,
    midwest_49 = yob_1949*midwestern_born,
    midwest_50 = yob_1950*midwestern_born,
    midwest_51 = yob_1951*midwestern_born,
    midwest_52 = yob_1952*midwestern_born,
    midwest_53 = yob_1953*midwestern_born)

#-------------------------------------------------------------------------------------------------------------------
#                                                       START FROM HERE
#-------------------------------------------------------------------------------------------------------------------



#-----------------------------------------------
# Splitting the census in the desired groups
#-----------------------------------------------

census <- readRDS("census.rds")

# Census for whites
census_whites <- census %>%
  filter(black == 0, othrace == 0)

# Census for white males 
census_whites_male <- census %>%
  filter(black == 0, othrace == 0, male == 1)

# Census for white males between 40 and 49 years
census_40to49 <- census %>%
  filter(black == 0, othrace == 0, male == 1,
         (year == 1960 & yob >= 1910 & yob <= 1919) | (year == 1970 & yob >= 1920 & yob <= 1929) |
           (year == 1980 & yob >= 1930 & yob <= 1939))
# Census for whites divided by regions

# Non south
census_whites_non_south <- census %>%
  filter(black == 0, othrace == 0,
         southern_born == 0)

# South
census_whites_south <- census %>%
  filter(black == 0, othrace == 0,
         southern_born == 1)
