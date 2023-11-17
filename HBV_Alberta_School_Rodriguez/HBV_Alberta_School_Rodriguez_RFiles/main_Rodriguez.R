
#----------------------------------------------------------------------------------------------------------
# Created by Maria Rodriguez, Data Scientist, 2023_Nov_16
# Objective:  Create a reproducible data product 
#     to describe grade 6 hepatitis B virus (HBV) school immunization coverage in Alberta over time.
# using data downloaded from http://www.ahw.gov.ab.ca/IHDA_Retrieval, under Immunization and School Coverage
#     filtered: http://www.ahw.gov.ab.ca/IHDA_Retrieval/selectSubCategoryParameters.do#
#-----------------------------------------------------------------------------------------------------------

# install.packages(c("tidyverse", 'ggplot2','renv'))

# library(renv) # to create virtual env
# renv::init()  # uncomment to create env, lock file
library(tidyverse)  
library(dplyr)
library(ggplot2)

df <- read_csv('Alberta_Sch_HBV_Gr6.csv') # in root folder

summary(df)
sum(is.na(df)) # 0, no nulls
unique(df$Geography) # "Z1" "Z2" "Z3" "Z4" "Z5"

# create a function that would return transformed data
get_df <- function(df){
  df_ <- df %>%
    
    # trim down to more relevant columns
    select(c('Geography', 'Year', 'Sex', 'Immunization Status', 'Immunization Percent', 'Standard Score') )
  
  # rename columns so easier to refer to
  colnames(df_) <- c('Geography','Year','Sex','Immunization_Status','Immunization_Percent', 'Standard_Score')
  
  # rename Geography so easier to interpret
  # base on https://open.alberta.ca/publications/official-standard-geographic-areas
  df_ <- df_ %>%
    mutate(Geography = case_when(
      Geography == 'Z1'~ 'South',
      Geography == 'Z2'~ 'Calgary',
      Geography == 'Z3'~ 'Central',
      Geography == 'Z4'~ 'Edmonton',
      Geography == 'Z5'~ 'North'
    )) %>%
    mutate(
      Latitude = case_when(
        Geography == 'South' ~ 50,
        Geography =='Calgary' ~ 51.0463,
        Geography == 'Central' ~ 52,
        Geography == 'Edmonton' ~ 53.631611,
        Geography == 'North' ~ 55
    ),
      Longitude = case_when(
        Geography == 'South' ~ -112,
        Geography =='Calgary' ~ -114.0534,
        Geography == 'Central' ~ -113,
        Geography == 'Edmonton' ~ -113.323975,
        Geography == 'North' ~ -115
    ))
  return(df_)
}

# create df to be used for IMMUNIZATION PERCENTAGE viz
perc_df <- get_df(df) %>%
  select(-c(Standard_Score, Latitude, Longitude))

# factor category so can control in the plotting
perc_df$Immunization_Status <- factor(perc_df$Immunization_Status,
                                  levels = c('Complete','Partial','Not Immunized'))

# create df to be used for STANDARD SCORE viz
score_df <- get_df(df) %>%
  # use only those with Complete immunization status, and 'Both' sex category
  filter(Immunization_Status == 'Complete',
         Sex == 'Both') %>%
  
  select( -c(Immunization_Status, Sex, Immunization_Percent)) %>%
  
  # base the category levels on the Standard Score in the Data Notes
  mutate(Score_Category = case_when (
    Standard_Score > 2 ~ 'Significantly Higher',
    Standard_Score >= 1 & Standard_Score <= 2 ~ 'High',
    Standard_Score >= -1 & Standard_Score < 1 ~ 'Average',
    Standard_Score >= -2 & Standard_Score < -1 ~ 'Low',
    Standard_Score < -2 ~ 'Significantly Lower'
  ))

# factor so can control categorical values during the plotting
score_df$Standard_Score <- factor(score_df$Standard_Score,
                              levels = c('Significantly Higher','High','Average','Low', 'Significantly Lower'))



  

