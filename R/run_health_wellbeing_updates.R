# Project: State of London
# Title: Health and wellbeing chapter - insert data to database
# Author: Joe Heywood
# Chapter Author(s): Veronica
# Date: May 2023

library(purrr)
library(dplyr)
library(glue)
# library(tidyxl)
# library(unpivotr)
# library(readxl)
library(vroom)

dt_dr <- file.path("C:/Users/joheywood/Greater London Authority/",
                   "IU - State of London report/Version 4 (January 2024)/Data/",
                   "Health and wellbeing" ) 

dt_dr_prev <- "Q:/Teams/D&PA/Demography/state_of_london_health_chapter/Data_for_JH" 

infl_vac_fl <- file.path("C:/Users/joheywood/Greater London Authority/",
                   "IU - State of London report/Version 4 (January 2024)/Data/",
                   "Health and wellbeing/Influenza_vaccine_uptake_forV4.csv" ) 

lbw_fl <- file.path("C:/Users/joheywood/Greater London Authority/",
                   "IU - State of London report/Version 4 (January 2024)/Data/",
                   "Health and wellbeing/LBW_trends_for_London and England_forV4.csv" ) 

sen_eth_fl <- file.path("C:/Users/joheywood/Greater London Authority/",
                   "IU - State of London report/Version 4 (January 2024)/Data/",
                   "Health and wellbeing/SEN_ethnicity_London_England_forV4.csv" ) 

smoking_fl <- file.path("C:/Users/joheywood/Greater London Authority/",
                   "IU - State of London report/Version 4 (January 2024)/Data/",
                   "Health and wellbeing/Smoking_Housing_forV4.csv" ) 

# Function: Inserts data for YP/Education chapter
# Arguments:
# arg1: Filepath to excel sheet containing data
# Returns: logical TRUE if it works
run_health_wellbeing_updates <- function(
        lbw_fl, sen_eth_fl, smoking_fl, dbfl # , dt_dr_prev
        ) {
    if(!file.exists(dbfl)) {
        print("no db file")
    }
    
    ### Healthy Life Expectancy (0-1) ####
    tryCatch({
        fread(file.path(dt_dr_prev, "1_Life_expectancy_0-1years.csv")) %>%
            as.data.frame() %>%
            mutate(xvarchar = Period, xvardt = NA, xwhich = 1, yvllb = Area_name,
                   text = ifelse(Sex == "Female",
                                 "Chart_left_Female", "Chart_right_Male"),
                   dataset = "sol_life_exp02", yval = Healthy_Life_Expectancy) %>%
            insert_db(log = log, excfl = "SoL - Health/Wellbeing")
   
    }, error = function(e){error_log(e, "SoL - Health/Wellbeing")})

    ## Healthy Life Expectancy (60-64) ####
    tryCatch({
        fread(file.path(dt_dr_prev, "2_Life_expectancy_60-64years.csv")) %>%
            as.data.frame() %>%
            mutate(xvarchar = Period, xvardt = NA, xwhich = 1, yvllb = Area_name,
                   text = ifelse(Sex == "Female",
                                 "Chart_left_Female",
                                 "Chart_right_Male"),
                   dataset = "sol_life_exp64", yval = Healthy_Life_Expectancy) %>%
            insert_db(log = log, excfl = "SoL - Health/Wellbeing")
   
    }, error = function(e){error_log(e, "SoL - Health/Wellbeing")})

    ## Preventable Mortality by Deprviation ####
    tryCatch({
        fread(file.path(dt_dr_prev,
                        "3_London_007b_PreventableMortality_by_DepDec_2019-21_LondonProfile.csv")) %>%
            as.data.frame() %>%
            mutate(xvarchar = x, xvardt = NA, xwhich = 1, yvllb = breakdown,
                   text = "", dataset = "sol_75_mort",
                   yval = y) %>%
            insert_db(log = log, excfl = "SoL - Health/Wellbeing")
   
    }, error = function(e){error_log(e, "SoL - Health/Wellbeing")})

    ## Infant mortality #####

    tryCatch({
        fread(file.path(dt_dr_prev,  "4_PHOF_IMR_London.csv")) %>%
            as.data.frame() %>%
            mutate(dataset = "sol_infmor", xwhich = 1, xvarchar = Area_Code,
                   xvardt = NA, yvllb = "", text = "", yval = Value) %>%
            insert_db(log = log, excfl = "SoL - Health/Wellbeing")
   
    }, error = function(e){error_log(e, "SoL - Health/Wellbeing")})

    #### Causes Death by Sex ####
    tryCatch({
        fread(file.path(dt_dr_prev,  "5_HPL_CausesOfDeath_Sex.csv")) %>%
            as.data.frame() %>%
            mutate(xvarchar = lc_name, xvardt = NA, xwhich = 1, yvllb = Sex,
                   text = "", dataset = "sol_adult_mort", yval = deaths) %>%
            insert_db(log = log, excfl = "SoL - Health/Wellbeing")
   
    }, error = function(e){error_log(e, "SoL - Health/Wellbeing")})
   
    #### Common preventable diseases by Deprivation ####
    tryCatch({
        fread(file.path(dt_dr_prev,  "6_Morbidity_deprivation_London_June2021.csv")) %>%
            as.data.frame() %>%
            mutate(xvarchar = Disease, xvardt = NA, xwhich = 1, yvllb = Quintile,
                   text = "", dataset = "sol_prev_disease", yval = Percentage) %>%
            insert_db(log = log, excfl = "SoL - Health/Wellbeing")
   
    }, error = function(e){error_log(e, "SoL - Health/Wellbeing")})
   
    #### Adult Anxiety ####
    tryCatch({
        fread(file.path(dt_dr_prev,  "7_WB_Means_2012_to_2022_long.csv")) %>%
            as.data.frame() %>%
            filter(Index %in% c("SATISFACTION", "ANXIETY")) %>%
            mutate(xvardt = as.Date(Year, format = "%d/%m/%Y"),
                   xvarchar = "", xwhich = 2,
                   yvllb = str_to_title(Area_Name),
                   text = ifelse(Index == "SATISFACTION",
                                 "Chart_right_Satisfaction", "Chart_left_Anxiety"),
                   dataset = "sol_adult_anx", yval = Value) %>%
            insert_db(log = log, excfl = "SoL - Health/Wellbeing")
   
    }, error = function(e){error_log(e, "SoL - Health/Wellbeing")})
     
    # #### SEMH #### 
    tryCatch({
        vroom(sen_eth_fl) %>%
            mutate(xvardt = NA, xvarchar = Eth_group, xwhich = 1,
                   yvllb = Region, text = "", dataset = "sol_semh", 
                   yval = Percent) %>%
            insert_db(log = log, excfl = "SoL - Health/Wellbeing")
    }, error = function(e){error_log(e, "SoL - Health/Wellbeing")})
    #### LBW #### 
    tryCatch({
        vroom(lbw_fl) |>
            mutate(dataset = "sol_lbw", xwhich = 1, xvarchar = Year, 
                   xvardt = NA, yvllb = Area, yval = Value, text = "") |>
            insert_db(log = log, excfl = "SoL - Health/Wellbeing")
    }, error = function(e){error_log(e, "SoL - Health/Wellbeing")}) 
    
  #### Influenza Vaccine #### map (ignore?)
    tryCatch({
        vroom(infl_vac_fl)
    }, error = function(e){error_log(e, "SoL - Health/Wellbeing")}) 
  
  
    ## Obesity #### 
    tryCatch({
        fread(file.path(dt_dr_prev,  "9_PHOF_OWandOB_London.csv")) %>%
            as.data.frame() %>%
            mutate(xvardt = NA, xvarchar = Area_Code, xwhich = 1, 
                   yvllb = "", text = "", dataset = "sol_obesity", yval = Value)  %>%
            insert_db(log = log, excfl = "SoL - Health/Wellbeing")
    }, error = function(e){error_log(e, "SoL - Health/Wellbeing")}) 
  
    ## Physical Activity #### 
    tryCatch({
        fread(file.path(dt_dr_prev,  "10_PHOF_PA_London.csv")) %>%
            as.data.frame() %>%
            mutate(xvardt = NA, xvarchar = Area_Code, xwhich = 1, 
                   yvllb = "", text = "", dataset = "sol_physact", yval = Value)  %>%
            insert_db(log = log, excfl = "SoL - Health/Wellbeing")
    }, error = function(e){error_log(e, "SoL - Health/Wellbeing")}) 
    
  
    ## Smoking ####
    tryCatch({
        vroom(smoking_fl) %>%
            mutate(xvardt = NA, xvarchar = Category, xwhich = 1,
                   yvllb = Area_Name, text = "", dataset = "sol_smoke", yval = Value) %>%
            insert_db(log = log, excfl = "SoL - Health/Wellbeing")

    }, error = function(e){error_log(e, "SoL - Health/Wellbeing")})
    
    #### Cancer by Deprivation ### 
    tryCatch({
        fread(file.path(dt_dr_prev,  "13_Cancer_ReferralRates_DepQuintile.csv")) %>%
            as.data.frame() %>%
            mutate(xvardt = NA, xvarchar = Financial_year, xwhich = 1, 
                   yvllb = Quintile, text = "", dataset = "sol_cancdep", 
                   yval = Rate) %>%
            insert_db(log = log, excfl = "SoL - Health/Wellbeing")
        
    }, error = function(e){error_log(e, "SoL - Health/Wellbeing")}) 
  
    ## Covid Jabs ####
    tryCatch({
        fread(file.path(dt_dr_prev,  "12_CHIME_Vaccination_London_Eth_CombCats.csv")) %>%
            as.data.frame() %>%
            mutate(xvardt = as.Date(TimePeriod, format = "%d/%m/%Y"), 
                   xvarchar = "", xwhich = 2, 
                   yvllb = Category, text = "", dataset = "sol_covjabs", 
                   yval = Value) %>%
            insert_db(log = log, excfl = "SoL - Health/Wellbeing")
        
    }, error = function(e){error_log(e, "SoL - Health/Wellbeing")}) 
  
    ## ASTHMA #### 
    tryCatch({
        fread(file.path(dt_dr_prev,  "14_PHE_ASTHMA_London.csv")) %>%
            as.data.frame() %>%
            mutate(xvardt = NA, xvarchar = Area_Code, xwhich = 1, 
                   yvllb = "", text = "", dataset = "sol_asthma", yval = Value)  %>%
            insert_db(log = log, excfl = "SoL - Health/Wellbeing")
    }, error = function(e){error_log(e, "SoL - Health/Wellbeing")}) 
     
}