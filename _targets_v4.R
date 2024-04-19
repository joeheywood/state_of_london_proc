# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("r2d3svg", "rsvg", "glue", "purrr", "RPostgres", "readxl",
               "dplyr", "tidyr", "readr", "git2r", "tidyxl", "unpivotr"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multiprocess")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# dbfl <- setup_db(
#     "data/sol_v4.db",
#     clear = TRUE,
#     mtd = "data/meta.csv")

# source("other_functions.R") # Source other scripts as needed. # nolint
data_dir <- file.path("C:/Users/joheywood/Greater London Authority/",
                      "IU - State of London report/Version 4 (January 2024)/Data/")

# Replace the target list below with your own:
list(
    ### dbfl ###
    tar_target(db_fl, "data/sol_v4.db", format = "file"),
    ### YP/Education Chapter ### 
    # tar_target(yp_ed_fl, 
    #            file.path(
    #                data_dir ,  
    #                "Young people and education/Chapter 10 - Children and Young People.xlsx"), 
    #            format = "file"),
    # tar_target(run_yp_ed,command = run_yp_education_updates(yp_ed_fl, dbfl)),
    ### Environment chapter ### 
    tar_target(
        env_fl, 
        file.path(data_dir, "Environment/Environment data v4.xlsx"), 
        format = "file"),
    tar_target(
        aq_fl, 
        file.path(data_dir, "Environment/airquality_data.csv"), 
        format = "file"),
    tar_target(run_env, command = run_environment_updates(env_fl, aq_fl, db_fl)),
    ### Communities Chapter ###
    tar_target(comm_fl, 
               file.path(data_dir, 
                         "Community Participation/Communities State of London data.xlsx"),
               format = "file"),
    tar_target(
        comm_resdash_fl,
        "Q:/Teams/D&PA/Social Policy/COVID-19 data/Recovery Dashboard data/BF data for Resilience Dashboard March 2021.xlsx",
        format = "file"),
    tar_target(htcrm_fl, 
               file.path(data_dir, 
                         "Community Participation/hate_crime.csv"),
               format = "file"),
    tar_target(run_comm, command = run_communities_updates(comm_fl, comm_resdash_fl, htcrm_fl, db_fl)),
    ### Transport and Infrastructure
    tar_target(trans_inf_fl, 
               file.path(data_dir, 
                         "Transport and Infrastructure/Transport & Infrastructure.xlsx"),
               format = "file"),
    tar_target(run_trans_inf, run_transport_infrastructure_updates(trans_inf_fl, db_fl)),
    
    ### Income/Deprivation Chapter ###
    tar_target(dep_fl,
               file.path(data_dir,
                         "Income, poverty and destitution/Data for IPD charts.xlsx"),
               format = "file"),
    tar_target(hh_uc,
               file.path(data_dir, "Income, poverty and destitution/hh_uc.csv"),
               format = "file"),
    tar_target(ucws,
               file.path(data_dir, "Income, poverty and destitution/pucws.csv"),
               format = "file"),
    tar_target(pchb,
               file.path(data_dir, "Income, poverty and destitution/pchb.csv"),
               format = "file"),
    tar_target(
        run_inc_dep,
        command = run_income_dep_updates(dep_fl, hh_uc, ucws, pchb, db_fl)
    ),
    ### Demography ###
    tar_target(dem_fl, file.path(data_dir,"Demography/demography_chapter_Dec23.xlsx"), format = "file"),
    tar_target(run_dem, command = run_population_updates(dem_fl, db_fl)),
    ### Economy ###
    tar_target(
        ec_skills_fl,
        file.path(data_dir, "The Economy and Labour Market/Skills_data.xlsx"),
        format = "file"),
    tar_target(
        ec_jobs_fl,
        file.path(data_dir, "The Economy and Labour Market/2023-12-01_jobs.xlsx"),
        format = "file"),
    tar_target(
        ec_bus_fl,
        file.path(data_dir, "The Economy and Labour Market/Econ_business_charts.xlsx"),
        format = "file"),
    tar_target(
        prv_jbs,
        "C:/Users/joheywood/Greater London Authority/IU - State of London report/Version 3 (June 2023)/Data/The Economy and Labour Market/2023-05-04_jobs_skills.xlsx",
        format = "file"),
    tar_target(run_econ, run_economy_updates(ec_skills_fl, ec_jobs_fl, ec_bus_fl, db_fl, prv_jbs)),
    ### Housing ###
    tar_target(hs_fl,
               file.path(data_dir, "Housing/SOL v4 Housing data workbook.xlsx"),
               format = "file"),
    tar_target(run_housing, run_housing_updates(hs_fl, db_fl)),
    ### Health/Wellbeing ### 
    tar_target(lbw_fl,
               file.path(data_dir, 
                         "Health and wellbeing/LBW_trends_for_London and England_forV4.csv"),
               format = "file"),
    tar_target(seneth_fl,
               file.path(data_dir, 
                         "Health and wellbeing/SEN_ethnicity_London_England_forV4.csv"),
               format = "file"),
    tar_target(smoke_fl,
               file.path(data_dir, 
                         "Health and wellbeing/Smoking_Housing_forV4.csv"),
               format = "file"),
    tar_target(run_hw, run_health_wellbeing_updates(lbw_fl, seneth_fl, smoke_fl, db_fl)),
    ### Crime ###
    tar_target(crm_fl,
               file.path(data_dir, 
                         "Crime and Safety/Crime and Safety Chapter - November 2023 - Data File.xlsx"),
               format = "file"),
    tar_target(run_crm, run_crime_updates(crm_fl, db_fl)),
    tar_target(cyp_fl,
               file.path(data_dir, 
                         "Young people and education/Chapter 10 - Children and Young People.xlsx"),
               format = "file"),
    tar_target(run_cyp, run_yp_education_updates(cyp_fl, db_fl))
    
)
