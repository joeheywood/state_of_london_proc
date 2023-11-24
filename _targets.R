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
# dbfl <-  "E:/project_folders/apps/db/dashboard_files/sqlite/sol_v4.db"
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
    tar_target(run_env, command = run_environment_updates(env_fl, db_fl)),
    ### Communities Chapter ###
    tar_target(comm_fl, 
               file.path(data_dir, 
                         "Community Participation/Communities State of London data.xlsx"),
               format = "file"),
    tar_target(
        comm_resdash_fl,
        "Q:/Teams/D&PA/Social Policy/COVID-19 data/Recovery Dashboard data/BF data for Resilience Dashboard March 2021.xlsx",
        format = "file"),
    tar_target(run_comm, command = run_communities_updates(comm_fl, comm_resdash_fl, db_fl)),
    ### Transport and Infrastructure
    tar_target(trans_inf_fl, 
               file.path(data_dir, 
                         "Transport and Infrastructure/Transport & Infrastructure.xlsx"),
               format = "file"),
    tar_target(run_trans_inf, run_transport_infrastructure_updates(trans_inf_fl, db_fl)),
    
    ### Income/Deprivation Chapter ###
    # tar_target(dep_fl, 
    #            file.path(data_dir,  
    #                      "Income, poverty and destitution/Data for IPD charts v3.xlsx"),
    #            format = "file"),
    # tar_target(hh_uc, 
    #            file.path(data_dir, "Income, poverty and destitution/hh_uc.csv"),
    #            format = "file"),
    # tar_target(ucws, 
    #            file.path(data_dir, "Income, poverty and destitution/pucws.csv"),
    #            format = "file"),
    # tar_target(
    #     run_inc_dep,
    #     command = run_income_dep_updates(dep_fl, hh_uc, ucws, dbfl)
    # ),
    ### Economy ###
    # tar_target(
    #     ec_bus_fl,  
    #     file.path(data_dir, "The Economy and Labour Market/Econ_business_charts.xlsx"), 
    #     format = "file"),
    # tar_target(
    #     jobs_skills_fl,  
    #     file.path(data_dir, "The Economy and Labour Market/2023-05-04_jobs_skills.xlsx"), 
    #     format = "file"),
    # tar_target(run_econ, command = run_business_labour_updates(ec_bus_fl, dbfl)),
    # tar_target(run_jobs_skills, command = run_jobs_skills_updates(jobs_skills_fl, dbfl)),
    ### Demography ###
    tar_target(dem_fl, file.path(data_dir,"Demography/demography_chapter_nov23.xlsx"), format = "file"),
    tar_target(run_dem, command = run_communities_updates(comm_fl, dbfl))
    
)
