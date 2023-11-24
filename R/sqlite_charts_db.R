library(RSQLite)
library(dplyr)
library(tidyr)
library(glue)
library(stringr)

source("R/get_chart_data.R")

run_charts_db <- function() {
    fl <- "data/sol_v4_charts.db"
    closeAllConnections()
    file.remove(fl)
    cn <- dbConnect(RSQLite::SQLite(), fl)
    tbls <- dbListTables(cn)
    if(!"chartdat" %in% tbls) {
        dbSendQuery(cn, "create table chartdat(dtst text, xd text, b text, y real, text text, chart text);")
        dbSendQuery(cn, "create table chartmeta(dtst text primary key, chartmeta json);")
        dbSendQuery(cn, "create table meta(dtst text primary key, chapter text, title text, sub text, meta json);")
        dbSendQuery(cn, "create table updates(dtst text primary key, lastmax text, newvals text, timestamp text, execfile text, latest_newvals text);")
    } 
    chart_sqlite_environment(cn)
    chart_sqlite_communities(cn)
    chart_sqlite_trans_infr(cn)
    # chart_sqlite_demography(cn)
    # chart_sqlite_inc_pov_dest(cn)  
    # chart_sqlite_health_wellbeing(cn)
    # chart_sqlite_crime(cn)
    # ingest_econ_business(cn)
    # ingest_housing(cn)
    # chart_sqlite_yp_education(cn)  
    
    dbDisconnect(cn)
    
}

save_chart <- function(dtst, opts = list(), mopts = list(), conn) {
    tryCatch({
        ### combine opts, mopts above with defaults?
        d <- get_obs_chart(dtst, opts)# add mopts here eventually
        d$d <- d$d |> rename(dtst = dataset)
        chart_meta <- data.frame(dtst = dtst,
                                 chartmeta = as.character(
                                     toJSON(d$co, auto_unbox = T,
                                            pretty = TRUE)),
                                 stringsAsFactors = FALSE)
        
        meta <- data.frame(dtst = dtst,
                           chapter = d$m$sol_chapter,
                           title = d$m$ttl,
                           sub = d$m$sol_sub,
                           meta = as.character(
                               toJSON(d$m, auto_unbox = TRUE,
                                      pretty = TRUE)))
        # conn <- dbConnect(RSQLite::SQLite(), cdbfl)
        dbSendQuery(conn, glue("delete from chartdat where dtst = '{dtst}'"))
        dbSendQuery(conn, glue("delete from chartmeta where dtst = '{dtst}'"))
        dbSendQuery(conn, glue("delete from meta where dtst = '{dtst}'"))
        # dbSendQuery(conn, glue("delete from updates where dtst = '{dtst}'"))
        if(!"chart" %in% names(d$d)) d$d$chart <- ""
        dbAppendTable(
            conn, "chartdat", 
            d$d |> select(dtst, xd, y, b, text, chart))
        dbAppendTable(conn, "chartmeta", chart_meta)
        dbAppendTable(conn, "meta", meta)
        # dbAppendTable(conn, "updates", uu)
        TRUE
    }, error = function(e){
        print(glue("Not working: {dtst}: {e}"))
        FALSE
    })
    
}

chart_sqlite_environment <- function(conn) {
    save_chart("bio", conn = cn)
    save_chart("sol_greenhouse", conn = cn)
    save_chart("epc", conn = cn)
    save_chart("wst", conn = cn)
    save_chart("rcyc", conn = cn)
    
}


chart_sqlite_communities <- function(cn) {
    save_chart("fmlvol", conn = cn)
    save_chart("votereg", conn = cn)
    save_chart("infvol", conn = cn)
    save_chart("socact", conn = cn)
    save_chart("nghbel", conn = cn)
    save_chart("nghbtrst", conn = cn)
    save_chart("sol_nghb", conn = cn)
    save_chart("infloc", conn = cn)
    # sprt <- c("__by_xd__", "2018-19", "2021-22", "2022 (Feb)", "2022 (May)", 
    #           "2022 (Aug)", "2022 (Nov)", "2023 (Feb)")
    # save_for_SoL("sol_culture", section = sctn, cnn = cn, ordr = sprt)
    # save_for_SoL("sol_sport", section = sctn, cnn = cn, ordr = sprt) 
    save_chart("coh", conn = cn)
    save_chart("htcrm", conn = cn)
    save_chart("sol_phtcrm", conn = cn)
    save_chart("sol_o2mobility", conn = cn)
    save_chart("sol_hlp", conn = cn)
    save_chart("sol_insttrst", conn = cn)
}


chart_sqlite_trans_infr <- function(cn) {
    # save_for_SoL("tfl",   mvav = 7, section = sctn, cnn = cn)
    save_chart("sol_modesplit", conn = cn)
    save_chart("tfl_wlkcyc", conn = cn)
    save_chart("sol_actvtrav", conn = cn)
    save_chart("sol_rd_safety", conn = cn)
    save_chart("sol_bus_safety", conn = cn)
    save_chart("sol_accessibility", conn = cn)
    save_chart("sol_bus", conn = cn)
    save_chart("sol_tube", conn = cn)
    save_chart("sol_traffic", conn = cn)
    save_chart("sol_flfb", conn = cn)
    save_chart("sol_notspot", conn = cn)
    save_chart("sol_transp_aff", conn = cn)
}


# add_to_sqlite <- function(d, cm, m, uu, conn) {
#     dtst <- cm$dtst[1]
#     # print(dtst)
#     # print(glue("delete from chartdat where dataset = '{dtst}'"))
#     dbSendQuery(conn, glue("delete from chartdat where dtst = '{dtst}'"))
#     dbSendQuery(conn, glue("delete from chartmeta where dtst = '{dtst}'"))
#     dbSendQuery(conn, glue("delete from meta where dtst = '{dtst}'"))
#     dbSendQuery(conn, glue("delete from updates where dtst = '{dtst}'"))
#     dbAppendTable(conn, "chartdat", d )
#     dbAppendTable(conn, "chartmeta", cm)
#     dbAppendTable(conn, "meta", m)
#     dbAppendTable(conn, "updates", uu)
#     
# }
# 
