

run_charts_db <- function() {
    fl <- "data/sol_v4_charts.db"
    closeAllConnections()
    file.remove(fl)
    cn <- dbConnect(RSQLite::SQLite(), fl)
    tbls <- dbListTables(cn)
    if(!"chartdat" %in% tbls) {
        dbSendQuery(cn, "create table chartdat(dtst text, xd text, b text, y real, text text, chart text);")
        dbSendQuery(cn, "create table chartmeta(dtst text primary key, chartmeta json);")
        dbSendQuery(cn, "create table meta(dtst text primary key, chapter text, section text, title text, meta json);")
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

save_chart <- function(dtst, opts = list(), mopts = list(), section, conn) {
    tryCatch({
        ### combine opts, mopts above with defaults?
        d <- get_chart_from_observable(dtst, opts)# add mopts here eventually
        d$d <- d$d |> rename(dtst = dataset)
        chart_meta <- data.frame(dtst = dtst,
                                 chartmeta = as.character(
                                     toJSON(d$co, auto_unbox = T,
                                            pretty = TRUE)),
                                 stringsAsFactors = FALSE)
        
        meta <- data.frame(dtst = dtst,
                           #chapter = chapter,
                           section = section,
                           title = mopts$title,
                           meta = as.character(
                               toJSON(d$m, auto_unbox = TRUE,
                                      pretty = TRUE)))
        # conn <- dbConnect(RSQLite::SQLite(), cdbfl)
        dbSendQuery(conn, glue("delete from chartdat where dtst = '{dtst}'"))
        dbSendQuery(conn, glue("delete from chartmeta where dtst = '{dtst}'"))
        dbSendQuery(conn, glue("delete from meta where dtst = '{dtst}'"))
        # dbSendQuery(conn, glue("delete from updates where dtst = '{dtst}'"))
        dbAppendTable(conn, "chartdat", d$d )
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
    save_chart("bio", list(), list(title = "Recycling or whatever"), "Environment", conn)
    
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
