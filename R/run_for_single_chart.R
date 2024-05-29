
source("R/sqlite_charts_db.R")


## Function to run a single chart from start to finish
## Args:
##   dtst - the dataset code (see data/meta.csv for more info)
##   opts - chart options see code below for what these should be
##   mopts - meta options see code below
##   pp - preproceessing option. eg when the data needs to be ordered in a
##        particular way before being added.
## Value: TRUE if runs ok. There's no try/catch here becuase you'll want to see
##        the error message
run_for_single_chart <- function(dtst, opts, mopts, pp) {
    ## Sets up a sqlite database that can be hosted on observable
    fl <- "data/single_chart.db"
    closeAllConnections()
    file.remove(fl)
    cn <- dbConnect(RSQLite::SQLite(), fl)
    tbls <- dbListTables(cn)
    if(!"chartdat" %in% tbls) {
        dbSendQuery(cn, "create table chartdat(dtst text, xd text, b text, y real, text text, chart text);")
        dbSendQuery(cn, "create table chartmeta(dtst text primary key, chartmeta json);")
        dbSendQuery(cn, "create table meta(dtst text primary key, chapter text, title text, sub text, last_update text, last_check text, max_xval text, meta json);")
        dbSendQuery(cn, "create table updates(dtst text primary key, lastmax text, newvals text, timestamp text, execfile text, latest_newvals text);")
    } 
    save_chart(dtst, opts, mopts, pp, cn)
    dbDisconnect(cn)
    convert_all_to_svg()
    TRUE
}