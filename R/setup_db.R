library(RSQLite)
library(vroom)
library(readxl)

# meta_file <- "Q:/"

setup_db <- function(fl, clear = TRUE, mtd = meta_file) {
    closeAllConnections()
    print(paste0("FILE EXISTS: ", file.exists(fl)))
    if(file.exists(fl)) stopifnot(file.access(fl) == 0)
    stopifnot(file.access(mtd) == 0)
    if(clear == TRUE) {
        if(file.exists(fl)) {
            file.remove(fl)
        } else {
            print("File does not exist")
        }
    }
    meta <- vroom(mtd)
    dbcon <- dbConnect(RSQLite::SQLite(), fl)
    dbWriteTable(dbcon, "mtd", meta, overrwrite = TRUE)
    ind_sql <- "CREATE TABLE ind_dat(
        dataset TEXT,
        xwhich INT,
        xvarchar TEXT,
        xvardt TEXT,
        yval NUMERIC,
        yvllb TEXT,
        text TEXT
    )
    "
    dbSendQuery(dbcon, ind_sql)
    dbDisconnect(dbcon)
    Sys.setenv(SOL_DB_PATH=fl)
    fl
}

refresh_meta <- function() {
    meta <- vroom("data/meta.csv")
    dbcon <- dbConnect(RSQLite::SQLite(), fl)
    dbWriteTable(dbcon, "mtd", meta, overrwrite = TRUE)
    dbDisconnect(dbcon)
    TRUE
}


