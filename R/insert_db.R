library(RSQLite)
library(glue)
library(cli)


insert_db_aq <- function(dat, log, excfl) {
    message("... sqlite AQ ...")
    sqlite_dpth <- "data/sol_v4.db"
    conn <- dbConnect(SQLite(), sqlite_dpth)
    qryn2 <- dbSendQuery(conn = conn, "DELETE FROM ind_dat WHERE dataset = 'aq_no2'")
    qrypm10 <- dbSendQuery(conn = conn, "DELETE FROM ind_dat WHERE dataset = 'aq_pm10'")
    qrypm25 <- dbSendQuery(conn = conn, "DELETE FROM ind_dat WHERE dataset = 'aq_pm25'")
    dbAppendTable(conn, "ind_dat", dat)
    dbDisconnect(conn)
    TRUE
    
}

# Function to add data to database for a given dataset ####
# Args: dat (df) data to add for a given dataset
#       log (character) file path to send messages/warnings to (console if "")
#       excfl (character file path of file used)
insert_db <- function(dat, log = "", excfl = "") {
    message("... sqlite ...")
    sqlite_dpth <- "data/sol_v4.db"
    clmns <- c("dataset", "xwhich", "xvarchar", "xvardt", 
               "yval", "yvllb", "text")
    #### Some error handling. Make sure there is data and all columns are there ####
    if(nrow(dat) == 0) {
        stop(glue("ERROR. NO DATA {excfl}"))
    } else if(!all(clmns %in% names(dat))) {
        dtnms <- paste(names(dat), collapse = "|") 
        expnms <- paste(clmns, collapse = "|") 
        mss <- clmns[which(!clmns %in% names(dat))] #names(dat)[which(!names(dat) %in% clmns)]
        message(glue("In data: {dtnms}"))
        message(glue("Expecting: {clmns}"))
        message(glue("Missing: {paste(mss, collapse = '|')}"))
        stop("ERROR. You need all the columns")
    } else {
        dat <- dat[, clmns]
        dtst <- unique(dat$dataset)
        
        #### Give some info to user about y/x ranges ####
        
        cat(glue("\n\n############ ADDING {dtst} ####################\n\n"), 
            file = log, append = TRUE)
        if(dat$xwhich[1] == 2) {
            dat$xvardt <- as.Date(dat$xvardt, origin = "1970-1-1")
            mindt <- format(min(dat$xvardt), "%d %b '%y")
            maxdt <- format(max(dat$xvardt), "%d %b '%y")
            xmsg <- glue("Xaxis Date. Range:{mindt} - {maxdt}\n\n")
            dat$xvardt <- format(dat$xvardt, "%Y-%m-%d")
        } else {
            xmsg <- glue("Xaxis Character: {toString(unique(dat$xvarchar))}\n\n")
            if(all(is.numeric(dat$xvarchar)) && all(dat$xvarchar %in% 1990:2040)) {
                dat$xvarchar <- as.character(round(dat$xvarchar))
            }
        }
        
        ##### Adds data here ####
        
        conn <- dbConnect(SQLite(), sqlite_dpth)
        m <- tbl(conn, "mtd") %>% 
            filter(dataset == dtst) %>% 
            collect()
        if(nrow(m) == 0) {
            cat("NO METADATA ASSOCIATED WITH THIS DATA\n\n", file = log)
            
        } else {
            cat(m$charttitle, "\n\n", file = log, append = TRUE)
        }
        cat(glue("Y range: {min(dat$yval)} - {max(dat$yval)}\n\n"), 
            file = log, append = TRUE)
        cat(xmsg, file = log, append = TRUE)
        
        if("ind_dat" %in% dbListTables(conn)) {
            lmx <- ""
            last_max <- NA
            new_vals <- FALSE
            #### removes existing data for this dataset first, to avoid duplicates #### 
            prev <- tbl(conn, "ind_dat") %>% 
                filter(dataset == dtst) %>% 
                collect()
            # print(tail(prev))
            if(nrow(prev) > 0) {
                if(prev$xwhich[1] == 2) {
                    last_max <- as.Date(max(prev$xvardt), origin = "1970-1-1")
                    new_vals <- max(dat$xvardt) > last_max
                    last_max <- as.character(last_max)
                } else {
                    last_max <- max(prev$xvarchar)
                    new_vals <- max(dat$xvarchar) > last_max
                }
                
                lmx <- glue("Last max: {last_max}\n\n")
            } else {
                lmx <- "Last max: N/A"
            }
            gsql <- glue_sql("DELETE FROM ind_dat WHERE dataset = {dtst}", 
                             .con = conn)
            qry <- dbSendQuery(conn = conn, gsql)
            rws <- dbGetRowsAffected(qry)
            cat(glue("Number of rows deleted: {rws}\n\n"), file = log, append = TRUE)
            cat(lmx, file = log, append = TRUE)
            dbClearResult(qry)
            dbAppendTable(conn, "ind_dat", dat)
            cat(glue("Number of rows added: {nrow(dat)}\n\n"), 
                file = log, append = TRUE)
        } else {
            dbWriteTable(conn, "ind_dat", dat)
            print(glue("Number of rows added: {nrow(dat)}\n\n"), file = log, 
                  append = TRUE)
        }
        upds <- data.frame(dataset = dat$dataset[1], 
                           lastmax = last_max,
                           newvals = new_vals,
                           timestamp = Sys.time(),
                           execfile = excfl)
        
        #### adds updates here ####
        
        if("updates" %in% dbListTables(conn)) {
            dbAppendTable(conn, "updates", upds)
        } else {
            dbWriteTable(conn, "updates", upds)
        }
        dbDisconnect(conn) 
        TRUE
    }
    
}

error_log <- function(e, dtst) {
    message("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
    cli_alert_danger(glue("ERROR in {dtst}"))
    message("CAll", e$call)
    message(e$message)
    message("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
}


