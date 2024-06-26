library(git2r)


upload_charts_db_to_git <- function() {
    git_location <- "E:/project_folders/apps/db/dashboard_files"
    dbfl <- file.path(git_location, "sqlite/sol_v4_charts.db")
    file.copy("data/sol_v4_charts.db", dbfl, overwrite = TRUE)
    cred <- cred_token(token = "GITHUB_PAT")
    repo <- repository(git_location)
    pull(repo)
    add(repo, dbfl)
    commit(repo, message = paste0("Automated update: ", Sys.time()))
    push(repo, credentials = cred)
}