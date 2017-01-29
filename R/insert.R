#install.packages("devtools")
#install.packages("jsonlite")
#devtools::install_github("r-pkgs/gh")

#my_repos2 <- gh::gh("GET /users/:username/repos", username = "gaborcsardi",
#                    type = "public", page = 2, .token = "e2681f474324a9e3b1cae72b4642cf1e6ebbc37d")
#vapply(my_repos2, "[[", "", "name")

insertAll <- function(dbName, dbHost, dbLogin, dbPassword, ghToken, newTableName, ttTableName){
  mydb = RMySQL::dbConnect(RMySQL::MySQL(), user = dbLogin, password = dbPassword, dbname = dbName, host = dbHost)
  #query <- paste("select distinct git_commit from ", ttTableName, " where author_mail IS NULL ", sep = "")
  #commits = DBI::dbGetQuery(mydb, query)
}
