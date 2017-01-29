#install.packages("devtools")
#install.packages("jsonlite")
#devtools::install_github("cscheid/rgithub", force=TRUE)
#library('github')

#my_repos2 <- gh::gh("GET /users/:username/repos", username = "gaborcsardi",
#                    type = "public", page = 2, .token = 'b1ada749c2afb717b3024e9e0231995d91fb7e75')
#.token = "e2681f474324a9e3b1cae72b4642cf1e6ebbc37d"
#ctx <- github::create.github.context(access_token = 'b1ada749c2afb717b3024e9e0231995d91fb7e75', api_url = 'https://github.com/k-raczkowska/RMeter')
#xx <- github::interactive.login(client_id = 'eb0e2d954e3c072e0e05', client_secret = '59e73934db0cc7e51363d7c3b59c6daa35629b2b')
#commit <- github::get.commit(owner = 'cscheid', repo = 'rgithub', sha = 'b8e010f2ac477d33927e19f6c22fe016a4618cc8')
#commit2 <- github::get.commit(owner = 'cscheid', repo = 'rgithub', sha = 'b8e010f2ac477d33927e19f6c22fe016a4618cc8')
#ctx <- github::create.github.context(client_id = 'eb0e2d954e3c072e0e05', client_secret = '59e73934db0cc7e51363d7c3b59c6daa35629b2b', api_url = 'https://github.com/k-raczkowska/RMeter')
#print(commit$content$author$email)

#vapply(my_repos2, "[[", "", "name")

insertAll <- function(dbName, dbHost, dbLogin, dbPassword, ghToken, newTableName, ttTableName){
  mydb = RMySQL::dbConnect(RMySQL::MySQL(), user = dbLogin, password = dbPassword, dbname = dbName, host = dbHost)
  query <- paste("select distinct git_commit from ", ttTableName, " where author_mail IS NULL ", sep = "")
  commits = DBI::dbGetQuery(mydb, query)
}
