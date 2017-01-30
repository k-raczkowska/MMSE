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

#pobiera dane z tabeli travistorrenta, buduje nowa tabele uzupelniona o kolumne author_mail, wypelnia tabele danymi na podstawie github api
insertAll <- function(dbName, dbHost, dbLogin, dbPassword, client_id, client_secret, newTableName, ttTableName, projects){
  mydb = RMySQL::dbConnect(RMySQL::MySQL(), user = dbLogin, password = dbPassword, dbname = dbName, host = dbHost)
  query <- paste("select distinct git_commit from ", ttTableName, " where author_mail IS NULL AND gh_project_name in(", projects, ")", sep = "")
  commits = DBI::dbGetQuery(mydb, query)
  print(nrow(commits))
}

#pobiera dane z tabeli utworzonej przez insertAll i wrzuca to do pliku .rda
createRda <- function(dbName, dbHost, dbLogin, dbPassword, ttTableName, projects, dataName){
  mydb = RMySQL::dbConnect(RMySQL::MySQL(), user = dbLogin, password = dbPassword, dbname = dbName, host = dbHost)
  queryResult = unique(DBI::dbGetQuery(mydb, paste("select tr_build_id, tr_status, author_mail, git_commit, gh_project_name,
                                       tr_started_at, gh_src_churn, gh_files_added, gh_files_modified, gh_files_deleted
                                       from ", ttTableName, " where gh_project_name in (", projects, ") order by tr_build_id")))
  print(nrow(queryResult))
  #commits = DBI::dbGetQuery(mydb, query)
}

#insertAll(dbName = 'travistorrent', dbHost = 'localhost', dbLogin = 'root', dbPassword = 'master', client_id = 'eb0e2d954e3c072e0e05',
#            client_secret = '59e73934db0cc7e51363d7c3b59c6daa35629b2b', newTableName = 'xyz', ttTableName = 'temptable',
#              projects = "'xyz', '47deg/appsly-android-rest', 'ActiveJpa/activejpa'")

#createRda(dbName = 'travistorrent', dbHost = 'localhost', dbLogin = 'root', dbPassword = 'master', ttTableName = 'temptable',
#          projects = "'xyz', '47deg/appsly-android-rest', 'ActiveJpa/activejpa'")
