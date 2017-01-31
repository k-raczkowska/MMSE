#install.packages("devtools")
#install.packages("jsonlite")
#devtools::install_github("cscheid/rgithub", force=TRUE)
#library('github')
#mydb = RMySQL::dbConnect(RMySQL::MySQL(), user = 'root', password = 'master', dbname = 'travistorrent', host = 'localhost')
#queryResult = unique(DBI::dbGetQuery(mydb, "select tr_build_id, tr_status, author_mail, git_commit, gh_project_name, tr_started_at, gh_src_churn, gh_files_added, gh_files_modified, gh_files_deleted from travistorrent_27_10_2016 order by tr_build_id"))
#commits <- unique(DBI::dbGetQuery(mydb, "select tr_build_id, tr_status, git_commit, gh_project_name, tr_started_at, gh_src_churn, gh_files_added, gh_files_modified, gh_files_deleted from travistorrent order by tr_build_id"))
#save(commits, file = "data/alldata.rda", compress = "xz")

#my_repos2 <- gh::gh("GET /users/:username/repos", username = "gaborcsardi",
#                    type = "public", page = 2, .token = 'b1ada749c2afb717b3024e9e0231995d91fb7e75')
#.token = "e2681f474324a9e3b1cae72b4642cf1e6ebbc37d"
#ctx <- github::create.github.context(access_token = 'b1ada749c2afb717b3024e9e0231995d91fb7e75', api_url = 'https://github.com/k-raczkowska/RMeter')
#xx <- github::interactive.login(client_id = 'eb0e2d954e3c072e0e05', client_secret = '59e73934db0cc7e51363d7c3b59c6daa35629b2b')
#commit <- github::get.commit(owner = 'cscheid', repo = 'rgithub', sha = 'b8e010f2ac477d33927e19f6c22fe016a4618cc8')
#print(commit$ok)
#commit2 <- github::get.commit(owner = 'cscheid', repo = 'rgithub', sha = 'b8e010f2ac477d33927e19f6c22fe016a4618cc8')
#ctx <- github::create.github.context(client_id = 'eb0e2d954e3c072e0e05', client_secret = '59e73934db0cc7e51363d7c3b59c6daa35629b2b', api_url = 'https://github.com/k-raczkowska/RMeter')
#print(commit$content$author$email)

#vapply(my_repos2, "[[", "", "name")

load(file = "data/alldata.rda")

#pobiera dane z tabeli travistorrenta, buduje nowa tabele uzupelniona o kolumne author_mail, wypelnia tabele danymi na podstawie github api
#' @export
insertFromDB <- function(dbName, dbHost, dbLogin, dbPassword, clientId, clientSecret, newTableName, ttTableName, projects){
  mydb <- RMySQL::dbConnect(RMySQL::MySQL(), user = dbLogin, password = dbPassword, dbname = dbName, host = dbHost)
  DBI::dbExecute(mydb, paste("DROP TABLE IF EXISTS ", newTableName, sep = ""))
  DBI::dbExecute(mydb, paste("CREATE TABLE ", newTableName, " LIKE ", ttTableName, sep = ""))
  DBI::dbExecute(mydb, paste("INSERT INTO ", newTableName, " SELECT * FROM ", ttTableName, " WHERE gh_project_name IN (", projects, ") GROUP BY tr_build_id", sep = ""))
  DBI::dbExecute(mydb, paste("ALTER TABLE ", newTableName, " ADD COLUMN author_mail VARCHAR(100) ", sep = ""))
  #print(q)
  #DBI::dbExecute(mydb, )
  query <- paste("select distinct git_commit, gh_project_name from ", newTableName, " where author_mail IS NULL ", sep = "")
  commits <- DBI::dbGetQuery(mydb, query)
  lenght <- nrow(commits)
  if(lenght > 0){
    xx <- github::interactive.login(client_id = clientId, client_secret = clientSecret)
    for(i in 1:lenght){
      gc <- commits[i,1]
      pn <- commits[i,2]
      splitted <- strsplit(pn, "/")[[1]]
      own <- splitted[1]
      rep <- splitted[2]
      commit <- github::get.commit(owner = own, repo = rep, sha = gc)
      if(commit$ok){
        print(gc)
        DBI::dbExecute(mydb, paste("UPDATE ", newTableName, " SET author_mail = '", commit$content$author$email, "' WHERE author_mail IS NULL
                                   AND git_commit = '", gc, "'", sep = ""))
      }
    }
  }
  print(nrow(commits))
  return(commits)
}

#' @export
insertFromRDA <- function(clientId, clientSecret, projects){
  pp <- strsplit(gsub(" ", "", projects), ",")[[1]]
  #print(pp)
  pr <- commits[commits$gh_project_name %in% pp,]
  pr["author_mail"] <- ""
  #print(nrow(pr))
  leng <- nrow(pr)
  print(leng)
  if(leng > 0){
    xx <- github::interactive.login(client_id = clientId, client_secret = clientSecret)
    for(i in 1:leng){
      #print("AAA")
      gc <- pr$git_commit[i]
      pn <- pr$gh_project_name[i]
      #print(gc)
      #print(pr)
      splitted <- strsplit(pn, "/")[[1]]
      own <- splitted[1]
      rep <- splitted[2]
      commit <- github::get.commit(owner = own, repo = rep, sha = gc)
      print(paste(pn, "/commits/", gc, sep = ""))
      print(commit$ok)
      if(commit$ok){
        print(gc)
        mail <- commit$content$author$email
        print(mail)
        pr$author_mail[i] <- mail
        #DBI::dbExecute(mydb, paste("UPDATE ", newTableName, " SET author_mail = '", commit$content$author$email, "' WHERE author_mail IS NULL
        #                           AND git_commit = '", gc, "'", sep = ""))
      }
    }
    #print(pr)
  }
    return(pr)
}

#' @export
replaceDatasetRDA <- function(queryResult){
  save(queryResult, file = "data/queryRes.rda", compress = "xz")
  load(file = "data/queryRes.rda")
  return(queryResult)
}

#pobiera dane z tabeli utworzonej przez insertAll i wrzuca to do pliku .rda
createRda <- function(dbName, dbHost, dbLogin, dbPassword, ttTableName, projects){
  mydb = RMySQL::dbConnect(RMySQL::MySQL(), user = dbLogin, password = dbPassword, dbname = dbName, host = dbHost)
  queryResult = unique(DBI::dbGetQuery(mydb, paste("select tr_build_id, tr_status, author_mail, git_commit, gh_project_name,
                                       tr_started_at, gh_src_churn, gh_files_added, gh_files_modified, gh_files_deleted
                                       from ", ttTableName, " where gh_project_name in (", projects, ") order by tr_build_id")))

  print(nrow(queryResult))
  #commits = DBI::dbGetQuery(mydb, query)M
}

#insertAll(dbName = 'travistorrent', dbHost = 'localhost', dbLogin = 'root', dbPassword = 'master', client_id = 'eb0e2d954e3c072e0e05',
#            client_secret = '59e73934db0cc7e51363d7c3b59c6daa35629b2b', newTableName = 'xyz', ttTableName = 'temptable',
#              projects = "'xyz', '47deg/appsly-android-rest', 'ActiveJpa/activejpa'")

#createRda(dbName = 'travistorrent', dbHost = 'localhost', dbLogin = 'root', dbPassword = 'master', ttTableName = 'temptable',
#          projects = "'xyz', '47deg/appsly-android-rest', 'ActiveJpa/activejpa'")
