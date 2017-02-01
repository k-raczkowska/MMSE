#install.packages("devtools")
#install.packages("jsonlite")
#devtools::install_github("cscheid/rgithub", force=TRUE)
#library('github')
#mydb = RMySQL::dbConnect(RMySQL::MySQL(), user = 'root', password = 'master', dbname = 'travistorrent', host = 'localhost')
#queryResult = unique(DBI::dbGetQuery(mydb, "select tr_build_id, tr_status, author_mail, git_commit, gh_project_name, tr_started_at, gh_src_churn, gh_files_added, gh_files_modified, gh_files_deleted from travistorrent_27_10_2016 order by tr_build_id"))
#commits <- unique(DBI::dbGetQuery(mydb, "select tr_build_id, tr_status, git_commit, gh_project_name, tr_started_at, gh_src_churn, gh_files_added, gh_files_modified, gh_files_deleted from travistorrent order by tr_build_id limit 100"))
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

#commits < data.frame()


#load(file = "C:/Users/Karolina R/Documents/alldata.rda")


#insertAll(dbName = 'travistorrent', dbHost = 'localhost', dbLogin = 'root', dbPassword = 'master', client_id = 'eb0e2d954e3c072e0e05',
#            client_secret = '59e73934db0cc7e51363d7c3b59c6daa35629b2b', newTableName = 'xyz', ttTableName = 'temptable',
#              projects = "'xyz', '47deg/appsly-android-rest', 'ActiveJpa/activejpa'")

#createRda(dbName = 'travistorrent', dbHost = 'localhost', dbLogin = 'root', dbPassword = 'master', ttTableName = 'temptable',
#          projects = "'xyz', '47deg/appsly-android-rest', 'ActiveJpa/activejpa'")
