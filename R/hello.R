#mydb = RMySQL::dbConnect(RMySQL::MySQL(), user = 'root', password = 'master', dbname = 'travistorrent', host = 'localhost')
#queryResult = unique(DBI::dbGetQuery(mydb, "select tr_build_id, tr_status, author_mail, git_commit, gh_project_name, tr_started_at, gh_src_churn, gh_files_added, gh_files_modified, gh_files_deleted from travistorrent_27_10_2016 order by tr_build_id"))
#q = DBI::dbGetQuery(mydb, "select tr_build_id, tr_status, author_mail, git_commit, gh_project_name, tr_started_at from travistorrent_27_10_2016 where gh_project_name = '47deg/appsly-android-rest' order by tr_build_id")
#save(queryResult, file = "data/queryRes.rda", compress = "xz")


