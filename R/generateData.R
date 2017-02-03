
#' Function generates a data frame with metrics computed from dataset
#' as columns and with additional column with current build id.
#' Columns of data frame are presented below:
#' \itemize{
#'  \item{build}
#'  \item{lccfa}
#'  \item{lccpa}
#'  \item{lcpa}
#'  \item{lcfa}
#'  \item{rate}
#'  \item{sccfa}
#'  \item{sccpa}
#'  \item{scfa}
#'  \item{scpa}
#'  \item{tlfb}
#'  \item{tlpb}
#'  }
#' @examples count(10)
#' @param n number of rows of queryResult
#' @export
count <- function(n = nrow(queryResult)){
  result <- queryResult$tr_build_id
  result <- data.frame(
    build=numeric(n),
    lccfa=numeric(n),
    lccpa=numeric(n),
    lcpa=numeric(n),
    lcfa=numeric(n),
    rate=numeric(n),
    sccfa=numeric(n),
    sccpa=numeric(n),
    scfa=numeric(n),
    scpa=numeric(n),
    tlfb=numeric(n),
    tlpb=numeric(n)
  )
  for(i in 1:n){
    build <- queryResult[i,1]
    result$lccfa[i] <- lccfa(build)
    result$lccpa[i] <- lccpa(build)
    result$lcpa[i] <- lcpa(build)
    result$lcfa[i] <- lcfa(build)
    result$rate[i] <- rate(build)
    result$sccfa[i] <- sccfa(build)
    result$sccpa[i] <- sccpa(build)
    result$scfa[i] <- scfa(build)
    result$scpa[i] <- scpa(build)
    result$tlfb[i] <- tlfb(build)
    result$tlpb[i] <- tlpb(build)
  }
  return(result)
}

#' Function selects data from TravisTorrent table, creates new table (with additional column author_mail
#' with e-mail address of commit's author) and insert data into this new table. Values of author_mail column
#' are collected from Github using Github API
#'
#' @param dbName name of database
#' @param dbHost host of database
#' @param dbLogin user of database
#' @param dbPassword user's password to connect to database
#' @param clientId Github's client ID which is received when register to Github
#' @param clientSecret Github's client secret which is received when register to Github
#' @param newTableName name of a table which you want to create and insert data to
#' @param ttTableName name of TravisTorrent table which currently exists in your database
#' @param projects names of projects which you want to filter data table by, should be given
#'  as string with comma as delimiter
#' @export
insertToDB <- function(dbName, dbHost, dbLogin, dbPassword, clientId, clientSecret, newTableName, ttTableName, projects){
  p <- gsub(" ", "", projects)
  pros <- gsub(",", "','", p)
  mydb <- RMySQL::dbConnect(RMySQL::MySQL(), user = dbLogin, password = dbPassword, dbname = dbName, host = dbHost)
  DBI::dbExecute(mydb, paste("DROP TABLE IF EXISTS ", newTableName, sep = ""))
  DBI::dbExecute(mydb, paste("CREATE TABLE ", newTableName, " LIKE ", ttTableName, sep = ""))
  DBI::dbExecute(mydb, paste("INSERT INTO ", newTableName, " SELECT * FROM ", ttTableName, " WHERE gh_project_name IN ('", pros, "')
                             GROUP BY tr_build_id", sep = ""))
  DBI::dbExecute(mydb, paste("ALTER TABLE ", newTableName, " ADD COLUMN author_mail VARCHAR(100) ", sep = ""))
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
      print(commit$ok)
      if(commit$ok){
        print(gc)
        DBI::dbExecute(mydb, paste("UPDATE ", newTableName, " SET author_mail = '", commit$content$author$email, "'
                                    WHERE author_mail IS NULL AND git_commit = '", gc, "'", sep = ""))
      }
    }
  }
  selectRes <- unique(DBI::dbGetQuery(mydb, paste("select tr_build_id, tr_status, author_mail, git_commit, gh_project_name,
                                                   tr_started_at, gh_src_churn, gh_files_added, gh_files_modified, gh_files_deleted
                                                   from ", newTableName, sep = "")))
  return(selectRes)
}

#' Function replaces dataset queryResult with another data, selected by user
#' @param queryResult new data
#' @export
replaceDatasetRDA <- function(queryResult){
  save(queryResult, file = "data/queryRes.rda", compress = "xz")
  load(file = "data/queryRes.rda")
  return(queryResult)
}

#' Function get data from dataset alldata.rda which contains all build from sample TravisTorrent table
#'
#' @param clientId Github's client ID which is received when register to Github
#' @param clientSecret Github's client secret which is received when register to Github
#' @param projects names of projects which you want to filter data table by, should be given
#'  as string with comma as delimiter
#' @usage selectFromRDA(clientId, clientSecret, projects)
#' @export
selectFromRDA <- function(clientId, clientSecret, projects){
  if(exists("commits")){
    pp <- strsplit(gsub(" ", "", projects), ",")[[1]]
    print(pp)
    pr <- commits[commits$gh_project_name %in% pp,]
    print(nrow(pr))
    if(nrow(pr) > 0){
      pr["author_mail"] <- ""
      leng <- nrow(pr)
      print(leng)
      if(leng > 0){
        xx <- github::interactive.login(client_id = clientId, client_secret = clientSecret)
        for(i in 1:leng){
          gc <- pr$git_commit[i]
          pn <- pr$gh_project_name[i]
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
          }
        }
      }
    }
    return(pr)
  }
}

#' Function selects data from customTravisTorrent table and returns it
#'
#' @param dbName name of database
#' @param dbHost host of database
#' @param dbLogin user of database
#' @param dbPassword user's password to connect to database
#' @param ttTableName table from which you want to select
#' @export
createDefaultQueryResult <- function(dbName, dbHost, dbLogin, dbPassword, ttTableName){
  mydb = RMySQL::dbConnect(RMySQL::MySQL(), user = dbLogin, password = dbPassword, dbname = dbName, host = dbHost)
  queryResult = unique(DBI::dbGetQuery(mydb, paste("select tr_build_id, tr_status, author_mail, git_commit, gh_project_name, tr_started_at,
                                       gh_src_churn, gh_files_added, gh_files_modified, gh_files_deleted from ", ttTableName,
                                       " where gh_project_name in ('47deg/appsly-android-rest','ActiveJpa/activejpa',
                                       'AzureAD/azure-activedirectory-library-for-android','BaseXdb/basex','Berico-Technologies/CLAVIN',
                                       'BuildCraft/BuildCraft','CloudifySource/cloudify','CyberAgent/android-gpuimage','DSpace/DSpace',
                                       'GoogleCloudPlatform/DataflowJavaSDK','HubSpot/Singularity','JakeWharton/u2020','Jasig/cas',
                                       'ReactiveX/RxJava','SpongePowered/Sponge','SpongePowered/SpongeAPI','bitcoinj/bitcoinj',
                                       'bndtools/bnd','broadinstitute/picard','caelum/vraptor4','dropwizard/dropwizard','facebook/presto',
                                       'geoserver/geoserver','google/bazel','google/closure-compiler','gradle/gradle',
                                       'igniterealtime/Openfire','mozilla/MozStumbler','timmolter/XChange') order by tr_build_id", sep = "")))
  save(queryResult, file = "data/queryRes.rda", compress = "xz")
  return(queryResult)
}

isConnection <- function(){
  tryCatch({
    DBI::dbConnect(RMySQL::MySQL(), "travistorrent")
    TRUE
  }, error = function(...){
      message("Cannot connect to default TravisTorrent database")
    FALSE
  })
}
