#mydb = RMySQL::dbConnect(RMySQL::MySQL(), user = 'root', password = 'master', dbname = 'travistorrent', host = 'localhost')
#queryResult = unique(DBI::dbGetQuery(mydb, "select tr_build_id, tr_status, author_mail, git_commit, gh_project_name, tr_started_at, gh_src_churn, gh_files_added, gh_files_modified, gh_files_deleted from travistorrent_27_10_2016 order by tr_build_id"))
#q = DBI::dbGetQuery(mydb, "select tr_build_id, tr_status, author_mail, git_commit, gh_project_name, tr_started_at from travistorrent_27_10_2016 where gh_project_name = '47deg/appsly-android-rest' order by tr_build_id")
#save(queryResult, file = "data/queryRes.rda", compress = "xz")

load(file = 'data/queryRes.rda')

#' @importFrom utils head
NULL










#abc <- queryResult$tr_build_id
#for(i in 1:length(abc)){
#  lccfa(abc[i])
#}



#' Compute the squared error
#'
#' This function computes the elementwise squared error for a
#' number or a vector
#'
#' @param currentBuild id of travis-ci build
#' @export
tlpb <- function(currentBuild){
  projectName <- head(queryResult[queryResult$tr_build_id == currentBuild,]$gh_project_name,1)
  projectBuilds <- queryResult[queryResult$gh_project_name == projectName,]
  passed <- projectBuilds[projectBuilds$tr_status == 'passed',]
  #print(passed$tr_status)
  data <- passed[passed$tr_build_id < currentBuild,]
  current <- projectBuilds[projectBuilds$tr_build_id == currentBuild,]
  #print(current$tr_build_id)
  last <- data[order(data$tr_build_id, decreasing = TRUE),]
  l <- last[1,]
  #print(last$tr_build_id)
  x = difftime(current$tr_started_at, strptime(l$tr_started_at, format = "%Y-%m-%d"), units = "mins")
  y = as.numeric(x, units="mins")
  #print(x)
  #print(y)
  return(y)
}

#' Compute the squared error
#'
#' This function computes the elementwise squared error for a
#' number or a vector
#'
#' @param currentBuild id of travis-ci build
#' @export
tlfb <- function(currentBuild){
  projectName <- head(queryResult[queryResult$tr_build_id == currentBuild,]$gh_project_name,1)
  projectBuilds <- queryResult[queryResult$gh_project_name == projectName,]
  failed <- projectBuilds[projectBuilds$tr_status == 'failed' | projectBuilds$tr_status == 'errored',]
  #print(failed$tr_status)
  data <- failed[failed$tr_build_id < currentBuild,]
  current <- projectBuilds[projectBuilds$tr_build_id == currentBuild,]
  #print(current$tr_build_id)
  last <- data[order(data$tr_build_id, decreasing = TRUE),]
  l <- last[1,]
  #print(last$tr_build_id)
  x = difftime(current$tr_started_at, strptime(l$tr_started_at, format = "%Y-%m-%d"), units = "mins")
  y = as.numeric(x, units="mins")
  #print(x)
  #print(y)
  return(y)
}

#' Compute the squared error
#'
#' This function computes the elementwise squared error for a
#' number or a vector
#'
#' @param currentBuild id of travis-ci build
#' @export
rate <- function(currentBuild){
  projectName <- head(queryResult[queryResult$tr_build_id == currentBuild,]$gh_project_name,1)
  committer <- head(queryResult[queryResult$tr_build_id == currentBuild,]$author_mail,1)
  byName <- queryResult[queryResult$gh_project_name == projectName,]
  byCommitter <- byName[byName$author_mail == committer,]
  projectBuilds <- byCommitter[byCommitter$tr_build_id < currentBuild,]
  passed <- projectBuilds[projectBuilds$tr_status == 'passed',]
  failed <- projectBuilds[projectBuilds$tr_status == 'errored' | projectBuilds$tr_status == 'failed',]
  p <- nrow(passed)
  f <- nrow(failed)
  if(p+f > 0){
    return(p/(p+f))
  }
  else{
    return(0)
  }
}

#' Compute the squared error
#'
#' This function computes the elementwise squared error for a
#' number or a vector
#'
#' @param currentBuild id of travis-ci build
#' @export
weightedRate <- function(currentBuild){
  projectName <- head(queryResult[queryResult$tr_build_id == currentBuild,]$gh_project_name,1)
  committer <- head(queryResult[queryResult$tr_build_id == currentBuild,]$author_mail,1)
  byName <- queryResult[queryResult$gh_project_name == projectName,]
  byCommitter <- byName[byName$author_mail == committer,]
  projectBuilds <- byCommitter[byCommitter$tr_build_id < currentBuild,]
  c = nrow(projectBuilds)
  weightPassed = 0
  weightFailed = 0
  if(c > 0){
    for(i in 1:c){
      if(projectBuilds[i,2] == 'passed'){
        #code churn
        weightPassed = weightPassed + projectBuilds[i,7]
      }
      else if(projectBuilds[i,2] == 'errored' || projectBuilds[i,2] == 'failed'){
        weightFailed = weightFailed + projectBuilds[i,7]
      }
    }
  }
  if(!is.na(weightFailed) && !is.na(weightPassed) && weightFailed + weightPassed > 0){
    return(weightPassed/(weightFailed+weightPassed))
  }
  else{
    return(0)
  }
}



#' Compute the squared error
#'
#' This function computes the elementwise squared error for a
#' number or a vector
#'
#' @export
count <- function(){
  result <- queryResult$tr_build_id
  x <- nrow(queryResult)
  result <- data.frame(
    build=numeric(x),
    lccfa=numeric(x),
    lccpa=numeric(x),
    lcpa=numeric(x),
    lcfa=numeric(x),
    rate=numeric(x),
    sccfa=numeric(x),
    sccpa=numeric(x),
    scfa=numeric(x),
    scpa=numeric(x),
    tlfb=numeric(x),
    tlpb=numeric(x),
    wr=numeric(x)
  )
  for(i in 1:x){
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
