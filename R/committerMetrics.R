
#' Compute the longest chain of past passed committer's builds
#'
#' This function computes the longest chain of past passed
#' committer's builds.
#' Past passed committer's builds are builds which:
#' \itemize{
#'  \item{ran on the same repository as current build,}
#'  \item{commit which triggered these builds was made
#'  by the same person who made commit which triggered current build,}
#'  \item{completed with status 'passed'}
#' }
#'
#' @param currentBuild id of travis-ci build
#' @examples lccpa(1763098)
#' @export
lccpa <- function(currentBuild){
  projectName <- head(queryResult[queryResult$tr_build_id == currentBuild,]$gh_project_name,1)
  committer <- head(queryResult[queryResult$tr_build_id == currentBuild,]$author_mail,1)
  projectBuilds <- queryResult[queryResult$gh_project_name == projectName,]
  committerBuilds <- queryResult[queryResult$author_mail == committer,]
  data <- committerBuilds[committerBuilds$tr_build_id < currentBuild,]
  c = nrow(data)
  count = 0
  max = 0
  if(c > 0){
    for(i in 1:c){
      if(data[i,2] == 'passed'){
        count = count + 1
      }
      else
        count = 0
      if(count > max){
        max = count
      }
    }
  }
  return(max)
}

#' Compute the longest chain of past failed committer's builds
#'
#' This function computes the longest chain of past failed
#' committer's builds.
#' Past failed committer's builds are builds which:
#' \itemize{
#'  \item{ran on the same repository as current build,}
#'  \item{commit which triggered these builds was made
#'  by the same person who made commit which triggered current build,}
#'  \item{completed with status 'failed' or 'errored'}
#' }
#'
#' @param currentBuild id of travis-ci build
#' @examples lccfa(1763098)
#' @export
lccfa <- function(currentBuild, newData){
  if(missing(newData)){
    newData = queryResult
  }
  projectName <- head(newData[newData$tr_build_id == currentBuild,]$gh_project_name,1)
  committer <- head(newData[newData$tr_build_id == currentBuild,]$author_mail,1)
  projectBuilds <- newData[newData$gh_project_name == projectName,]
  committerBuilds <- projectBuilds[projectBuilds$author_mail == committer,]
  data <- committerBuilds[committerBuilds$tr_build_id < currentBuild,]
  c = nrow(data)
  count = 0
  max = 0
  if(c > 0){
    for(i in 1:c){
      if(data[i,2] == 'failed' || data[i,2] == 'errored'){
        count = count + 1
      }
      else
        count = 0
      if(count > max){
        max = count
      }
    }
  }
  return(max)
}

#' Compute the shortest chain of past passed committer's builds
#'
#' This function computes the shortest chain of past passed
#' committer's builds.
#' Past passed committer's builds are builds which:
#' \itemize{
#'  \item{ran on the same repository as current build,}
#'  \item{commit which triggered these builds was made
#'  by the same person who made commit which triggered current build,}
#'  \item{completed with status 'passed'}
#' }
#'
#' @param currentBuild id of travis-ci build
#' @examples sccpa(1763098)
#' @export
sccpa <- function(currentBuild){
  projectName <- head(queryResult[queryResult$tr_build_id == currentBuild,]$gh_project_name,1)
  committer <- head(queryResult[queryResult$tr_build_id == currentBuild,]$author_mail,1)
  projectBuilds <- queryResult[queryResult$gh_project_name == projectName,]
  committerBuilds <- queryResult[queryResult$author_mail == committer,]
  data <- committerBuilds[committerBuilds$tr_build_id < currentBuild,]
  c = nrow(data)
  count = 0
  min = c+1
  if(c > 0){
    for(i in 1:c){
      if(data[i,2] == 'passed'){
        count = count + 1
      }
      else{
        if((count < min && count != 0)){
          min = count
        }
        count = 0
      }
    }
  }

  if(min == c+1){
    min = count
  }
  return(min)
}

#' Compute the shortest chain of past failed committer's builds
#'
#' This function computes the shortest chain of past failed
#' committer's builds.
#' Past failed committer's builds are builds which:
#' \itemize{
#'  \item{ran on the same repository as current build,}
#'  \item{commit which triggered these builds was made
#'  by the same person who made commit which triggered current build,}
#'  \item{completed with status 'failed' or 'errored'}
#' }
#'
#' @param currentBuild id of travis-ci build
#' @examples sccfa(1763098)
#' @export
sccfa <- function(currentBuild){
  projectName <- head(queryResult[queryResult$tr_build_id == currentBuild,]$gh_project_name,1)
  committer <- head(queryResult[queryResult$tr_build_id == currentBuild,]$author_mail,1)
  projectBuilds <- queryResult[queryResult$gh_project_name == projectName,]
  committerBuilds <- queryResult[queryResult$author_mail == committer,]
  data <- committerBuilds[committerBuilds$tr_build_id < currentBuild,]
  c = nrow(data)
  count = 0
  min = c+1
  if(c > 0){
    for(i in 1:c){
      if(data[i,2] == 'failed' || data[i,2] == 'errored'){
        count = count + 1
      }
      else{
        if((count < min && count != 0)){
          min = count
        }
        count = 0
      }
    }
  }

  if(min == c+1){
    min = count
  }
  return(min)
}
