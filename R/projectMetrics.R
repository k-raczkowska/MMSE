#' Compute the longest chain of past passed builds
#'
#' This function computes the longest chain of past passed builds.
#' Past passed builds are builds which ran on the same repository
#' as current build and completed with status 'passed'
#'
#' @param currentBuild id of travis-ci build
#' @examples lcpa(1763098)
#' @export
lcpa <- function(currentBuild){
  projectName <- head(queryResult[queryResult$tr_build_id == currentBuild,]$gh_project_name,1)
  projectBuilds <- queryResult[queryResult$gh_project_name == projectName,]
  data <- projectBuilds[projectBuilds$tr_build_id < currentBuild,]
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

#' Compute the longest chain of past failed builds
#'
#' This function computes the longest chain of past failed builds.
#' Past failed builds are builds which ran on the same repository
#' as current build and completed with status 'failed' or 'errored'
#'
#' @param currentBuild id of travis-ci build
#' @examples lcfa(1763098)
#' @export
lcfa <- function(currentBuild){
  projectName <- head(queryResult[queryResult$tr_build_id == currentBuild,]$gh_project_name,1)
  projectBuilds <- queryResult[queryResult$gh_project_name == projectName,]
  data <- projectBuilds[projectBuilds$tr_build_id < currentBuild,]
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

#' Compute the shortest chain of past passed builds
#'
#' This function computes the shortest chain of past passed builds.
#' Past passed builds are builds which ran on the same repository
#' as current build and completed with status 'passed'
#'
#' @param currentBuild id of travis-ci build
#' @examples scpa(1763098)
#' @export
scpa <- function(currentBuild){
  projectName <- head(queryResult[queryResult$tr_build_id == currentBuild,]$gh_project_name,1)
  projectBuilds <- queryResult[queryResult$gh_project_name == projectName,]
  data <- projectBuilds[projectBuilds$tr_build_id < currentBuild,]
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

#' Compute the shortest chain of past failed builds
#'
#' This function computes the shortest chain of past failed builds.
#' Past failed builds are builds which ran on the same repository
#' as current build and completed with status 'failed' or 'errored'
#'
#' @param currentBuild id of travis-ci build
#' @examples scfa(1763098)
#' @export
scfa <- function(currentBuild){
  projectName <- head(queryResult[queryResult$tr_build_id == currentBuild,]$gh_project_name,1)
  projectBuilds <- queryResult[queryResult$gh_project_name == projectName,]
  data <- projectBuilds[projectBuilds$tr_build_id < currentBuild,]
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
