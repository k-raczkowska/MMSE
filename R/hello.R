#mydb = RMySQL::dbConnect(RMySQL::MySQL(), user = 'root', password = 'master', dbname = 'travistorrent', host = 'localhost')
#queryResult = unique(DBI::dbGetQuery(mydb, "select tr_build_id, tr_status, author_mail, git_commit, gh_project_name, tr_started_at, gh_src_churn, gh_files_added, gh_files_modified, gh_files_deleted from travistorrent_27_10_2016 order by tr_build_id"))
#q = DBI::dbGetQuery(mydb, "select tr_build_id, tr_status, author_mail, git_commit, gh_project_name, tr_started_at from travistorrent_27_10_2016 where gh_project_name = '47deg/appsly-android-rest' order by tr_build_id")
#save(queryResult, file = "data/queryRes.rda", compress = "xz")

load(file = 'data/queryRes.rda')

#x <- unique(queryResult)

#replaceDataset <- function(newData){
#  queryResult = newData
#  save(newData, file = "queryRes.rda")
#  load(file = "queryRes.rda")
#}

#replaceDataset(q)



#' @importFrom utils head
NULL

#' Compute the squared error
#'
#' This function computes the elementwise squared error for a
#' number or a vector
#'
#' @param currentBuild id of travis-ci build
#' @export
lcpa <- function(currentBuild){
  projectName <- head(queryResult[queryResult$tr_build_id == currentBuild,]$gh_project_name,1)
  projectBuilds <- queryResult[queryResult$gh_project_name == projectName,]
  data <- projectBuilds[projectBuilds$tr_build_id < currentBuild,]
  c = nrow(data)
  #print(c)
  count = 0
  max = 0
  if(c > 0){
    for(i in 1:c){
      #print(data[i,2])
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
  #print(max)
  return(max)
}


#' Compute the squared error
#'
#' This function computes the elementwise squared error for a
#' number or a vector
#'
#' @param currentBuild id of travis-ci build
#' @export
lcfa <- function(currentBuild){
  projectName <- head(queryResult[queryResult$tr_build_id == currentBuild,]$gh_project_name,1)
  projectBuilds <- queryResult[queryResult$gh_project_name == projectName,]
  data <- projectBuilds[projectBuilds$tr_build_id < currentBuild,]
  c = nrow(data)
  #print(c)
  count = 0
  max = 0
  if(c > 0){
    for(i in 1:c){
      #print(data[i,2])
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
  #print(max)
  return(max)
}

#' Compute the squared error
#'
#' This function computes the elementwise squared error for a
#' number or a vector
#'
#' @param currentBuild id of travis-ci build
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

#' Compute the squared error
#'
#' This function computes the elementwise squared error for a
#' number or a vector
#'
#' @param currentBuild id of travis-ci build
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

#' Compute the squared error
#'
#' This function computes the elementwise squared error for a
#' number or a vector
#'
#' @param currentBuild id of travis-ci build
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

#' Compute the squared error
#'
#' This function computes the elementwise squared error for a
#' number or a vector
#'
#' @param currentBuild id of travis-ci build
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

#' Compute the squared error
#'
#' This function computes the elementwise squared error for a
#' number or a vector
#'
#' @param currentBuild id of travis-ci build
#' @param newData id of travis-ci build
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
  #print(c)
  count = 0
  max = 0
  if(c > 0){
    for(i in 1:c){
      #print(i)
      #print(data[i,2])
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
  #print(currentBuild)
  #print(max)
  return(max)
}

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
lccpa <- function(currentBuild){
  projectName <- head(queryResult[queryResult$tr_build_id == currentBuild,]$gh_project_name,1)
  committer <- head(queryResult[queryResult$tr_build_id == currentBuild,]$author_mail,1)
  projectBuilds <- queryResult[queryResult$gh_project_name == projectName,]
  committerBuilds <- queryResult[queryResult$author_mail == committer,]
  data <- committerBuilds[committerBuilds$tr_build_id < currentBuild,]
  c = nrow(data)
  #print(c)
  count = 0
  max = 0
  if(c > 0){
    for(i in 1:c){
      #print(data[i,2])
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
  #print(max)
  return(max)
}

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
        weightPassed = weightPassed + projectBuilds[i,7]/(projectBuilds[i,8]+projectBuilds[i,9]+projectBuilds[i,10])
      }
      else if(projectBuilds[i,2] == 'errored' || projectBuilds[i,2] == 'failed'){
        weightFailed = weightFailed + projectBuilds[i,7]/(projectBuilds[i,8]+projectBuilds[i,9]+projectBuilds[i,10])
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

#tlb(1763098)

#' Compute the squared error
#'
#' This function computes the elementwise squared error for a
#' number or a vector
#'
#' @param currentBuild id of travis-ci build
#' @param qresult id of travis-ci build
#' @export
lcpa2 <- function(currentBuild,qresult){
  projectName <- head(qresult[qresult$tr_build_id == currentBuild,]$gh_project_name,1)
  projectBuilds <- qresult[qresult$gh_project_name == projectName,]
  data <- projectBuilds[projectBuilds$tr_build_id < currentBuild,]
  c = nrow(data)
  #print(c)
  count = 0
  max = 0
  for(i in 1:c){
    #print(data[i,2])
    if(data[i,2] == 'passed'){
      count = count + 1
    }
    else
      count = 0
    if(count > max){
      max = count
    }
  }
  #print(max)
  return(max)
}

#' Compute the squared error
#'
#' This function computes the elementwise squared error for a
#' number or a vector
#'
#' @export
count <- function(){
  x <- nrow(queryResult)
  for(i in 1:x){
    build <- queryResult[i,1]
    print(paste("NUMER: ",i))
    print(MetricsR::lccfa(build))
    print(MetricsR::lccpa(build))
    print(MetricsR::lcpa(build))
    print(MetricsR::lcfa(build))
    print(MetricsR::rate(build))
    print(MetricsR::sccfa(build))
    print(MetricsR::sccpa(build))
    print(MetricsR::scfa(build))
    print(MetricsR::scpa(build))
    print(MetricsR::tlfb(build))
    print(MetricsR::tlpb(build))
    print(MetricsR::weightedRate(build))
  }
}
