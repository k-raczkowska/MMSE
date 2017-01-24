mydb = RMySQL::dbConnect(RMySQL::MySQL(), user = 'root', password = 'master', dbname = 'travistorrent', host = 'localhost')
queryResult = DBI::dbGetQuery(mydb, "select tr_build_id, tr_status, author_mail, git_commit, gh_project_name from travistorrent_27_10_2016 order by tr_build_id")
save(queryResult, file = "queryRes.rda")

load(file = 'queryRes.rda')

lcpa <- function(currentBuild){
  projectName <- head(queryResult[queryResult$tr_build_id == currentBuild,]$gh_project_name,1)
  projectBuilds <- queryResult[queryResult$gh_project_name == projectName,]
  data <- projectBuilds[projectBuilds$tr_build_id < currentBuild,]
  c = nrow(data)
  print(c)
  count = 0
  max = 0
  for(i in 1:c){
    print(data[i,2])
    if(data[i,2] == 'passed'){
      count = count + 1
    }
    else
      count = 0
    if(count > max){
      max = count
    }
  }
  print(max)
  return(max)
}

lcfa <- function(currentBuild){
  projectName <- head(queryResult[queryResult$tr_build_id == currentBuild,]$gh_project_name,1)
  projectBuilds <- queryResult[queryResult$gh_project_name == projectName,]
  data <- projectBuilds[projectBuilds$tr_build_id < currentBuild,]
  c = nrow(data)
  print(c)
  count = 0
  max = 0
  for(i in 1:c){
    print(data[i,2])
    if(data[i,2] == 'failed' || data[i,2] == 'errored'){
      count = count + 1
    }
    else
      count = 0
    if(count > max){
      max = count
    }
  }
  print(max)
  return(max)
}

scpa <- function(currentBuild){
  projectName <- head(queryResult[queryResult$tr_build_id == currentBuild,]$gh_project_name,1)
  projectBuilds <- queryResult[queryResult$gh_project_name == projectName,]
  data <- projectBuilds[projectBuilds$tr_build_id < currentBuild,]
  c = nrow(data)
  count = 0
  min = c+1
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

  if(min == c+1){
    min = count
  }
  return(min)
}

scfa <- function(currentBuild){
  projectName <- head(queryResult[queryResult$tr_build_id == currentBuild,]$gh_project_name,1)
  projectBuilds <- queryResult[queryResult$gh_project_name == projectName,]
  data <- projectBuilds[projectBuilds$tr_build_id < currentBuild,]
  c = nrow(data)
  count = 0
  min = c+1
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

  if(min == c+1){
    min = count
  }
  return(min)
}

lccfa <- function(currentBuild){
  projectName <- head(queryResult[queryResult$tr_build_id == currentBuild,]$gh_project_name,1)
  committer <- head(queryResult[queryResult$tr_build_id == currentBuild,]$author_mail,1)
  projectBuilds <- queryResult[queryResult$gh_project_name == projectName,]
  committerBuilds <- queryResult[queryResult$author_mail == committer,]
  data <- committerBuilds[committerBuilds$tr_build_id < currentBuild,]
  c = nrow(data)
  #print(c)
  count = 0
  max = 0
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
  #print(max)
  return(max)
}

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

tlb <- function(currentBuild){
  projectName <- head(queryResult[queryResult$tr_build_id == currentBuild,]$gh_project_name,1)
  projectBuilds <- queryResult[queryResult$gh_project_name == projectName,]
  passed <- projectBuilds[projectBuilds$tr_status == 'passed',]
  data <- passed[passed$tr_build_id < currentBuild,]
  x = difftime(Sys.Date(), strptime(data, format = "%Y-%m-%d"), units = "minutes")
  y = as.numeric(x, units="minutes")
  return(y)
}

lcpa(1763098)
