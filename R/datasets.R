
#' TravisTorrent builds data
#'
#' A dataset containing data relating to builds which ran on Travis-CI
#'
#' @format A data frame with 19396 rows and 10 variables:
#'
#' \describe{
#'   \item{tr_build_id}{id of build started at Travis-CI}
#'   \item{tr_status}{status of completed build}
#'   \item{author_mail}{e-mail address of person whose commit triggered build}
#'   \item{git_commit}{SHA of commit}
#'   \item{gh_project_name}{name of repository}
#'   \item{tr_started_at}{datetime when build has started}
#'   \item{gh_src_churn}{churn of source code on all commits included in build}
#'   \item{gh_files_added}{number of files added by all commits included in build}
#'   \item{gh_files_modified}{number of files modified by all commits included in build}
#'   \item{gh_files_deleted}{number of files deleted by all commits included in build}
#' }
#' @examples
#' head(queryResult)
"queryResult"

#' TravisTorrent builds data
#'
#' A dataset containing data relating to builds which ran on Travis-CI
#'
#' @format A data frame with 19396 rows and 10 variables:
#' \describe{
#'   \item{tr_build_id}{price, in US dollars}
#'   \item{tr_status}{weight of the diamond, in carats}
#'   \item{git_commit}{weight of the diamond, in carats}
#'   \item{gh_project_name}{weight of the diamond, in carats}
#'   \item{tr_started_at}{weight of the diamond, in carats}
#'   \item{gh_src_churn}{weight of the diamond, in carats}
#'   \item{gh_files_added}{weight of the diamond, in carats}
#'   \item{gh_files_modified}{weight of the diamond, in carats}
#'   \item{gh_files_deleted}{weight of the diamond, in carats}
#' }
#' @examples
#' head(queryResult)
#' @source \url{http://www.diamondse.info/}
"commits"
