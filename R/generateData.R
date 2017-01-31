
#' Generate a data.frame with metrics computed from dataset
#' as columns and with additional column with current build id
#' columns are:
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
    tlpb=numeric(x)
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
