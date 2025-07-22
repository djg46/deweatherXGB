#' doPred
#'
#' @param mydata data
#' @param mod mod
#' @param metVars metVars
#'
#' @importFrom stats predict
#'
#' @export
#'
doPred <- function(mydata = mydata, mod = mod, metVars = metVars) {
  ## random samples
  n <- nrow(mydata)
  id <- sample(1:n, n, replace = FALSE)

  ## new data with random samples
  mydata[metVars] <- lapply(mydata[metVars], function(x) {
    x[id]
  })

  prediction <- predict(mod, mydata)

  prediction <- data.frame(date = mydata$date, pred = prediction)

  return(prediction)
}
