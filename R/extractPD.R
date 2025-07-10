#' extractPD
#'
#' @param vars vars
#' @param mod mod
#' @param x x
#' @param n.core n.core
#'
#' @importFrom parallel makeCluster stopCluster
#' @importFrom pdp partial
#'
#' @export
#'
extractPD <- function(vars, mod, x, n.core){

  if(vars == "hour"){
    n <- 24
  } else{
    n <- 100
  }

  if(is.factor(x[[vars]])){
    pred.grid <- data.frame(x = levels(x[[vars]]))
    names(pred.grid) <- vars
  }

  if(n.core != 1){

    cl <- makeCluster(n.core)

    registerDoParallel(cl)

    if(!is.factor(x[[vars]])){

      res <- partial(object = mod,
                     pred.var = vars,
                     train = x,
                     type = "regression",
                     grid.resolution = n,
                     parallel = TRUE)
    } else {

      res <- partial(object = mod,
                     pred.var = vars,
                     train = x,
                     type = "regression",
                     grid.resolution = n,
                     parallel = TRUE,
                     pred.grid = pred.grid)
    }

    stopCluster(cl)
  } else {
    if(!is.factor(x[[vars]])){

      res <- partial(object = mod,
                     pred.var = vars,
                     train = x,
                     type = "regression",
                     grid.resolution = n,
                     parallel = TRUE)
    } else {

      res <- partial(object = mod,
                     pred.var = vars,
                     train = x,
                     type = "regression",
                     grid.resolution = n,
                     parallel = TRUE,
                     pred.grid = pred.grid)
    }
  }

  res <- data.frame(y = res$yhat,
                    var = vars,
                    x = res[[vars]],
                    var_type = ifelse(is.numeric(x[[vars]]), "numeric", "factor")
  )

  return(res)

}
