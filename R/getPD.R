#' getPD
#'
#' @param x x
#' @param mod mod
#' @param predictor predictor
#' @param var var
#' @param var_type var_type
#'
#' @export
#'
getPD <- function(predictor = predictor,
                  var = var,
                  var_type = var_type,
                  mod = mod,
                  x = x){

  temp <- x
  if(var_type == "numeric"){

    temp[, var] <- predictor

  } else {

    temp[, var] <- factor(predictor, levels = levels(temp[[var]]))
  }

  pred <- mean(predict(mod, temp))

  return(pred)
}

