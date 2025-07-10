#' RunXGB
#'
#' @param dat dataframe
#' @param vars variables
#' @param return.mod return model?
#' @param simulate random sample?
#' @param nrounds iterations
#' @param eta eta
#' @param max_depth max_depth
#' @param min_child_weight min_child_weight
#' @param gamma gamma
#' @param lambda lambda
#' @param alpha alpha
#' @param subsample subsample
#' @param colsample_bytree colsample_bytree
#' @param seed seed
#' @param n.core n.core
#'
#' @importFrom xgboost xgboost xgb.importance xgb.plot.importance
#'
#' @export
#'
runXGB <-
  function(
    dat,
    vars,
    return.mod,
    simulate,
    nrounds = nrounds,
    eta = eta,
    max_depth = max_depth,
    min_child_weight = min_child_weight,
    gamma = gamma,
    lambda = lambda,
    alpha = alpha,
    subsample = subsample,
    colsample_bytree = colsample_bytree,
    seed = seed,
    n.core = n.core
  ) {

    # these models for AQ data are not very sensitive to tree sizes > 1000
    # make reproducible
    if (!simulate) {
      set.seed(seed)
    } else {
      set.seed(stats::runif(1))
    }

    ## sub-sample the data for bootstrapping
    if (simulate) {
      dat <- dat[sample(nrow(dat), nrow(dat), replace = TRUE), ]
    }

    x <- dat[, 2:(length(dat)-1)]

    y <- dat[[ncol(dat)]]

    mod <- xgboost(x = x,
                   y = y,
                   objective = "reg:squarederror",
                   learning_rate = eta, #eta
                   max_depth = max_depth,
                   min_child_weight = min_child_weight,
                   min_split_loss = gamma, #gamma
                   reg_lambda = lambda, #lambda
                   reg_alpha = alpha, #alpha
                   subsample = subsample,
                   colsample_bytree = colsample_bytree,
                   nthreads = 1,
                   nrounds = nrounds)

    ## extract partial dependence components
    pd <- purrr::map(vars, extractPD, mod = mod, x = x, n.core = n.core) %>%
      purrr::map(
        ~ dplyr::nest_by(.x, var, var_type) %>%
          tidyr::pivot_wider(
            names_from = "var_type",
            values_from = "data"
          )
      ) %>%
      dplyr::bind_rows()

    ## relative influence
    ri <- xgb.plot.importance(xgb.importance(mod), plot = FALSE)
    ri <- ri[, -c(2:4)]
    ri[, 2] <- ri[, 2] * 100
    ri$Feature <- reorder(ri$Feature, ri$Importance)

    if (return.mod) {
      result <- list("pd" = pd, "ri" = ri, "model" = mod)

      return(result)
    } else {
      return(list("pd" = pd, "ri" = ri))
    }
  }
