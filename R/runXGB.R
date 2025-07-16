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
#' @param type type
#'
#' @importFrom xgboost xgboost xgb.importance xgb.plot.importance
#' @importFrom foreach foreach
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
    n.core = n.core,
    type = type
  ) {

    i <- NULL

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

    cl <- makeCluster(n.core, type = type)

    registerDoParallel(cl)

    pd <- foreach(i = 1:length(vars)) %dopar% {

      if(is.numeric(x[[vars[i]]])){

        if(vars[i] %in% c("hour_sin", "hour_cos")){
          resolution <- 24
        } else if(vars[i] == "week"){
          resolution <- max(x[[vars[i]]])
        } else if(vars[i] %in% c("wd_sin", "wd_cos")) {
          resolution <- 36
        } else if(vars[i] %in% c("air_temp", "ws")){
          resolution <- 50
        } else {
          resolution <- 100
        }

        grid <- seq(from = min(x[[vars[i]]], na.rm = TRUE),
                    to = max(x[[vars[i]]], na.rm = TRUE),
                    length = resolution)

      } else {

        grid <- levels(x[[vars[i]]])
      }

      grid <- expand.grid(grid)

      names(grid) <- vars[i]

      grid$pred <- 1

      for(j in 1:nrow(grid)){

        temp <- x

        temp[, vars[i]] <- grid[j, 1]

        grid$pred[j] <- mean(predict(mod, temp))
      }

      res <- data.frame(y = grid$pred,
                        var = vars[i],
                        x = grid[[vars[i]]],
                        var_type = ifelse(is.numeric(x[[vars[i]]]), "numeric", "factor")
      )

      return(res)
    }

    stopCluster(cl)

    pd <- pd %>% purrr::map(
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
