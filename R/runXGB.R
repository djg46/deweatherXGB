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
#' @importFrom data.table rbindlist
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

    i <- var <- NULL

    # these models for AQ data are not very sensitive to tree sizes > 1000
    # make reproducible
    if (!simulate) {
      set.seed(seed)
    } else {
      set.seed(stats::runif(1, 0, 10000))
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

    grid <- foreach(i = 1:length(vars)) %do% {

      if(is.numeric(x[[vars[i]]])){

        if(vars[i] %in% c("hour_sin", "hour_cos")){
          resolution <- 25
        } else if(vars[i] == "week"){
          resolution <- max(x[[vars[i]]] + 1)
        } else if(vars[i] %in% c("wd_sin", "wd_cos")) {
          resolution <- 37
        } else if(vars[i] %in% c("air_temp", "ws")){
          resolution <- 50
        } else {
          resolution <- 100
        }

        ret <- as.data.frame(seq(from = min(x[[vars[i]]], na.rm = TRUE),
                                 to = max(x[[vars[i]]], na.rm = TRUE),
                                 length = resolution))

      } else {

        ret <- as.data.frame(levels(x[[vars[i]]]))
      }

      names(ret) <- "x"

      ret$var <- vars[i]

      ret$var_type <- ifelse(is.numeric(x[[vars[i]]]), "numeric", "factor")

      return(ret)

    }

    grid <- rbindlist(grid)

    grid$y <- 1

    cl <- makeCluster(n.core, type = type)

    registerDoParallel(cl)

    out <- foreach(i = 1:nrow(grid)) %dopar% {

      temp <- x

      if(grid$var_type[i] == "numeric"){

        temp[, grid$var[i]] <- grid$x[i]

      } else {

        temp[, grid$var[i]] <- factor(grid$x[i], levels = levels(temp[[grid$var[i]]]))
      }

      out <- mean(predict(mod, temp))

      return(out)
    }

    stopCluster(cl)

    for(j in 1:length(out)){

      grid$y[j] <- out[[j]]

    }

    pd <- list()

    for(k in 1:length(vars)){

      pd[[k]] <- grid %>% filter(var == vars[k])

      if(is.numeric(x[[vars[k]]])){

        pd[[k]]$x <- as.numeric(pd[[k]]$x)

      }

    }

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
