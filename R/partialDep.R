#' partialDep
#'
#' @param dat dat
#' @param vars vars
#' @param B B
#' @param n.core n.core
#' @param nrounds nrounds
#' @param eta eta
#' @param max_depth max_depth
#' @param min_child_weight min_child_weight
#' @param gamma gamma
#' @param lambda lambda
#' @param alpha alpha
#' @param subsample subsample
#' @param colsample_bytree colsample_bytree
#' @param seed seed
#' @param type type
#'
#' @import doParallel
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom stats reorder
#' @importFrom foreach %dopar%
#'
#' @export
#'
partialDep <-
  function(
    dat,
    vars,
    B = 100,
    n.core = 4,
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
    type = "PSOCK"
  ) {
    if (B == 1) {
      return.mod <- TRUE
    } else {
      return.mod <- FALSE
    }

    var <- NULL

    if (B == 1) {
      pred <- runXGB(
        dat,
        vars,
        return.mod = TRUE,
        simulate = FALSE,
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
      )
    } else {
    #   cl <- parallel::makeCluster(n.core, type = type)
    #   doParallel::registerDoParallel(cl)
    #
    #   pred <- foreach::foreach(
    #     i = 1:B,
    #     .inorder = FALSE,
    #     .packages = "xgboost",
    #     .export = "runXGB"
    #   ) %dopar%
    #     runXGB(
    #       dat,
    #       vars,
    #       return.mod = FALSE,
    #       simulate = TRUE,
    #       nrounds = nrounds,
    #       eta = eta,
    #       max_depth = max_depth,
    #       min_child_weight = min_child_weight,
    #       gamma = gamma,
    #       lambda = lambda,
    #       alpha = alpha,
    #       subsample = subsample,
    #       colsample_bytree = colsample_bytree,
    #       seed = seed,
    #       n.core = 1
    #     )
    #
    #   parallel::stopCluster(cl)
    # }

      pred <- foreach::foreach(
        i = 1:B,
        .inorder = FALSE,
        .packages = "xgboost",
        .export = "runXGB"
      ) %do%
        runXGB(
          dat,
          vars,
          return.mod = FALSE,
          simulate = TRUE,
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
        )
      }

    # partial dependence plots

    if (B == 1) {
      pd <- pred$pd
      ri <- pred$ri
      mod <- pred$model
    } else {
      pd <- purrr::map(pred, "pd") %>%
        dplyr::bind_rows()

      ## relative influence
      ri <- purrr::map(pred, "ri") %>%
        dplyr::bind_rows()

      mod <- pred[[1]]$model
    }

    # if either character/numeric not in the output df, add dummy col
    if (!"factor" %in% names(pd)) {
      pd$factor <- rep(list(data.frame()), nrow(pd))
    }

    if (!"numeric" %in% names(pd)) {
      pd$numeric <- rep(list(data.frame()), nrow(pd))
    }

    pd <- pd %>% rename("Feature" = var)

    # Calculate 95% CI for different vars
    resCI <-
      dplyr::group_by(pd, .data$Feature) %>%
      dplyr::reframe(
        numeric = list(dplyr::bind_rows(numeric)),
        factor = list(dplyr::bind_rows(factor))
      ) %>%
      dplyr::mutate(
        numeric = purrr::map(
          numeric,
          purrr::possibly(
            ~ dplyr::mutate(
              .x,
              x_bin = cut(.data$x, 100, include.lowest = TRUE)
            ) %>%
              dplyr::group_by(x_bin) %>%
              dplyr::summarise(
                x = mean(x),
                mean = mean(y),
                lower = quantile(y, probs = 0.025),
                upper = quantile(y, probs = 0.975)
              ) %>%
              dplyr::ungroup()
          )
        ),
        factor = purrr::map(
          factor,
          purrr::possibly(
            ~ dplyr::group_by(.x, x) %>%
              dplyr::summarise(
                mean = mean(y),
                lower = quantile(y, probs = 0.025),
                upper = quantile(y, probs = 0.975)
              ) %>%
              dplyr::ungroup()
          )
        )
      )

    resRI <- dplyr::group_by(ri, .data$Feature) %>%
      dplyr::summarise(
        mean = mean(.data$Importance),
        lower = stats::quantile(.data$Importance, probs = c(0.025)),
        upper = stats::quantile(.data$Importance, probs = c(0.975))
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Feature = stats::reorder(.data$Feature, mean)) %>%
      dplyr::arrange(dplyr::desc(.data$Feature))

    if (return.mod) {
      return(list(resCI, resRI, mod))
    } else {
      return(list(resCI, resRI))
    }
  }
