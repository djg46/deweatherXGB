#' metSim
#'
#' @param dw_model mod
#' @param newdata data
#' @param metVars metVars (excl. trend)
#' @param n.core n.core
#' @param B B
#' @param type type
#'
#' @importFrom xgboost getinfo
#' @importFrom rlang :=
#' @importFrom foreach %dopar%
#'
#' @export
#'
metSim <-
  function(
    dw_model,
    newdata,
    metVars = c("ws", "wd", "air_temp"),
    n.core = 4,
    B = 200,
    type = "PSOCK"
  ) {
    if (!inherits(dw_model, "deweather")) {
      cli::cli_abort(
        c(
          "x" = "Provided {.field dw_model} is of class {.class {class(dw_model)}}.",
          "i" = "Please supply a {.pkg deweather} model from {.fun buildMod}."
        ),
        call = NULL
      )
    }

    ## extract the model
    mod <- dw_model$model

    # pollutant name
    pollutant <- dw_model$response

    if (!"trend" %in% getinfo(mod, "feature_name")) {
      stop(
        "The model must have a trend component as one of the explanatory variables."
      )
    }

    if (missing(newdata)) {
      ## should already have variables
      newdata <- dw_model$data
    } else {
      ## add variables needed
      newdata <- prepData(newdata)
    }

    cl <- parallel::makeCluster(n.core, type = type)

    doParallel::registerDoParallel(cl)

    prediction <- foreach::foreach(
      i = 1:B,
      .inorder = FALSE,
      .combine = "rbind",
      .packages = "gbm",
      .export = "doPred"
    ) %dopar%
      doPred(newdata, mod, metVars)

    parallel::stopCluster(cl)

    # use pollutant name
    names(prediction)[2] <- pollutant

    ## Aggregate results
    prediction <- dplyr::group_by(prediction, .data$date) %>%
      dplyr::summarise({{ pollutant }} := mean(.data[[pollutant]]))

    return(dplyr::tibble(prediction))
  }


## randomly sample from original data

