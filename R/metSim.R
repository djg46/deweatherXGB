#' metSim
#'
#' @param dw_model mod
#' @param newdata data
#' @param metVars metVars (excl. trend)
#' @param n.core n.core
#' @param B B
#' @param type type multisession = SOCK, multicore = FORK
#'
#' @importFrom xgboost getinfo
#' @importFrom rlang :=
#' @importFrom future plan
#' @importFrom furrr future_map_dfr furrr_options
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
    type = "multisession"
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

    . <- NULL

    plan(strategy = type, workers = n.core)

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

    prediction <- 1:B %>% future_map_dfr(.,
                                         ~ doPred(mydata = newdata,
                                                  mod = mod,
                                                  metVars = metVars),
                                         .options = furrr_options(seed = TRUE))

    # use pollutant name
    names(prediction)[2] <- pollutant

    ## Aggregate results
    prediction <- dplyr::group_by(prediction, .data$date) %>%
      dplyr::summarise({{ pollutant }} := mean(.data[[pollutant]]))

    plan("sequential")

    return(dplyr::tibble(prediction))
  }
