#' buildMod
#'
#' @param input_data input_data
#' @param vars vars
#' @param pollutant pollutant
#' @param sam.size sam.size
#' @param simulate simulate
#' @param B B
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
#' @param n.core n.core
#' @param type type multisession = SOCK, multicore = FORK
#'
#' @importFrom tidyr all_of
#' @importFrom future plan
#'
#' @export
#'
buildMod <- function(
    input_data,
    vars = c(
      "trend",
      "ws",
      "wd",
      "hour",
      "weekday",
      "air_temp"
    ),
    pollutant = "NO2",
    sam.size = nrow(input_data),
    simulate = FALSE,
    B = 100,
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
    type = "multisession"
) {

  plan(strategy = type, workers = n.core)

  ## add other variables, select only those required for modelling
  input_data <- prepData(input_data)
  input_data <-
    dplyr::select(input_data, all_of(c("date", vars, pollutant)))
  input_data <-
    stats::na.omit(input_data) # only build model where all data are available - can always predict in gaps

  # randomly sample data according to sam.size
  if (sam.size > nrow(input_data)) {
    sam.size <- nrow(input_data)
  }

  if (simulate) {
    id <- sample(nrow(input_data), size = sam.size, replace = TRUE)
    input_data <- input_data[id, ]
  } else {
    id <- sample(nrow(input_data), size = sam.size)
    input_data <- input_data[id, ]
  }

  if("month" %in% vars){
    input_data$month <- factor(input_data$month, levels = month.abb)
  }

  if("weekday" %in% vars){
    input_data$weekday <- factor(input_data$weekday, levels = DescTools::day.name)
  }

  ## if more than one simulation only return model ONCE
  if (B != 1L) {
    mod <- runXGB(
      input_data,
      vars,
      return.mod = TRUE,
      simulate = simulate,
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

  # if model needs to be run multiple times
  res <- partialDep(
    input_data,
    vars,
    B,
    n.core,
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
    type = type
  )

  if (B != 1) {
    Mod <- mod$model
  } else {
    Mod <- res[[3]]
  }

  # return a list of model, data, partial deps
  result <-
    list(
      model = Mod,
      influence = res[[2]],
      data = input_data,
      pd = res[[1]],
      response = pollutant
    )
  class(result) <- "deweather"

  plan("sequential")

  return(result)
}
