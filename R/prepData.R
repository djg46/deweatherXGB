#' prepData
#'
#' @param mydata data
#' @param add additional vars
#' @param local.tz local timezone
#' @param lag vars to lag
#'
#' @export
#'
prepData <- function(
    mydata,
    add = c(
      "hour",
      "hour.local",
      "weekday",
      "trend",
      "week",
      "jday",
      "month"
    ),
    local.tz = "Europe/London",
    lag = NULL
) {
  ## Some cheack to make sure data are OK.
  # does `date` exist?
  if (!"date" %in% names(mydata)) {
    cli::cli_abort("No mydata${.field date} field supplied.")
  }
  # is `date` a date?
  if (
    inherits(mydata$date, "character") |
    inherits(mydata$date, "factor") |
    inherits(mydata$date, "numeric")
  ) {
    cli::cli_abort(
      c(
        "x" = "mydata{.field $date} is of class {.code {class(mydata$date)}}",
        "i" = "Please ensure mydata{.field $data} is class {.code Date} or {.code POSIXt} (e.g., with {.fun as.POSIXct} or {.pkg lubridate})"
      )
    )
  }

  if ("hour" %in% add) {
    mydata$hour <- lubridate::hour(mydata$date)
  }

  if ("hour.local" %in% add) {
    mydata$hour.local <- lubridate::hour(lubridate::with_tz(
      mydata$date,
      local.tz
    ))
  }

  if ("weekday" %in% add) {
    mydata$weekday <- as.factor(format(mydata$date, "%A"))
  }

  if ("trend" %in% add) {
    mydata$trend <- as.numeric(mydata$date)
  }

  if ("week" %in% add) {
    mydata$week <- as.numeric(format(mydata$date, "%W"))
  }

  if ("jday" %in% add) {
    mydata$jday <- as.numeric(format(mydata$date, "%j"))
  }

  if ("month" %in% add) {
    mydata$month <- as.factor(format(mydata$date, "%b"))
  }

  ## add lagged variables
  if (!is.null(lag)) {
    for (i in seq_along(lag)) {
      mydata[[paste0("lag1", lag[i])]] <- mydata[[lag[i]]][c(
        NA,
        1:(nrow(mydata) - 1)
      )]
    }
  }

  ## NaN spells trouble for gbm for some reason
  mydata[] <- lapply(mydata, function(x) {
    replace(x, which(is.nan(x)), NA)
  })
  mydata
}
