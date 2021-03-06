# These functions summarise hourly data (v) to give daily mean, max or min
# They by default allows missing values of up to 75% (threshold = 0.75)

# ignore for now
# dailySumm <- function (v, FUN, threshold=0.75) {
#   if (!is.function(FUN)) FUN <- eval(parse(text=FUN))
#   if (mean(is.na(v)) < threshold) {
#     return(FUN(v, na.rm = T))
#   } else {
#     return(NA)
#   }
# }

dailyMean <- function (v, threshold=0.75) {
  if (mean(is.na(v)) < threshold) {
    return(mean(v, na.rm = T))
  } else {
    return(NA)
  }
}

dailyMin <- function (v, threshold=0.75) {
  if (mean(is.na(v)) < threshold) {
    return(min(v, na.rm = T))
  } else {
    return(NA)
  }
}

dailyMax <- function (v, threshold=0.75) {
  if (mean(is.na(v)) < threshold) {
    return(max(v, na.rm = T))
  } else {
    return(NA)
  }
}
