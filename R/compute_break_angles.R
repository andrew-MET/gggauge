compute_break_angles <- function(gauge_level, breaks, limits, num_breaks) {

  if (is.null(gauge_level)) {
    if (is.null(breaks)) {
      min_data <- limits[1]
      if (is.na(min_data)) min_data <- 0
      max_data <- limits[2]
      if (is.na(max_data)) max_data <- 1
      gauge_level <- 0
    } else {
      gauge_level <- breaks
    }
  }

  if (is.null(breaks)) {
    if (is.na(limits[1])) {
      min_limit <- ifelse(length(gauge_level) == 1 && gauge_level == 0, 0, min(gauge_level))
    } else {
      min_limit <- limits[1]
    }
    if (is.na(limits[2])) {
      max_limit <- ifelse(length(gauge_level) == 1 && gauge_level == 0, 1, max(gauge_level))
    } else {
      max_limit <- limits[2]
    }
    breaks <- pretty(c(min_limit, max_limit), n = num_breaks)
  }

  if (!is.na(limits[1]) && limits[1] < min(breaks)) breaks <- c(limits[1], breaks)
  if (!is.na(limits[2]) && limits[2] > max(breaks)) breaks <- c(breaks, limits[2])

  list(
    breaks       = breaks,
    break_angles = pi * ((max(breaks) - breaks) / (max(breaks) - min(breaks)))
  )
}
