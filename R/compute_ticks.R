compute_ticks <- function(breaks, circle_centre, radius, gauge_width, tick_length) {

  radius_inner <- radius - gauge_width

  break_x <- c(
    circle_centre[1] + (radius - tick_length) * cos(breaks[["break_angles"]]),
    circle_centre[1] + radius_inner * cos(breaks[["break_angles"]])
  )
  break_y <- c(
    circle_centre[2] + (radius - tick_length) * sin(breaks[["break_angles"]]),
    circle_centre[2] + radius_inner * sin(breaks[["break_angles"]])
  )

  data.frame(x = break_x, y = break_y, label = breaks[["breaks"]], angle = breaks[["break_angles"]], radius = tick_length)

}
