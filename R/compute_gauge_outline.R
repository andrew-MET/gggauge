compute_gauge_outline <- function(gauge_width, circle_centre, radius) {

  radius_inner  <- radius - gauge_width
  angle         <- seq(0, pi, length.out = 1000)

  x_points <- c(
    circle_centre[1] - radius_inner,
    circle_centre[1] - radius,
    circle_centre[1] - radius * cos(angle),
    circle_centre[1] + radius_inner,
    circle_centre[1] + radius_inner * cos(angle)
  )

  y_points <- c(
    0,
    0,
    circle_centre[1] + radius * sin(angle),
    0,
    circle_centre[1] + radius_inner * sin(angle)
  )

  data.frame(x = x_points, y = y_points)

}
