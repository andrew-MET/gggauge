compute_ticks_fill <- function(breaks_angle, ticks_fill, radius, gauge_width) {

  num_breaks <- length(breaks_angle)
  num_fills  <- length(ticks_fill)

  if (num_breaks - 1 != num_fills) {
    warning("Number of colours in 'ticks_fill' is not 1 fewer than the number of breaks. Interpolating colours.")
    num_fills <- num_breaks - 1
  }

  fill_colours <- colorRampPalette(ticks_fill)(num_fills)

  breaks_polygon <- function(angles, pol, inner_radius, outer_radius) {
    data.frame(
      x = c(
        cos(seq(angles[1], angles[2], length.out = 50)) * outer_radius,
        cos(seq(angles[2], angles[1], length.out = 50)) * inner_radius
      ),
      y = c(
        sin(seq(angles[1], angles[2], length.out = 50)) * outer_radius,
        sin(seq(angles[2], angles[1], length.out = 50)) * inner_radius
      ),
      breaks_pol = pol
    )
  }

  angle_pairs <- lapply(seq(1, length(breaks_angle) - 1), function(x) c(breaks_angle[x], breaks_angle[x + 1]))

  breaks_polygons <- mapply(
    breaks_polygon,
    angle_pairs,
    formatC(seq_along(angle_pairs), width = 3, flag = "0"),
    MoreArgs = list(inner_radius = radius - gauge_width, outer_radius = radius),
    SIMPLIFY = FALSE
  )

  list(
    breaks_polygons = breaks_polygons,
    breaks_fill_col = fill_colours
  )

}
