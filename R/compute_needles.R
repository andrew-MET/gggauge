compute_needles <- function(gauge_level, gauge_angle, radius, gauge_names, circle_radius) {

  needle_points <- function(angle, gauge_radius) {
    needle_x    <- c(gauge_radius * cos(angle), circle_radius * cos(seq(angle + pi / 6, angle - pi / 6, length.out = 50)))
    needle_y    <- c(gauge_radius * sin(angle), circle_radius * sin(seq(angle + pi / 6, angle - pi / 6, length.out = 50)))
    list(needle_x, needle_y)
  }

  needle_polygons <- lapply(gauge_angle, needle_points, radius)

  needle_polygons <- lapply(
    seq_along(needle_polygons),
    function(x) data.frame(
      polygon = x,
      value = gauge_level[x],
      x = needle_polygons[[x]][[1]],
      y = needle_polygons[[x]][[2]]
    )
  )

  if (!is.null(gauge_names)) {
    if (length(gauge_names) != length(gauge_level)) {
      warning("'gauge_names' must be the same length as 'gauge_level' to be used.")
      gauge_names <- NULL
    } else {
      needle_polygons <- mapply(
        function(x, y) {x[["polygon"]] <- y; x},
        needle_polygons,
        gauge_names,
        SIMPLIFY = FALSE
      )
    }
  }

  needle_polygons <- Reduce(rbind, needle_polygons)

  if (!is.null(gauge_names)) {
    needle_polygons[["polygon"]] <- factor(needle_polygons[["polygon"]], levels = gauge_names)
  } else {
    needle_polygons[["polygon"]] <- factor(needle_polygons[["polygon"]])
  }

  needle_circle   <- data.frame(
    x = circle_radius * 0.9 * cos(seq(0, 2 * pi, length.out = 100)),
    y = circle_radius * 0.9 * sin(seq(0, 2 * pi, length.out = 100))
  )

  list(needles = needle_polygons, needle_circle = needle_circle)

}
