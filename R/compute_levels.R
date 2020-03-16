compute_levels <- function(gauge_level, gauge_angle, radius, gauge_names, gauge_width) {

  level_polygon <- function(angle, radius, width) {
    level_x <- c(
      radius * cos(seq(pi, angle, length.out = 1 + round(1000 * (pi - angle) / pi))),
      (radius - width) * cos(seq(angle, pi, length.out = 1 + round(1000 * (pi - angle) / pi)))
    )
    level_y <- c(
      radius * sin(seq(pi, angle, length.out = 1 + round(1000 * (pi - angle) / pi))),
      (radius - width) * sin(seq(angle, pi, length.out = 1 + round(1000 * (pi - angle) / pi)))
    )
    list(level_x, level_y)
  }

  level_polygons <- lapply(gauge_angle, level_polygon, radius, gauge_width)

  level_polygons <- lapply(
    seq_along(level_polygons),
    function(x) data.frame(
      polygon = x,
      value = gauge_level[x],
      x = level_polygons[[x]][[1]],
      y = level_polygons[[x]][[2]]
    )
  )

  if (!is.null(gauge_names)) {
    if (length(gauge_names) != length(gauge_level)) {
      warning("'gauge_names' must be the same length as 'gauge_level' to be used.")
      gauge_names <- NULL
    } else {
      level_polygons <- mapply(
        function(x, y) {x[["polygon"]] <- y; x},
        level_polygons,
        gauge_names,
        SIMPLIFY = FALSE
      )
    }
  }

  level_polygons <- Reduce(rbind, level_polygons)

  if (!is.null(gauge_names)) {
    level_polygons[["polygon"]] <- factor(level_polygons[["polygon"]], levels = gauge_names)
  } else {
    level_polygons[["polygon"]] <- factor(level_polygons[["polygon"]])
  }

  level_polygons

}
