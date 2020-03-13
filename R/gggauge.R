#' Title
#'
#'
#' @param gauge_level
#' @param type
#' @param animate
#' @param anim_fps
#' @param gauge_names
#' @param gauge_width
#' @param gauge_colour
#' @param gauge_fill
#' @param show_gauge_name
#' @param gauge_name_size
#' @param gauge_name_colour
#' @param gauge_name_family
#' @param gauge_name_face
#' @param plot_ticks
#' @param tick_length
#' @param tick_colour
#' @param plot_tick_labels
#' @param tick_label_colour
#' @param tick_label_size
#' @param rm_obscured_labels
#' @param limits
#' @param num_breaks
#' @param breaks
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
gggauge <- function(
  gauge_level        = NULL,
  type               = c("needle", "level"),
  animate            = FALSE,
  anim_fps           = 10,
  gauge_names        = NULL,
  gauge_width        = 1 / 3,
  gauge_colour       = "grey70",
  gauge_fill         = "transparent",
  needle_colour      = "grey50",
  needle_halfwidth   = 0.1,
  show_gauge_name    = TRUE,
  gauge_name_size    = 11,
  gauge_name_colour  = "grey20",
  gauge_name_family  = NULL,
  gauge_name_face    = NULL,
  gauge_name_spacer  = 2,
  plot_ticks         = TRUE,
  tick_length        = 0.05,
  tick_colour        = "grey70",
  plot_tick_labels   = TRUE,
  tick_label_colour  = "grey50",
  tick_label_size    = 4,
  ticks_fill         = NULL,
  rm_obscured_labels = TRUE,
  limits             = c(NA, NA),
  num_breaks         = 11,
  breaks             = NULL,
  ...
) {

  if (!is.null(gauge_level)) {
    stopifnot(is.numeric(gauge_level))
  }
  stopifnot(gauge_width < 1)
  stopifnot(length(limits) == 2)

  gauge_type <- match.arg(type)

  circle_centre <- c(0, 0)
  radius        <- 1

  if (is.null(gauge_level)) {
    warning("'gauge_level' not set. Setting to 0.")
    gauge_level <- 0
  }

  gauge_outline <- compute_gauge_outline(gauge_width, circle_centre, radius)

  breaks_data <- compute_break_angles(gauge_level, breaks, limits, num_breaks)

  ticks_df <- compute_ticks(breaks_data, circle_centre, radius, gauge_width, tick_length)

  if (!is.null(ticks_fill)) {
    ticks_fill_data <- compute_ticks_fill(breaks_data[["break_angles"]], ticks_fill, radius, gauge_width)
    ticks_fill_df   <- ticks_fill_data[["breaks_polygons"]]
    ticks_fill      <- ticks_fill_data[["breaks_fill_col"]]
  }

  gauge_min    <- min(breaks_data[["breaks"]])
  gauge_max    <- max(breaks_data[["breaks"]])
  gauge_angle  <- pi * ((gauge_max - gauge_level) / (gauge_max - gauge_min))

  if (gauge_type == "needle") {
    needle_data   <- compute_needles(gauge_level, gauge_angle, radius, gauge_names, needle_halfwidth)
    plot_data     <- needle_data[["needles"]]
    needle_circle <- needle_data[["needle_circle"]]
  }

  gg <- ggplot2::ggplot(plot_data, ggplot2::aes(.data[["x"]], .data[["y"]]), ...)

  if (!is.null(ticks_fill)) {
    for (i in seq_along(ticks_fill_df)) {
      gg <- gg + ggplot2::geom_polygon(data = ticks_fill_df[[i]], fill = ticks_fill[i])
    }
  }

  gg <- gg + ggplot2::geom_polygon(data = gauge_outline, colour = gauge_colour, fill = gauge_fill) +
    ggplot2::coord_equal() +
    ggplot2::theme_void()

  if (plot_ticks) {

    gg <- gg +
      ggplot2::geom_spoke(data = ticks_df, mapping = ggplot2::aes(angle = angle, radius = radius), colour = tick_colour)

    if (plot_tick_labels) {

      ticks_df[["label_angle"]]              <- 0 #ticks_df[["angle"]] * 180 / pi
      ticks_df[["hjust"]]                    <- (pi - ticks_df[["angle"]]) / pi
      ticks_df[["vjust"]]                    <- 1 - 2 * (pi / 2 - ticks_df[["angle"]]) / pi
      left_labels                            <- which(ticks_df[["angle"]] > pi / 2)
      right_labels                           <- which(ticks_df[["angle"]] < pi / 2)
      ticks_df[["label_angle"]][left_labels] <- 0 #ticks_df[["label_angle"]][left_labels] + 180
      ticks_df[["vjust"]][left_labels]       <- rev(ticks_df[["vjust"]][right_labels])

      if (rm_obscured_labels) {
        ticks_df[["label"]][c(1, nrow(ticks_df) / 2)] <- NA_real_
      }

      gg <- gg +
        ggplot2::geom_text(
          data     = ticks_df[1:nrow(ticks_df) / 2,],
          mapping  = ggplot2::aes(label = .data[["label"]], angle = .data[["label_angle"]], hjust = .data[["hjust"]], vjust = 1), #vjust = .data[["vjust"]]),
          fontface = "plain",
          colour   = tick_label_colour,
          size     = tick_label_size
        )

    }

  }

  if (gauge_type == "needle") {
    gg <- gg +
      ggplot2::geom_polygon(ggplot2::aes(group = .data[["polygon"]]), colour = needle_colour, fill = needle_colour) +
      ggplot2::geom_polygon(data = needle_circle, colour = needle_colour, fill = needle_colour)
  }

  gg <- gg + theme(legend.position = "none")

  if (animate) {
    if (!requireNamespace("gganimate", quietly = TRUE)) {
      warning("Package 'gganimate' is required for animated output. Faceting instead.")
      animate <- FALSE
    } else {
      gg <- gg + gganimate::transition_manual(.data[["polygon"]])
      gg <- gganimate::animate(gg, nframes = length(gauge_level), fps = anim_fps)
    }
  }

  if (!animate) {
    gg <- gg + ggplot2::facet_wrap("polygon")
    if (show_gauge_name) {
      gg <- gg + ggplot2::theme(
        strip.text.x = ggplot2::element_text(
          family = gauge_name_family,
          face   = gauge_name_face,
          colour = gauge_name_colour,
          size   = gauge_name_size,
          margin = ggplot2::margin(b = gauge_name_spacer)
        )
      )
    } else {
      gg <- gg + ggplot2::theme(strip.text.x = ggplot2::element_text(size = 0))
    }
  }

  gg

}

######

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

######

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

  list(
    breaks       = breaks,
    break_angles = pi * ((max(breaks) - breaks) / (max(breaks) - min(breaks)))
  )
}

######

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

######

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
  }

  needle_circle   <- data.frame(
    x = circle_radius * 0.9 * cos(seq(0, 2 * pi, length.out = 100)),
    y = circle_radius * 0.9 * sin(seq(0, 2 * pi, length.out = 100))
  )

  list(needles = needle_polygons, needle_circle = needle_circle)

}

######

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
