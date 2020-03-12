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
  gauge_colour       = "grey30",
  gauge_fill         = "transparent",
  show_gauge_name    = TRUE,
  gauge_name_size    = 11,
  gauge_name_colour  = "grey20",
  gauge_name_family  = NULL,
  gauge_name_face    = NULL,
  plot_ticks         = TRUE,
  tick_length        = 0.05,
  tick_colour        = "grey70",
  plot_tick_labels   = TRUE,
  tick_label_colour  = "grey50",
  tick_label_size    = 4,
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
  radius_inner  <- 1 - gauge_width
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

  if (is.null(gauge_level)) {
    if (is.null(breaks)) {
      min_data <- limits[1]
      if (is.na(min_data)) min_data <- 0
      max_data <- limits[2]
      if (is.na(max_data)) max_data <- 1
      gauge_level <- c(min_data, max_data)
    } else {
      gauge_level <- breaks
    }
  }

  if (is.null(breaks)) {
    if (is.na(limits[1])) {
      min_limit <- min(gauge_level)
    } else {
      min_limit <- limits[1]
    }
    if (is.na(limits[2])) {
      max_limit <- max(gauge_level)
    } else {
      max_limit <- limits[2]
    }
    breaks <- pretty(c(min_limit, max_limit), n = num_breaks)
  }
  break_angles <- pi * ((max(breaks) - breaks) / (max(breaks) - min(breaks)))
  break_x      <- c(
    circle_centre[1] + (radius - tick_length) * cos(break_angles),
    circle_centre[1] + radius_inner * cos(break_angles)
  )
  break_y      <- c(
    circle_centre[2] + (radius - tick_length) * sin(break_angles),
    circle_centre[2] + radius_inner * sin(break_angles)
  )
  ticks_df <- data.frame(x = break_x, y = break_y, label = breaks, angle = break_angles, radius = tick_length)

  gauge_outline <- data.frame(x = x_points, y = y_points)

  if (!is.null(gauge_level)) {
    gauge_min    <- min(breaks)
    gauge_max    <- max(breaks)
    gauge_angle  <- pi * ((gauge_max - gauge_level) / (gauge_max - gauge_min))

    if (gauge_type == "needle") {
      needle_points <- function(angle, gauge_radius) {
        needle_x    <- c(gauge_radius * cos(angle), 0.1 * cos(seq(angle + pi / 6, angle - pi / 6, length.out = 50)))
        needle_y    <- c(gauge_radius * sin(angle), 0.1 * sin(seq(angle + pi / 6, angle - pi / 6, length.out = 50)))
        list(needle_x, needle_y)
      }
      needle_polygons <- lapply(gauge_angle, needle_points, radius)
      needle_polygons <- lapply(
        seq_along(needle_polygons),
        function(x) data.frame(polygon = x, value = gauge_level[x], x = needle_polygons[[x]][[1]], y = needle_polygons[[x]][[2]])
      )
      if (!is.null(gauge_names)) {
        if (length(gauge_names) != length(gauge_level)) {
          warning("'gauge_names' must be the same length as 'gauge_level' to be used.")
        } else {
          needle_polygons <- mapply(function(x, y) {x[["polygon"]] <- y; x}, needle_polygons, gauge_names, SIMPLIFY = FALSE)
        }
      }
      needle_polygons <- Reduce(rbind, needle_polygons)
      if (!is.null(gauge_names)) needle_polygons[["polygon"]] <- factor(needle_polygons[["polygon"]], levels = gauge_names)
      needle_circle   <- data.frame(
        x       = 0.09 * cos(seq(0, 2 * pi, length.out = 100)),
        y       = 0.09 * sin(seq(0, 2 * pi, length.out = 100))
      )
      plot_data <- needle_polygons
    }

  }

  gg <- ggplot2::ggplot(plot_data, ggplot2::aes(.data[["x"]], .data[["y"]]), ...) +
    ggplot2::geom_polygon(data = gauge_outline, colour = gauge_colour, fill = gauge_fill) +
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
      ggplot2::geom_polygon(ggplot2::aes(group = .data[["polygon"]]), colour = "#008080", fill = "#008080") +
      ggplot2::geom_polygon(data = needle_circle, colour = "#008080", fill = "#008080")
  }

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
          margin = ggplot2::margin(b = 2)
        )
      )
    } else {
      gg <- gg + ggplot2::theme(strip.text.x = ggplot2::element_text(size = 0))
    }
  }

  gg

}
