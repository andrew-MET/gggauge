#' Title
#'
#' @param gauge_level The value for the gauge to point to. If more than one
#'   value is given, one gauge will be plotted for each value.
#' @param type The type of gauge to plot. 'needle' plots a gauge with a needle
#'   pointing the value. 'level' plots a gauge that is filled to the value.
#' @param animate Logical. If more than one value is given in
#'   \code{gauge_value}, and \code{animate = TRUE}, an animated gif of the gauge
#'   will be returned. Requires the R package '\code{gganimate}' and the
#'   'gifski' library.
#' @param anim_fps The number of frames per second for the animation.
#' @param gauge_names The names of the gauges. If set to NULL each gauge is
#'   given a number starting from 1 to the length of \code{gauge_level}.
#' @param gauge_width The width of the gauge as a fraction of the radius of the
#'   gauge.
#' @param gauge_colour The outline colour for the gauge.
#' @param gauge_fill The fill colour for the gauge.
#' @param needle_colour The colour of the needle of the gauge if \code{type =
#'   "needle"}.
#' @param needle_halfwidth The needle is an isosceles triangle so this is the
#'   width of half of that triangle. Note that the radius of the whole gauge is
#'   equal to 1.
#' @param level_fill The fill colour when \code{type = "level"}. If more than
#'   one level is given in \code{gauge_level} and there are fewer colours
#'   specified in \code{level_fill}, colours will be recycled.
#' @param show_gauge_name Logical. Whether to show the name of the gauge aboove
#'   each gauge.
#' @param gauge_name_size The font size for gauge names.
#' @param gauge_name_colour The font colour for gauge names.
#' @param gauge_name_family The font family for gauge names.
#' @param gauge_name_face The font face for gauge names.
#' @param gauge_name_spacer When the \code{gauge_name_size} becomes large, the
#'   bottom of the text may be cut off. To prevent this from happening you can
#'   increase the value here to get a larger margin.
#' @param plot_ticks Logical. Whether to plot tick marks.
#' @param tick_length The length of the tick marks.
#' @param tick_colour The colour of the tick marks.
#' @param plot_tick_labels Logical. Whether to plot tick labels.
#' @param tick_label_colour The colour of the tick labels.
#' @param tick_label_size The size of the tick labels.
#' @param ticks_fill For \code{type = "needle"}, the fill colour of the gauge
#'   can be changed between each tick mark. If there is one fewer colours than
#'   the number of breaks, those colours will be used, otherwise colours will be
#'   interpolated so that there are one fewer colours than the number of breaks.
#' @param rm_obscured_labels Logical. Labels at the start and end of the gauge
#'   are obscured by the gauge outline. Set to TRUE (the default) to not show
#'   these labels.
#' @param limits The limits of the gauge. Should be a numeric vector of length
#'   2. Set to NA to use the data.
#' @param num_breaks The number of breaks in the gauge. A set of breaks will be
#'   determined using either the values set in \code{limits} or from the data
#'   using the \link[base]{pretty}. Ignored if \code{breaks} is not NULL.
#' @param breaks Explicit breaks. If \code{limits} are outside of breaks, the
#'   values for \code{limits} will be added as the first and last breaks.
#' @param ... Not used.
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
  level_fill         = "#008080",
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

  if (gauge_type == "level") {
    plot_data <- compute_levels(gauge_level, gauge_angle, radius, gauge_names, gauge_width)
  }

  gg <- ggplot2::ggplot(plot_data, ggplot2::aes(.data[["x"]], .data[["y"]]), ...)

  if (!is.null(ticks_fill)) {
    if (gauge_type == "needle") {
      for (i in seq_along(ticks_fill_df)) {
        gg <- gg + ggplot2::geom_polygon(data = ticks_fill_df[[i]], fill = ticks_fill[i])
      }
    } else {
      warning("'ticks_fill' is ignored with `gauge_type = 'level'`.")
    }
  }

  gg <- gg + ggplot2::geom_polygon(data = gauge_outline, colour = gauge_colour, fill = gauge_fill) +
    ggplot2::coord_equal() +
    ggplot2::theme_void()

  if (gauge_type == "level") {
    gg <- gg +
      ggplot2::geom_polygon(ggplot2::aes(group = .data[["polygon"]], fill = .data[["polygon"]])) +
      ggplot2::geom_path(ggplot2::aes(group = .data[["polygon"]]), colour = gauge_colour)
    if (length(level_fill) == 1) {
      level_fill <- rep(level_fill, length(gauge_level))
    }
    if (length(level_fill) < length(gauge_level)) {
      warning("Fewer colours supplied in 'level_fill' than the length of 'gauge_level'. Recycling colours.")
      num_cycles <- ceiling(length(gauge_level) / length(level_fill))
      level_fill <- rep(level_fill, num_cycles)
    }
    level_fill <- level_fill[1:length(gauge_level)]
    gg <- gg + ggplot2::scale_fill_manual(values = level_fill)
  }

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

