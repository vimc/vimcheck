#' Plotting theme for vimcheck
#'
#' @description
#' A simple plotting theme building on [ggplot2::theme_bw()].
#'
#' @name plotting_theme
#' @rdname plotting_theme
#'
#' @param x_text_angle The angle for X-axis labels. Defaults to 45 degrees.
#'
#' @param y_text_angle The angle for Y-axis labels. Defaults to 0 degrees.
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Other arguments passed to
#' [ggplot2::theme()]. These will be applied in addition to, or in place of,
#' pre-existing elements defined by this theme. See the examples for this
#' theme's appearance.
#'
#' @return A `ggplot2` theme that can be added to `ggplot2` plots or objects.
#'
#' @keywords plotting
#'
#' @examples
#' # using an inbuilt dataset
#' data(mtcars)
#'
#' # standard theme
#' ggplot2::ggplot(mtcars, ggplot2::aes(disp, mpg)) +
#'   ggplot2::geom_point() +
#'   theme_vimc()
#'
#' # with X-axis suppression
#' ggplot2::ggplot(mtcars, ggplot2::aes(disp, mpg)) +
#'   ggplot2::geom_point() +
#'   theme_vimc_noxaxis()
#'
#' @export
theme_vimc <- function(x_text_angle = 45, y_text_angle = 0, ...) {
  ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        size = 10,
        angle = x_text_angle
      ),
      strip.text.y = ggplot2::element_text(
        angle = y_text_angle
      ),
      plot.margin = ggplot2::margin(1, 0, 1, 0, "cm"),
      ...
    )
}

#' @name plotting_theme
#'
#' @importFrom ggplot2 '%+replace%'
#'
#' @export
theme_vimc_noxaxis <- function() {
  theme_vimc() %+replace%
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    )
}
