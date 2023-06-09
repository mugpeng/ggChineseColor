#' Different types of Chinese Color palettes grouped by basic colors.
#' 
#' Mimic from ggsci(https://github.com/nanxstats/ggsci) 
#'
#' @param palette Palette type.
#' There are 9 available options:
#' \itemize{
#' \item \code{"blue"}
#' \item \code{"black"}
#' \item \code{"cyan"}
#' \item \code{"gray"}
#' \item \code{"green"}
#' \item \code{"purple"}
#' \item \code{"red"}
#' \item \code{"white"}
#' \item \code{"yellow"}
#' @param n Number of individual colors to be generated.
#' @param alpha Transparency level, a real number in (0, 1].
#' See \code{alpha} in \code{\link[grDevices]{rgb}} for details.
#' @param reverse Logical. Should the order of the colors be reversed?
#'
#' @export pal_ChineseColor
#'
#' @importFrom grDevices colorRamp rgb
#' @importFrom prismatic color
#'
#' @author Yu Zhong Peng <\email{mugpeng@@foxmail.com}>
#'
#' @references
#' \url{https://colors.masantu.com/#/}
#'
#' @examples
#' pal_ChineseColor("cyan")
#' pal_ChineseColor("green", n = 30, alpha = 0.6, reverse = TRUE)
pal_ChineseColor <- function(
    palette = c(
      "blue", "black", "cyan", "gray", "green", "purple", "red", "white", "yellow"
      ), n = 3, alpha = 1, reverse = FALSE) {
  palette <- match.arg(palette)
  
  if (alpha > 1L | alpha <= 0L) stop("alpha must be in (0, 1]")
  
  raw_cols <- color_list[[palette]]
  func_cols <- colorRamp(raw_cols, space = "Lab", interpolate = "spline")
  mat_cols <- func_cols(seq(0L, 1L, length.out = n))
  alpha_cols <- rgb(
    mat_cols[, 1L], mat_cols[, 2L], mat_cols[, 3L],
    alpha = alpha * 255L, maxColorValue = 255L
  )
  
  if (reverse) alpha_cols <- rev(alpha_cols)
  
  return(prismatic::color(alpha_cols))
}
