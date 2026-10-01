#' Draw the ggChinaFlag hex logo
#'
#' Generates the hexagonal sticker logo of the ggChinaFlag package: a cream
#' upper half, a red waving-flag area chart, and five golden stars ascending
#' along the wave crest. The output size follows the hex sticker convention
#' (1.73 x 2.00 inches, pointy top).
#'
#' @return A \code{ggplot} object that can be printed or saved with
#'   \code{ggplot2::ggsave()}.
#' @export
#'
#' @examples
#' p <- plot_ggChinaFlag_logo()
#' print(p)
plot_ggChinaFlag_logo <- function() {

  # 01. Geometric helper functions ---------------------------------------------

  # Regular hexagon with a pointy top and circumradius r
  hex_polygon <- function(r = 1) {
    ang <- seq(90, 390, by = 60) * pi / 180
    data.frame(x = r * cos(ang), y = r * sin(ang))
  }

  # Five-pointed star polygon: 10 vertices, outer/inner radius ratio 1/phi^2,
  # first vertex pointing straight up
  star_polygon <- function(x0, y0, r, rot = 0, id = "star") {
    ratio <- (3 - sqrt(5)) / 2
    ang <- pi / 2 + rot + seq(0, 9) * pi / 5
    rad <- rep(c(r, r * ratio), length.out = 10)
    data.frame(x = x0 + rad * cos(ang), y = y0 + rad * sin(ang), id = id)
  }

  # Cubic Bezier curve; p0..p3 are length-2 vectors
  # (start, two control points, end)
  bezier <- function(p0, p1, p2, p3, n = 200) {
    t <- seq(0, 1, length.out = n)
    data.frame(
      x = (1 - t)^3 * p0[1] + 3 * (1 - t)^2 * t * p1[1] +
        3 * (1 - t) * t^2 * p2[1] + t^3 * p3[1],
      y = (1 - t)^3 * p0[2] + 3 * (1 - t)^2 * t * p1[2] +
        3 * (1 - t) * t^2 * p2[2] + t^3 * p3[2]
    )
  }

  # Convert design coordinates (188 x 214, y down, hexagon center (94, 107),
  # circumradius 100) to plot coordinates centered at the hexagon center,
  # radius 1, y up
  toPlot <- function(x, y) data.frame(x = (x - 94) / 100, y = (107 - y) / 100)


  # 02. Flag wave ---------------------------------------------------------------

  # Two cubic Bezier segments form one wave: low on the left, rising to the
  # right. Adjust the control-point y values to change the waving amplitude.
  seg1 <- bezier(c(0, 112),  c(26, 88),  c(52, 132),  c(94, 106))
  seg2 <- bezier(c(94, 106), c(130, 84), c(158, 110), c(188, 88))
  wave <- rbind(seg1, seg2[-1, ])
  wave <- toPlot(wave$x, wave$y)

  # Hexagon size: r = 0.985 so the border stroke stays fully inside the canvas
  hexR <- 0.985
  wR <- hexR * sqrt(3) / 2   # |x| of the hexagon's vertical edges

  # Part of the crest between the vertical edges
  xs <- seq(-wR, wR, length.out = 300)
  crest <- data.frame(x = xs, y = stats::approx(wave$x, wave$y, xout = xs)$y)

  # Red flag field: walk along the crest, then return to the start following
  # the lower half of the hexagon boundary
  redField <- rbind(
    crest,
    data.frame(x = c(wR, 0, -wR), y = c(-hexR / 2, -hexR, -hexR / 2))
  )


  # 03. Five stars ---------------------------------------------------------------

  # One large star on the left; four small stars ascend along the crest
  # toward the upper right (design coordinates)
  starInfo <- data.frame(
    id = c("big", "s1", "s2", "s3", "s4"),
    sx = c(44, 80, 108, 136, 162),
    sy = c(86, 82,  74,  66,  58),
    sr = c(13, 6.5, 6.5, 6.5, 6.5)
  )
  starInfo <- cbind(starInfo, toPlot(starInfo$sx, starInfo$sy))
  starInfo$r <- starInfo$sr / 100

  starPoly <- do.call(rbind, lapply(seq_len(nrow(starInfo)), function(i) {
    star_polygon(starInfo$x[i], starInfo$y[i], starInfo$r[i], id = starInfo$id[i])
  }))

  # Two faint red horizontal lines in the upper half, hinting at ggplot2's
  # panel grid
  gridLines <- data.frame(y = c(0.61, 0.35), x = -0.54, xend = 0.54)


  # 04. Plot ---------------------------------------------------------------------

  flagRed <- "#DE2910"   # national-flag red
  darkRed <- "#A81020"   # crest line and star stroke
  gold    <- "#FFD449"   # star gold
  cream   <- "#FAF6EC"   # cream background

  p <- ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data = hex_polygon(hexR), ggplot2::aes(x, y), fill = cream
    ) +
    ggplot2::geom_segment(
      data = gridLines,
      ggplot2::aes(x = x, xend = xend, y = y, yend = y),
      colour = flagRed, alpha = 0.13, linewidth = 0.4
    ) +
    ggplot2::geom_polygon(data = redField, ggplot2::aes(x, y), fill = flagRed) +
    ggplot2::geom_path(
      data = crest, ggplot2::aes(x, y),
      colour = darkRed, linewidth = 1, lineend = "round"
    ) +
    ggplot2::geom_polygon(
      data = starPoly, ggplot2::aes(x, y, group = id),
      fill = gold, colour = darkRed, linewidth = 0.3, linejoin = "round"
    ) +
    ggplot2::geom_polygon(
      data = hex_polygon(hexR), ggplot2::aes(x, y),
      fill = NA, colour = flagRed, linewidth = 1.3, linejoin = "mitre"
    ) +
    ggplot2::annotate(
      "text", x = 0, y = -0.63, label = "ggChinaFlag",
      colour = cream, family = "sans", fontface = "bold", size = 3.8
    ) +
    ggplot2::coord_fixed(xlim = c(-0.866, 0.866), ylim = c(-1, 1), expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin     = ggplot2::margin(0, 0, 0, 0),
      plot.background = ggplot2::element_rect(fill = "transparent", colour = NA)
    )

  return(p)
}
