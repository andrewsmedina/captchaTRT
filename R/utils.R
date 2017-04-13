#' Pipe operator
#'
#' See \code{\link[magrittr]{\%>\%}} for more details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' Ler Captcha
#'
#' copiada do captchasaj.
#'
#' @export
#'
ler <- function(a) {
  img <- jpeg::readJPEG(a)
  img_dim <- dim(img)
  img_df <- tibble::tibble(
    x = rep(1:img_dim[2], each = img_dim[1]),
    y = rep(img_dim[1]:1, img_dim[2]),
    r = as.vector(img[,,1]),
    g = as.vector(img[,,2]),
    b = as.vector(img[,,3])
  )
  d <- dplyr::mutate(img_df, cor = rgb(r, g, b), id = 1:n())
  d <- dplyr::filter(d, cor != '#FFFFFF')
  d
}

#' Desenhar Captcha
#'
#' copiada do captchasaj
#'
#' @import ggplot2
#' @export
#'
desenhar <- function(d) {
  p <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = y))
  p <- p + ggplot2::coord_equal() + ggplot2::theme_bw()
  p <- p + ggplot2::geom_point(colour = d$cor, shape = 15, size = 3)
  p +
    ggplot2::scale_x_continuous(breaks = 0:1000 * 3) +
    ggplot2::scale_y_continuous(breaks = 0:100 * 5)
}


limpar <- function() {

}
