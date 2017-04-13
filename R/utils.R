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
ler <- function(a, scale = '70%') {
  tmp <- tempfile(pattern = '.jpeg')
  magick::image_read(a) %>%
    magick::image_scale(scale) %>%
    magick::image_write(tmp)
  img <- jpeg::readJPEG(tmp)
  img_dim <- dim(img)
  img_df <- tibble::tibble(
    x = rep(1:img_dim[2], each = img_dim[1]),
    y = rep(img_dim[1]:1, img_dim[2]),
    r = as.vector(img)
  )
  d <- dplyr::mutate(img_df, cor = rgb(r, r, r), id = 1:n())
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
  # path <- 'caminho/para/captchaTRTData/inst/img'
  # arqs <- dir(path, full.names = TRUE)
  # set.seed(1)
  arqs %>%
    sample(1) %>%
    ler() %>%
    dplyr::filter(r < .7, y <= 25, x <= 95, x >= 12) %>%
    desenhar()
}

#' classificar Captcha
#'
#' adaptada do captchasaj
#'
#' @export
classificar <- function(arq, path) {
  plot(magick::image_read(arq))
  letras <- readline(prompt="Letras: ")
  data_hora <- stringr::str_replace_all(lubridate::now(), "[^0-9]", "")
  file.rename(arq, sprintf('%s/%s_%s.png', path, data_hora, letras))
}

#' classificar Captchas
#'
#' @export
classificar_arqs <- function(arqs, path) {
  dir.create(path, showWarnings = FALSE)
  for(i in seq_along(arqs)) classificar(arqs[i], path)
}
