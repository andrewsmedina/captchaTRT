#' Pipe operator
#'
#' See /code{/link[magrittr]{/%>/%}} for more details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs /%>/% rhs
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


limpar <- function(arqs) {
  # path <- 'caminho/para/captchaTRTData/inst/img'
  # arqs <- dir(path, full.names = TRUE)
  # set.seed(1)
  arqs %>%
    sample(1) %>%
    ler() %>%
    dplyr::filter(r < .75, y <= 25, x <= 95, x >= 12) %>%
    desenhar()
}


# a <- download_img(dir = "C:/Users/ap_da/OneDrive/Documents/captchaTRTData/inst/img")

library(tidyverse)
library(magrittr)

arqs <- list.files("C:/Users/ap_da/OneDrive/Documents/captchaTRTData/inst/img", full.names = TRUE)

set.seed(1)
arqs %>%
  sample(1) %>%
  ler() %>%
  dplyr::filter(r < .70, y <= 25, x <= 95, x >= 10) %T>%
  {print(desenhar(.))} %>%
  mutate(parte = cut(x, breaks = c(9, 20, 30, 40, 50, 60, 70))) %>%
  filter(parte %>% is.na %>% not) %T>%
  {print(desenhar_picote(.))}
  nest(-parte)

desenhar_picote <- function(d) {
  p <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = y))
  p <- p + ggplot2::coord_equal() + ggplot2::theme_bw()
  p <- p + ggplot2::geom_point(colour = d$cor, shape = 15, size = 3)
  p +
    ggplot2::scale_x_continuous(breaks = 0:1000 * 3) +
    ggplot2::scale_y_continuous(breaks = 0:100 * 5) +
    facet_wrap(~parte, ncol = 6, scales = "free_x") +
    coord_fixed()
}

#' recebe a saida do 'ler()'
picotar <- function(d) {

}
