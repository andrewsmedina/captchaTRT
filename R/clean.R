#' Tira pontos que nao tenham pixels maior ou igual a k na vizinhança de n
#'
#' @export
tirar_sujeira <- function(d, k = 6, n = 1, r_lim = 0, tab_completa = NULL) {
  if (is.null(tab_completa)) {
    tab_completa <- list(x = min(d$x):max(d$x), y = min(d$y):max(d$y)) %>%
      purrr::cross_df()
  }
  fk <- function(x, ...) {9 - sum(x)}
  mk <- matrix(1, 1 + 2 * n, 1 + 2 * n)
  arrumado <- dplyr::left_join(tab_completa, d, c('x', 'y')) %>%
    dplyr::mutate(r = ifelse(is.na(r), 1, r), r = ifelse(r < 1, 0, 1))
  for(j in k) {
    m_inicial <- arrumado %>% converter_em_matriz()
    m <- m_inicial %>%
      raster::raster() %>%
      raster::focal(mk, fk, pad = TRUE, padValue = 1) %>%
      raster::as.matrix()
    m <- ifelse(m >= j & m_inicial == 0, 0, 1)
    arrumado <- m %>% converter_em_df()
  }
  arrumado %>%
    dplyr::filter(r == 0)
}

porra <- function() {
  set.seed(1)
  f <- sample(fs, 1)
  p1 <- f %>%
    captcha_read() %>%
    captcha_clean() %>%
    captcha_draw()
  p2 <- f %>%
    captcha_read() %>%
    captcha_clean() %>%
    tirar_sujeira(k = rep(4, 10)) %>%
    captcha_clean() %>%
    dplyr::filter(x <= 100, y <= 25) %>%
    captcha_draw()
  gridExtra::grid.arrange(p1, p2, ncol = 1)
}


#' Função para converter a imagem em uma matriz 180x50
#'
#' @export
converter_em_matriz <- function(d) {
  d %>%
    dplyr::select(x, y, r) %>%
    tidyr::spread(x, r, fill = 1) %>%
    dplyr::select(-y) %>%
    as.matrix()
}


#' Função para converter a imagem em um data.frame do jeito correto
#'
#'
#' @export
converter_em_df <- function(m) {
  m %>%
    as.data.frame() %>%
    dplyr::mutate(y = as.numeric(1:nrow(.))) %>%
    tidyr::gather(x, r, -y) %>%
    dplyr::mutate(x = readr::parse_number(x))
}
