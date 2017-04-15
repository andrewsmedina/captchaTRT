#' Tells whether the model passed CAPTCHA
#'
#' Returns TRUE if the model bypass the CAPTCHA and FALSE otherwise.
#'   If model is NULL, we ask the user to type the text manually.
#'
#' @param model model to predict CAPTCHA.
#'
#' @return logical
#'
#' @export
oraculo <- function(model = NULL) {
  u_proc <- 'https://pje.trt3.jus.br/consultaprocessual/pages/consultas/CaptchaProcesso.seam'
  query <- list('num_pje' = '407013', 'grau_pje' = '1', 'p_seq' = '10935',
                'p_vara' = '184', 'dt_autuacao' = '14/09/2015')
  r_proc <- httr::GET(u_proc, query = query)
  u_captcha <- 'https://pje.trt3.jus.br/consultaprocessual/seam/resource/captcha'
  f_dir <- tempfile()
  dir.create(f_dir)
  f_file <- paste0(f_dir, '/captcha.jpeg')
  r_captcha <- httr::GET(u_captcha, httr::write_disk(f_file))
  # plot(magick::image_read(f_file))
  # scrapr::html_view(r2)
  if (is.null(model)) {
    plot(magick::image_read(f_file))
    captcha <- readline(prompt = "Captcha: ")
  } else {
    captcha <- predict_model(f_file, model)
  }
  form <- list(
    'consultaProcFormForm' = 'consultaProcFormForm',
    'j_id57' = 'true',
    'consultaProcFormDecorate:verifyCaptcha' = captcha,
    'consultar' = 'Consultar',
    'p_ano' = '',
    'p_vara' = '184',
    'p_nome' = '',
    'p_ano2' = '',
    'dt_autuacao' = '14/09/2015',
    'numero_unic' = '',
    'num_pje' = '407013',
    'grau_pje' = '1',
    'javax.faces.ViewState' = get_state(r_processo)
  )
  u_submit <- 'https://pje.trt3.jus.br/consultaprocessual/pages/consultas/CaptchaProcesso.seam'
  r_submit <- httr::POST(u_submit, body = form)
  # scrapr::html_view(r_submit)
  unlink(f_dir, recursive = TRUE)
  passed <- r_submit %>%
    httr::content('text') %>%
    xml2::read_html() %>%
    rvest::html_node('#panelDetalhesProcesso') %>%
    length() %>%
    magrittr::is_greater_than(0)
  passed
}

get_state <- function(r) {
  r %>%
    httr::content('text') %>%
    xml2::read_html() %>%
    rvest::html_node(xpath = '//input[@id="javax.faces.ViewState"]') %>%
    rvest::html_attr('value')
}

max_freq <- function(x) {
  tab <- table(x)
  names(tab)[which.max(tab)]
}

predict_model <- function(img, model = NULL) {
  d <- captcha_trainset(img)
  dd <- captcha_trainset_one(d$data[[1]], 'a')
  dd$y <- as.factor(dd$y)
  dd$id_captcha <- rep(d$captcha_id, each = 6 * 8)
  dd$id_corte <- rep(rep(1:8, each = 6), nrow(d))
  dd$id_letra <- rep(rep(1:6, 8), nrow(d))
  dd$pred <- as.character(predict(model, dd))
  lvs <- c(".", "2", "3", "4", "5", "6", "7", "8", "9", "a", "b", "d",
           "e", "f", "h", "j", "k", "m", "n", "r", "s", "t", "u", "v", "w",
           "x", "y")
  dd_ensemble <- dd %>%
    dplyr::select(pred, x = id_corte, id_captcha, id_letra) %>%
    tidyr::spread(x, pred, fill = '.', sep = '') %>%
    dplyr::mutate_if(is.character, dplyr::funs(factor(., levels = lvs))) %>%
    dplyr::select(x1:x8, id_captcha, id_letra)
  preds <- as.character(predict(model_ensemble, dd_ensemble))
  paste(preds, collapse = '')
  # tibble::tibble(pred = preds, ordem = rep(1:6, 8)) %>%
  #   dplyr::group_by(ordem) %>%
  #   dplyr::summarise(l = max_freq(pred)) %>%
  #   with(l) %>%
  #   paste(collapse = '')
  ##
  # tibble::tibble(pred = preds, ordem = rep(1:6, 8), .id = rep(1:8, each = 6)) %>%
  #   tidyr::spread(.id, pred) %>%
  #   janitor::clean_names() %>%
  #   dplyr::select(-ordem)
}

afff <- function() {
  arqs <- dir('~/decryptr/captchaTRTData/inst/img', f=T)
  arq <- sample(arqs, 1)
  plot(magick::image_read(arq))
  predict_model(arq, model)
}
