#' Tells whether the model broke the captcha
#'
#' Returns TRUE if the model managed to break the captcha and FALSE otherwise.
#'   If model is NULL, we ask the user to insert the captcha.
#'
#' @param model model to predict captcha.
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
  arq_dir <- tempfile()
  dir.create(arq_dir)
  arq_file <- paste0(arq_dir, '/captcha.jpeg')
  r_captcha <- httr::GET(u_captcha, httr::write_disk(arq_file))
  # plot(magick::image_read(arq_file))
  # scrapr::html_view(r2)
  if (is.null(model)) {
    plot(magick::image_read(arq_file))
    captcha <- readline(prompt = "Captcha: ")
  } else {
    captcha <- predizer_model(arq_file, model)
  }
  dados <- list(
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
  r_submit <- httr::POST(u_submit, body = dados)
  # scrapr::html_view(r_submit)
  unlink(arq_dir, recursive = TRUE)
  passou <- r_submit %>%
    httr::content('text') %>%
    xml2::read_html() %>%
    rvest::html_node('#panelDetalhesProcesso') %>%
    length() %>%
    magrittr::is_greater_than(0)
  passou
}

get_state <- function(r) {
  r %>%
    httr::content('text') %>%
    xml2::read_html() %>%
    rvest::html_node(xpath = '//input[@id="javax.faces.ViewState"]') %>%
    rvest::html_attr('value')
}

predizer_model <- function(img, model = NULL) {
  '123456'
}
