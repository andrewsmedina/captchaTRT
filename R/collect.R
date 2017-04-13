
download_img <- function(dir = "C:/Users/ap_da/OneDrive/Documents/captchaTRTData/inst/img", sleep = 1, repeat_last_captcha = FALSE) {

  url_img <- "https://pje.trt3.jus.br/consultaprocessual/seam/resource/captcha"

  if(!repeat_last_captcha) httr::handle_reset(url_img)
  if (!file.exists(dir)) dir.create(dir, recursive = TRUE)

  solicitacao <- httr::GET(url_img)
  data_hora <- stringr::str_replace_all(lubridate::now(), "[^0-9]", "")
  if (is.null(dir)) dir <- tempdir()
  arq <- tempfile(pattern = data_hora, tmpdir = dir)

  wd_img <- httr::write_disk(paste0(arq, ".jpeg"), overwrite = TRUE)
  imagem <- httr::GET(url_img, wd_img)
  while (as.numeric(solicitacao$headers[['content-length']]) < 1) {
    msg <- sprintf('Something went bad. trying again in %d seconds...', sleep)
    message(msg)
    Sys.sleep(sleep)
    imagem <- httr::GET(url_img, wd_img)
  }
  return(imagem)
}

download_imgs <- function(n, repeat_captcha_n_times = 1, dir = "C:/Users/ap_da/OneDrive/Documents/captchaTRTData/inst/img", sleep = 1) {

  repeat_last_captcha <- as.numeric((seq.int(n) - 1) %% repeat_captcha_n_times != 0)

  imagens <- repeat_last_captcha %>% purrr::map(~ baixa_img(dir = dir, sleep = sleep, .x))

  return(imagens)
}





