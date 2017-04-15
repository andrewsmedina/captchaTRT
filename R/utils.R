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
captcha_read <- function(a, scale = '100%') {
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
  file.remove(tmp)
  img_df %>%
    dplyr::mutate(colour = rgb(r, r, r), id = 1:n()) %>%
    dplyr::filter(colour != '#FFFFFF')
}

#' Desenhar Captcha
#'
#' copiada do captchasaj
#'
#' @import ggplot2
#' @export
captcha_draw <- function(d) {
  p <- ggplot(d, aes(x = x, y = y)) +
    coord_equal() +
    theme_bw() +
    geom_point(colour = d$colour, shape = 15, size = 3) +
    scale_x_continuous(breaks = 0:1000 * 3) +
    scale_y_continuous(breaks = 0:100 * 5)
  p
}

captcha_clean <- function(d) {
  # d %>%
  #   dplyr::filter(r < .75, y <= 25, x <= 95, x >= 12)
  d %>%
    dplyr::mutate(x = x - min(x) + 1, y = y - min(y) + 1) %>%
    dplyr::filter(r < .75, y <= 25, x <= 97, x >= 1)
}


# a <- download_img(dir = "C:/Users/ap_da/OneDrive/Documents/captchaTRTData/inst/img")
# library(tidyverse)
# library(magrittr)
#
# arqs <- list.files("C:/Users/ap_da/OneDrive/Documents/captchaTRTData/inst/img", full.names = TRUE)
#
# set.seed(1)
# arqs %>%
#   sample(1) %>%
#   ler() %>%
#   dplyr::filter(r < .70, y <= 25, x <= 95, x >= 10) %T>%
#   {print(desenhar(.))} %>%
#   mutate(parte = cut(x, breaks = c(9, 20, 30, 40, 50, 60, 70))) %>%
#   filter(parte %>% is.na %>% not) %T>%
#   {print(desenhar_picote(.))}
#   nest(-parte)

captcha_draw_cut <- function(d) {
  p <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = y))
  p <- p + ggplot2::coord_equal() + ggplot2::theme_bw()
  p <- p + ggplot2::geom_point(colour = d$cor, shape = 15, size = 3)
  p +
    ggplot2::scale_x_continuous(breaks = 0:1000 * 3) +
    ggplot2::scale_y_continuous(breaks = 0:100 * 5) +
    facet_wrap(~parte, ncol = 6, scales = "free_x") +
    coord_fixed()
}

captcha_classify_one <- function(a, folder, remove) {
  plot(magick::image_read(a))
  txt <- readline(prompt = "Text: ")
  date_time <- stringr::str_replace_all(lubridate::now(), "[^0-9]", "")
  new_file <- glue::glue('{folder}/{date_time}_{txt}.png')
  if (remove) {
    file.rename(a, new_file)
  } else {
    file.copy(a, new_file)
  }
}

#' Classify CAPTCHAS
#'
#' @param files character vector containing file paths
#' @param folder folder to save new files.
#' @param remove if true, remove original file. Otherwise, copy it.
#'
#' @export
captcha_classify <- function(files, folder, remove = TRUE) {
  dir.create(folder, showWarnings = FALSE)
  for(i in seq_along(files)) captcha_classify_one(files[i], folder, remove)
}

captcha_trainset <- function(files, clean = TRUE, cut = FALSE) {
  labels <- files %>%
    basename() %>%
    tools::file_path_sans_ext() %>%
    stringr::str_match('_([0-9a-z]{6}$)') %>%
    magrittr::extract(TRUE, 2)
  l_captcha <- list(f = files, l = labels, s = seq_along(labels)) %>%
    purrr::pmap(function(f, l, s) {
      d <- dplyr::mutate(captcha_read(f), label = l, captcha_id = s)
      if(clean) captcha_clean(d) else d
    })
  if (cut) {
    stop('not implemented.')
  } else {
    d_captcha <- l_captcha %>%
      dplyr::bind_rows() %>%
      dplyr::group_by(captcha_id, label) %>%
      tidyr::nest() %>%
      dplyr::mutate(label = strsplit(label, ''))
  }
  d_captcha
}

captcha_cut <- function(d, cuts) {
  d_complete <- purrr::cross_df(list(x = 1:15, y = 1:25))
  purrr::map2_df(cuts[-length(cuts)], cuts[-1], ~{
    d %>%
      dplyr::filter(x > .x, x <= .y) %>%
      dplyr::mutate(x = x - min(x) + 1) %>%
      dplyr::right_join(d_complete, c('x', 'y')) %>%
      dplyr::mutate(x = sprintf('x%02d', as.numeric(x))) %>%
      dplyr::mutate(y = sprintf('y%02d', as.numeric(y))) %>%
      dplyr::select(x, y, r) %>%
      tidyr::replace_na(list(r = 0)) %>%
      tidyr::unite(xy, x, y) %>%
      dplyr::arrange(xy) %>%
      tidyr::spread(xy, r, fill = 0)
  })
}

captcha_trainset_one <- function(data, label) {
  # i <- sample(1:nrow(data), 1)
  # data <- d_captcha$data[[i]]
  # label <- d_captcha$label[[i]]
  purrr::map_df(0:7, function(ct) {
    cuts <- 15 * 0:6 + ct
    captcha_cut(data, cuts) %>%
      dplyr::mutate(y = label) %>%
      dplyr::select(y, dplyr::everything())
  })
}

aff <- function(x) {

  d_captcha <- captcha_trainset(files)
  aff <- purrr::map2_df(d_captcha$data, d_captcha$label, captcha_trainset_one)
  aff <- dplyr::bind_rows(aff)
  aff$y <- as.factor(aff$y)
  aff$id_captcha <- rep(d_captcha$captcha_id, each = 6 * 8)
  aff$id_corte <- rep(rep(1:8, each = 6), nrow(d_captcha))
  aff$id_letra <- rep(rep(1:6, 8), nrow(d_captcha))

  set.seed(100)
  ids_train <- sample(seq_len(nrow(aff)), round(nrow(aff) * 2 / 3), replace = FALSE)
  aff_train <- aff[ids_train,]
  aff_test <- aff[-ids_train,]
  model <- randomForest::randomForest(y ~ . -id_captcha -id_corte, data = aff_train)

  preds <- as.character(predict(model))
  aff_train$pred <- preds
  aff_train_ensemble <- aff_train %>%
    dplyr::select(y, pred, x = id_corte, id_captcha, id_letra) %>%
    tidyr::spread(x, pred, fill = '.', sep = '') %>%
    dplyr::mutate_if(is.character, as.factor)

  model_ensemble <- randomForest::randomForest(y ~ . - id_captcha - id_letra,
                                               data = aff_train_ensemble)


  aff_test$pred <- as.character(predict(model, aff_test))
  aff_test_ensemble <- aff_test %>%
    dplyr::select(y, pred, x = id_corte, id_captcha, id_letra) %>%
    tidyr::spread(x, pred, fill = '.', sep = '') %>%
    dplyr::mutate_if(is.character, as.factor)

  aff_test_ensemble$pred <- predict(model_ensemble, aff_test_ensemble)

  with(aff_test_ensemble, table(y, pred)) %>%
    as.data.frame() %>%
    tidyr::spread(pred, Freq, fill = '.') %>%
    View()

  # d_captcha %>%
  #   dplyr::sample_n(1) %>%
  #   with(data[[1]]) %>%
  #   dplyr::filter(x >= min(cortes), x <= max(cortes)) %>%
  #   captcha_draw() +
  #   ggplot2::geom_vline(xintercept = cortes)
  #
  #
  # d_captcha %>%
  #   dplyr::mutate(data = purrr::map(data, ~{
  #   }))
  #
  # d_captcha %>%
  #   dplyr::sample_n(1) %>%
  #   with(data[[1]]) %>%
  #   captcha_draw()
}


