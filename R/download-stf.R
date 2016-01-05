#' Download de processos do stf
#'
#' @export
download_stf <- function(n_processos, path) {
  download_stf_um <- function(p, path) {
    u_base <- 'http://www.stf.jus.br/portal/processo'
    u <- sprintf('%s/listarProcessoUnico.asp', u_base)
    p <- gsub('[^0-9]', '', p)
    dat <- list('dropmsgoption'='5', 'partesAdvogadosRadio'='1', 'numero'=p)
    link <- httr::POST(u, body = dat, encode = 'form') %>%
      httr::content('text') %>%
      xml2::read_html() %>%
      rvest::html_nodes('.resultadoLista') %>%
      dplyr::first() %>%
      rvest::html_nodes('a') %>%
      dplyr::first() %>%
      rvest::html_attr('href') %>%
      stringr::str_trim()
    u_processo <- sprintf('%s/%s', u_base, link)
    r <- httr::GET(u_processo)
    arq <- sprintf('%s/%s.html', path, p)
    cat(httr::content(r, 'text'), file = arq)
    return('OK')
  }

  f <- dplyr::failwith('Erro na busca', download_stf_um, quiet = TRUE)
  d <- dplyr::data_frame(n_processo = n_processos) %>%
    dplyr::distinct(n_processo) %>%
    dplyr::group_by(n_processo) %>%
    dplyr::do(result = f(p = .$n_processo, path = path)) %>%
    tidyr::unnest(result)
  d
}

buscar_abas <- function(r) {
  abas <- r %>%
    httr::content('text') %>%
    xml2::read_html() %>%
    rvest::html_nodes('.abas > li') %>%
    {dplyr::data_frame(
      text = rvest::html_text(.),
      link = rvest::html_attr(rvest::html_nodes(., 'a'), 'href')
    )} %>%
    dplyr::mutate(text = stringr::str_trim(text))
  abas
}

#' Pega andamentos do STF
#'
#' Pega andamentos do stf a partir de arquivos já baixados.
#'
#' @param arqs character vector com nomes dos arquivos html
#' baixados usando \code{\link{download_stf}}
#'
#' @export
parse_stf <- function(arqs) {
  parse_stf_um <- function(a) {
    trim <- stringr::str_trim
    tab <- a %>%
      xml2::read_html(encoding = 'UTF-8') %>%
      rvest::html_nodes('.resultadoAndamentoProcesso') %>%
      dplyr::first() %>%
      rvest::html_table() %>%
      dplyr::tbl_df() %>%
      arrumar_nomes() %>%
      dplyr::mutate_each(dplyr::funs(trim))
    tab
  }
  d_erro <- dplyr::data_frame('Erro no parse')
  f <- dplyr::failwith(d_erro, parse_stf_um, quiet = TRUE)
  d <- dplyr::data_frame(arq = arqs) %>%
    dplyr::mutate(n_processo = gsub('[^0-9]', '', basename(arq))) %>%
    dplyr::distinct(arq) %>%
    dplyr::group_by(arq, n_processo) %>%
    dplyr::do(f(a = .$arq)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-arq)
  d
}

#' Pega andamentos do STF
#'
#' Guarda em arquivos temporários, depois deleta
#'
#' @export
download_parse_stf <- function(n_processos) {
  tmpdir <- 'temporariotemporario'
  dir.create(tmpdir)
  cat('downloading...\n')
  d <- download_stf(n_processos, tmpdir)
  d_ok <- dplyr::filter(d, result == 'OK')
  if(nrow(d_ok) < 0) return(dplyr::data_frame())
  arqs <- sprintf('%s/%s.html', tmpdir, d_ok$n_processo)
  cat('parsing...\n')
  d_parse <- parse_stf(arqs)
  unlink(tmpdir, recursive = TRUE)
  d_parse
}
