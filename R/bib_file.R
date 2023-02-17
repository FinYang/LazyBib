
#' @export
bibkey <- function(x){
  UseMethod("bibkey")
}
#' @export
bibkey.default <- function(x){
  path <- normalizePath(x)
  qmd_file <- lazytype::lazyqmd(path)
  bibkey(qmd_file)
}

#' @export
bibkey.lazyqmd <- function(x){
  ref_tag <- unlist(stringr::str_extract_all(x$lines[!x$yaml], "@[[:alnum:]|-]+"))
  ref_tag[!stringr::str_detect(ref_tag, "^@(tbl|sec|fig|eq)-")] %>%
    gsub("@", "", .)
}

#' @export
subset_bib <- function(in_file, out_file = NULL, bibkey = NULL, force = TRUE){
  stopifnot(tools::file_ext(in_file)=="bib")
  if(is.null(out_file)) out_file <- file.path(dirname(in_file), paste0("subset_", basename(in_file)))
  if(file.exists(out_file) & (!force)) stop(paste(out_file, "exists"))
  in_bib <- in_file %>%
    xfun::read_utf8() %>%
    select_entry() %>%
    lapply(stringr::str_subset, ".+")
  stopifnot(all(bibkey %in% names(in_bib)))
  if(is.null(bibkey)){
    out_bib <- in_bib
  } else {
    out_bib <- in_bib[bibkey]
  }
  out_bib %>%
    lapply(c, "") %>%
    unlist() %>%
    xfun::write_utf8(out_file)
}


