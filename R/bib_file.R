
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
  key_ignore <- c("tbl", "sec", "fig", "eq",
                  "thm", "lem", "cor", "prp", "cnj", "def", "exm", "exr"
                  )
  ref_tag <- unlist(stringr::str_extract_all(x$lines[!x$yaml], "@[[:alnum:]|[-|_]]+"))
  ref_tag[!stringr::str_detect(
    ref_tag,
    paste0("^@(", paste0(key_ignore, collapse = "|"), ")-"))] %>%
    unique() %>%
    gsub("@", "", .)
}

#' @export
subset_bib <- function(in_file, out_file = NULL, bibkey = NULL, force = FALSE){
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

#' @export
append_bib <- function(in_file, out_file, bibkey = NULL){
  stopifnot(tools::file_ext(in_file)=="bib")
  in_bib <- in_file %>%
    xfun::read_utf8() %>%
    select_entry() %>%
    lapply(stringr::str_subset, ".+")

  old_bib <- out_file %>%
    xfun::read_utf8() %>%
    select_entry() %>%
    lapply(stringr::str_subset, ".+")
  if(!all(setdiff(bibkey, names(old_bib)) %in% names(in_bib))) {
    stop(paste(setdiff(setdiff(bibkey, names(old_bib)),  names(in_bib)), collapse = ", "), " not in ", in_file)
  }
  if(is.null(bibkey)){
    out_bib <- c(old_bib, in_bib[setdiff(names(in_bib), names(old_bib))])
  } else {
    out_bib <- c(old_bib, in_bib[setdiff(bibkey, names(old_bib))])
  }
  out_bib %>%
    lapply(c, "") %>%
    unlist() %>%
    xfun::write_utf8(out_file)
}

#' @export
update_bib <- function(in_file, out_file, bibkey = NULL){
  stopifnot(tools::file_ext(in_file)=="bib")
  in_bib <- in_file %>%
    xfun::read_utf8() %>%
    select_entry() %>%
    lapply(stringr::str_subset, ".+")
  stopifnot(all(bibkey %in% names(in_bib)))

  out_bib <- out_file %>%
    xfun::read_utf8() %>%
    select_entry() %>%
    lapply(stringr::str_subset, ".+")
  stopifnot(all(bibkey %in% names(out_bib)))
  if(is.null(bibkey)){
    bibkey <- intersect(names(in_bib), names(out_bib))
  }
  out_bib[bibkey] <- in_bib[bibkey]
  out_bib %>%
    lapply(c, "") %>%
    unlist() %>%
    xfun::write_utf8(out_file)
}


#' @export
compare_bib <- function(in_file, out_file, bibkey = NULL){
  stopifnot(tools::file_ext(in_file)=="bib")
  in_bib <- in_file %>%
    xfun::read_utf8() %>%
    select_entry() %>%
    lapply(stringr::str_subset, ".+")
  stopifnot(all(bibkey %in% names(in_bib)))

  out_bib <- out_file %>%
    xfun::read_utf8() %>%
    select_entry() %>%
    lapply(stringr::str_subset, ".+")
  stopifnot(all(bibkey %in% names(out_bib)))
  if(is.null(bibkey)){
    bibkey <- intersect(names(in_bib), names(out_bib))
  }
  file1 <- tempfile()
  on.exit(unlink(file1), add = TRUE)
  file2 <- tempfile()
  on.exit(unlink(file2), add = TRUE)

  (in_bib[bibkey]) %>%
    lapply(c, "") %>%
    unlist() %>%
    xfun::write_utf8(file1)
  (out_bib[bibkey]) %>%
    lapply(c, "") %>%
    unlist() %>%
    xfun::write_utf8(file2)

  diff(file1, file2, before = in_file, after = out_file)
}


