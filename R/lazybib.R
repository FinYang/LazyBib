#' @export
lazybib <- function(x){
  UseMethod("lazybib")
}
#' @export
lazybib.list <- function(x){
  bib_list <- lapply(x, lazybib)
  bib_list[!sapply(bib_list, is.null)]
}
#' @export
lazybib.character <- function(x){
  key_lab <- "^@(.*)?\\{(.*),"
  entry_type <- gsub(key_lab, "\\1", x[[1]])
  bibkey <- gsub(key_lab, "\\2", x[[1]])
  if(stringr::str_length(entry_type)==0) return(NULL)
  field_idx <- grep("=", x)
  fields <- x[do.call(seq, as.list(range(field_idx)))]
  fields <- split(fields, cumsum(grepl("=", fields))) %>%
    lapply(stringr::str_trim) %>%
    lapply(paste, collapse = " ") %>%
    unlist()


  field_lab <- "^(.*)?=(.*),*"
  field_name <- gsub(field_lab, "\\1", fields) %>% stringr::str_trim()
  field_content <- gsub(field_lab, "\\2", fields) %>%
    stringr::str_trim() %>%
    gsub('"', "", .) %>%
    gsub(",?$", "", .) %>%
    stringr::str_trim()
  `names<-`(c(entry_type, bibkey, field_content), c("entry_type", "bibkey", field_name))
}




