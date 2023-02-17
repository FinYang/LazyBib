# path <- "../Reference.bib"
# drop <- c("abstract", "keywords", "file")


drop_field <- function(entries, drop = "abstract"){


  # entry <- entries[[2]]

  foreach_entry <- function(entry){
    fie <- "^(.*)=\\s*\\{{1}(.*)\\}{1},?$"
    fields <- stringr::str_trim(gsub(fie, "\\1", entry))
    idx_keep <- !fields %in% drop
    entry_len <- sum(idx_keep)
    entry <- entry[idx_keep]
    if(entry[[entry_len]] == "}"){
      entry <- c(entry, "")
      if(stringr::str_sub(stringr::str_trim(entry[[entry_len-1]]), start = -1L, end = -1L) ==","){
        entry[[entry_len-1]] <- stringr::str_sub(stringr::str_trim(entry[[entry_len-1]]), start = 1L, end = -2L)
      }
    }
    return(entry)
  }
  entries <- lapply(entries, foreach_entry)

  return(entries)
  # write(output, "../Reference.bib")
}
