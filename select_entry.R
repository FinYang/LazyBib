# path <- "../Reference.bib"
# drop <- c("abstract", "keywords", "file")


select_entry <- function(lines, drop = "abstract"){
  lab <- "^@(.*)\\{(.*)\\,$"
  idx <- cumsum(grepl(lab, lines))
  if (idx[1] == 0) {
    idx = c(0, idx)
    lines = c("", lines)
  }
  groups <- unname(split(lines, idx))
  labels <- stringr::str_trim(gsub(lab, "\\2", sapply(groups, `[`, 1)))
  entries <- setNames(groups, labels)
  
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
  output <- do.call(base::c, unname(entries))
  return(output)
  # write(output, "../Reference.bib")
}
