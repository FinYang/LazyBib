
title_tolower <- function(entries){
  foreach_entry <- function(entry){
    fie <- "^(.*)=\\s*\\{{0,1}(.*)\\}{0,1},?$"
    fields <- stringr::str_trim(gsub(fie, "\\1", entry))
    title_index <- which("title" == fields)
    title <- gsub(fie, "\\2", entry[title_index])
    title <- chartr("{", " ", title)
    title <- chartr("}", " ", title)
    title <- stringr::str_trim(title)
    words <- strsplit(title, " ")[[1]]
    more_capital <- (strsplit(substring(words, 2), "") %>%
                       lapply(function(x) x[x %in% c(letters, LETTERS)]) %>%
                       sapply(function(x) any(x %in% LETTERS)))
    if(any(more_capital)){
      with_dash <- words %>%
      sapply(function(x) length(strsplit(x, "-")[[1]])>1)
      more_capital <- more_capital & (!with_dash)
    }

    words[!more_capital] <- tolower(words[!more_capital])
    words[[1]] <- paste0(toupper(substring(words[[1]], 1, 1)), substring(words[[1]], 2))
    colon_index <- which(str_split(words, "") %>%
      sapply(function(x) any(":" == x)))
    if(length(colon_index) >0)
      words[[colon_index+1]] <- paste0(toupper(substring(words[[colon_index+1]], 1, 1)), substring(words[[colon_index+1]], 2))

    title <- paste(words, collapse = " ")
    entry[title_index] <- paste0("title = {{", title, "}},")
    return(entry)
  }
  entries <- lapply(entries, foreach_entry)
  return(entries)

}

