
select_entry <- function(lines){
  lab <- "^@(.*)\\{(.*)\\,$"
  idx <- cumsum(grepl(lab, lines))
  if (idx[1] == 0) {
    idx = c(0, idx)
    lines = c("", lines)
  }
  groups <- unname(split(lines, idx))
  labels <- stringr::str_trim(gsub(lab, "\\2", sapply(groups, `[`, 1)))
  entries <- setNames(groups, labels)
  return(entries)
}

end_operation <- function(entries){
  return(do.call(base::c, unname(entries)))
}
