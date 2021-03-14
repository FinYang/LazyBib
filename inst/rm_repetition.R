rm_repetition <- function(entries){
  rep_name <- names(table(names(entries)))[table(names(entries))>1]
  if(length(rep_name) < 1) return(entries)
  drop_id <- lapply(rep_name, function(name) which(names(entries)%in% name))
  drop_id <- unlist(lapply(drop_id, function(x) x[-length(x)]))
  entries <- entries[-drop_id]
  return(list(entries, rep_name))
}
