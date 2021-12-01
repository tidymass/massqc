.onAttach <- function(...) {
  needed <- core[!is_attached(core)]
  if (length(needed) == 0)
    return()
  
  crayon::num_colors(TRUE)
  massqc_attach()
}

is_attached <- function(x) {
  paste0("package:", x) %in% search()
}

