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


globalVariables(names = c(
  "silence.deprecated",
  "name",
  "injection.order",
  "peak.name",
  "sample.name",
  "min.fraction.qc",
  "batch",
  "median",
  "maxo",
  "sample_name",
  "intensity",
  "rt",
  "rtmed",
  "mzmed",
  "x",
  "freq",
  "xdata3",
  "group",
  "mz",
  "colou.index",
  "X1",
  "X2",
  "Class",
  "value",
  "sample_group",
  "rt.min",
  "rt.max",
  "min.intensity",
  "max.intensity",
  "diffRT",
  "data.bpca",
  "int",
  "query.RT",
  "reference.RT",
  "Degree.Span.MSE",
  "Poly.MSE",
  "extenstion",
  "rt_correction",
  "variable_id",
  "value",
  "sample_id",
  "na_freq",
  "PC1"
))

