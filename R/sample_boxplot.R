# sxtTools::setwd_project()
# setwd("test_data/")
# library(massdataset)
# data("expression_data")
# data("sample_info")
# data("sample_info_note")
# data("variable_info")
# data("variable_info_note")
# object =
#   create_tidymass_class(
#     expression_data = expression_data,
#     sample_info = sample_info,
#     variable_info = variable_info,
#     sample_info_note = sample_info_note,
#     variable_info_note = variable_info_note
#   )
# object
# 
# object@expression_data = log(object@expression_data, 10)
# 
# plot =
# sample_boxplot(object = object) +
#   geom_jitter(aes(color = class)) +
#   ggsci::scale_color_aaas()
# 
# plot

#' @title sample_boxplot
#' @description Boxplot for each samples.
#' @author Xiaotao Shen
#' \email{shenxt1990@@163.com}
#' @param object tidymass-class object.
#' @param color_by one column of sample_info from object.
#' @param outlier.shape outlier.shape from ggplot2.
#' @return ggplot2 plot.
#' @export
#' @examples 
#' library(massdataset)
#' library(ggplot2)
#' data("expression_data")
#' data("sample_info")
#' data("sample_info_note")
#' data("variable_info")
#' data("variable_info_note")
#' object =
#' create_tidymass_class(
#' expression_data = expression_data,
#' sample_info = sample_info,
#' variable_info = variable_info,
#' sample_info_note = sample_info_note,
#' variable_info_note = variable_info_note
#' )
#' object
#' object@expression_data = log(object@expression_data, 10)
#' plot =
#' sample_boxplot(object = object) +
#' geom_jitter(aes(color = class)) +
#' ggsci::scale_color_aaas()
#' plot

sample_boxplot = function(object,
                          color_by,
                          outlier.shape = NA) {
  if (class(object)[1] != "tidymass") {
    stop("only for tidymass-class object.\n")
  }
  
  sample_info = object@sample_info
  expression_data = object@expression_data
  
  if (missing(color_by)) {
    warning("color_by is not provided, will use class.\n")
    color_by = "class"
  }
  
  if (!color_by %in% colnames(sample_info)) {
    stop(color_by, " is not in the sample_info.\n")
  }
  
  if (sum(is.na(expression_data)) > 0) {
    warning("NA will be removed in the boxplot.\n")
  }
  
  plot =
    expression_data %>%
    tibble::rownames_to_column(var = "variable_id") %>%
    tidyr::pivot_longer(
      cols = -variable_id,
      names_to = "sample_id",
      values_to = "value"
    ) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::left_join(sample_info, by = c("sample_id")) %>%
    ggplot(aes(sample_id, value)) +
    geom_boxplot(aes(color = get(color_by)), 
                 outlier.shape = outlier.shape) +
    theme_bw() +
    labs(x = "", y = "Intensity") +
    guides(color = guide_legend(title = color_by))
  
  return(plot)
}