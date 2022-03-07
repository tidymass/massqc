#' @title massqc_sample_correlation
#' @description Boxplot for each samples.
#' @author Xiaotao Shen
#' \email{shenxt1990@@163.com}
#' @param object tidymass-class object.
#' @param cor_method ?cor
#' @param method ?ggcorrplot
#' @param type ?ggcorrplot
#' @param ggtheme ?ggcorrplot
#' @param title ?ggcorrplot
#' @param show.legend ?ggcorrplot
#' @param legend.title ?ggcorrplot
#' @param show.diag ?ggcorrplot
#' @param colors ?ggcorrplot
#' @param outline.color ?ggcorrplot
#' @param hc.order ?ggcorrplot
#' @param order_by ?ggcorrplot
#' @param hc.method ?ggcorrplot
#' @param lab ?ggcorrplot
#' @param lab_col ?ggcorrplot
#' @param lab_size ?ggcorrplot
#' @param p.mat ?ggcorrplot
#' @param sig.level ?ggcorrplot
#' @param insig ?ggcorrplot
#' @param pch ?ggcorrplot
#' @param pch.col ?ggcorrplot
#' @param pch.cex ?ggcorrplot
#' @param tl.cex ?ggcorrplot
#' @param tl.col ?ggcorrplot
#' @param tl.srt ?ggcorrplot
#' @param digits ?ggcorrplot
#' @return ggplot2 plot.
#' @export
#' @examples
#' library(massdataset)
#' library(tidyverse)
#' data("expression_data")
#' data("sample_info")
#' data("variable_info")
#'
#' object =
#'   create_mass_dataset(
#'     expression_data = expression_data,
#'     sample_info = sample_info,
#'     variable_info = variable_info
#'   )
#'
#' object %>%
#'   massqc_sample_correlation()
#'
#' object %>%
#'   massqc_sample_correlation()
#'
#' object %>%
#'   activate_mass_dataset(what = "sample_info") %>%
#'   filter(class == "QC") %>%
#'   massqc_sample_correlation()
#'
#' object %>%
#'   massqc_sample_correlation(lab = TRUE)
#'
#' object %>%
#'   massqc_sample_correlation(lab = TRUE, type = "upper")

massqc_sample_correlation =
  function(object,
           cor_method = c("spearman", "kendall", "pearson"),
           method = c("circle", "square"),
           type = c("full", "lower", "upper"),
           ggtheme = ggplot2::theme_bw,
           title = "",
           show.legend = TRUE,
           legend.title = "Correlation",
           show.diag = FALSE,
           colors = c("blue", "white", "red"),
           outline.color = "gray",
           hc.order = FALSE,
           order_by,
           hc.method = "complete",
           lab = FALSE,
           lab_col = "black",
           lab_size = 4,
           p.mat = NULL,
           sig.level = 0.05,
           insig = c("pch", "blank"),
           pch = 4,
           pch.col = "black",
           pch.cex = 5,
           tl.cex = 12,
           tl.col = "black",
           tl.srt = 45,
           digits = 2) {
    if (!missing(order_by)) {
      hc.order = FALSE
      
      if (!order_by %in% colnames(object@sample_info)) {
        stop(order_by, " is not in sample_info.\n")
      } else{
        object =
          object %>%
          massdataset::activate_mass_dataset(what = "sample_info") %>%
          dplyr::arrange(order_by)
      }
    }
    
    if(!is(object = object, class2 = "mass_dataset")){
      stop("obejct should be mass_dataset class.\n")
    }
    
    cor_method = match.arg(cor_method)
    method = match.arg(method)
    type = match.arg(type)
    insig = match.arg(insig)
    if (massdataset::get_mv_number(object = object) > 0) {
      warning("NA will be set as 0.\n")
    }
    
    expression_data =
      object@expression_data
    expression_data[is.na(expression_data)] = 0
    
    cor_data =
      cor(expression_data, method = cor_method)
    
    plot =
      ggcorrplot::ggcorrplot(
        cor_data,
        type = type,
        method = method,
        ggtheme = ggtheme,
        title = title,
        show.legend = show.legend,
        legend.title = legend.title,
        show.diag = show.diag,
        colors = colors,
        outline.color = outline.color,
        hc.order = hc.order,
        hc.method = hc.method,
        lab = lab,
        lab_col = lab_col,
        lab_size = lab_size,
        p.mat = p.mat,
        sig.level = sig.level,
        insig = insig,
        pch = pch,
        pch.col = pch.col,
        pch.cex = pch.cex,
        tl.cex = tl.cex,
        tl.col = tl.col,
        tl.srt = tl.srt,
        digits = digits
      )
    return(plot)
  }
