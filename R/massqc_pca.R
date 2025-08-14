#' @title massqc_pca
#' @description Boxplot for each samples.
#' @author Xiaotao Shen
#' \email{shenxt1990@@163.com}
#' @param object tidymass-class object.
#' @param color_by which column (sample_info) is used to color samples
#' @param point_alpha point_alpha
#' @param frame ?ggplot2::autoplot
#' @param frame.type ?ggplot2::autoplot
#' @param line Add lines or not.
#' @param ... other paramters for ggplot2::autoplot
#' @return ggplot2 plot.
#' @export
#' @examples
#' library(massdataset)
#' library(ggplot2)
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
#'   massqc_pca()
#'
#' object %>%
#'   massqc_pca(color_by = "class")
#'
#' object %>%
#'   scale %>%
#'   massqc_pca(color_by = "class")
#'
#' object %>%
#'   scale %>%
#'   massqc_pca(color_by = "class", frame = FALSE) +
#'   ggsci::scale_fill_lancet()
#'
#' object %>%
#'   scale %>%
#'   massqc_pca(color_by = "class", frame = FALSE) +
#'   ggsci::scale_fill_lancet() +
#'   ggrepel::geom_text_repel(aes(label = sample_id))
#'
#' object %>%
#'   scale %>%
#'   massqc_pca(color_by = "class", frame = FALSE) +
#'   ggsci::scale_fill_lancet() +
#'   ggrepel::geom_text_repel(aes(label = ifelse(class == "QC", sample_id, NA)))

massqc_pca <-
  function(object,
           color_by,
           point_alpha = 0.8,
           frame = TRUE,
           frame.type = 'norm',
           line = TRUE,
           ...) {
    if(!is(object = object, class2 = "mass_dataset")){
      stop("obejct should be mass_dataset class.\n")
    }
    
    if (sum(is.na(object@expression_data)) > 0) {
      warning("MVs in you object,
            \nwill remove variables > 50% and imputate with zero.\n")
      object <-
        object %>%
        massdataset::mutate_variable_na_freq()
      object <-
        object %>%
        massdataset::activate_mass_dataset(what = "variable_info") %>%
        dplyr::filter(na_freq < 0.5)
    }
    
    sample_info <- object@sample_info
    expression_data <- object@expression_data
    
    expression_data <-
      expression_data %>%
      apply(1, function(x) {
        x[is.na(x)] = min(x[!is.na(x)])
        x
      }) %>%
      t()
    
    if (missing(color_by)) {
      color_by <- "no"
    } else{
      if (all(colnames(object@sample_info) != color_by)) {
        stop("no ", color_by, " in sample_info, please check.\n")
      }
    }
    
    if (all(names(object@process_info) != "scale")) {
      warning("no scale for this dataset, try to scale() before pca.\n")
    }
    
    pca_object <- prcomp(x = t(as.matrix(expression_data)),
                         center = FALSE,
                         scale. = FALSE)
    
    if (color_by == "no") {
      plot <-
        ggfortify:::autoplot.pca_common(
          object = pca_object,
          data = sample_info,
          size = 5,
          shape = 21,
          alpha = point_alpha,
          frame = frame,
          frame.type = frame.type,
          ...
        ) +
        # geom_vline(xintercept = 0, linetype = 2) +
        # geom_hline(yintercept = 0, linetype = 2) +
        theme_bw() +
        theme(panel.grid.minor = element_blank())
    } else{
      plot <-
        autoplot(
          object = pca_object,
          data = sample_info,
          fill = color_by,
          frame.colour = color_by,
          size = 5,
          shape = 21,
          alpha = point_alpha,
          frame = frame,
          frame.type = frame.type,
          ...
        ) +
        # geom_vline(xintercept = 0, linetype = 2) +
        # geom_hline(yintercept = 0, linetype = 2) +
        theme_bw() +
        theme(panel.grid.minor = element_blank())
    }
    
    if (line) {
      plot <-
        plot +
        geom_vline(xintercept = 0, linetype = 2) +
        geom_hline(yintercept = 0, linetype = 2)
    }
    
    return(plot)
  }






#' @title massqc_pca_pc1
#' @description Boxplot for each samples.
#' @author Xiaotao Shen
#' \email{shenxt1990@@163.com}
#' @param object tidymass-class object.
#' @param color_by which column (sample_info) is used to color samples
#' @param order_by which column (sample_info) is used to order samples
#' @param point_alpha point_alpha
#' @param point_size point_size
#' @param desc desc
#' @return ggplot2 plot.
#' @export
#' @examples
#' library(massdataset)
#' library(ggplot2)
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
#'   massqc_pca_pc1()
#'
#' object %>%
#'   massqc_pca_pc1(color_by = "class")
#'
#' object %>%
#'   scale %>%
#'   massqc_pca_pc1(color_by = "class")
#'
#' object %>%
#'   scale %>%
#'   massqc_pca_pc1(
#'     color_by = "class",
#'     order_by = "injection.order",
#'     point_alpha = 1,
#'     point_size = 5
#'   ) +
#'   ggsci::scale_color_lancet()
#'
#' object %>%
#'   scale %>%
#'   massqc_pca_pc1(
#'     color_by = "class",
#'     order_by = "injection.order",
#'     point_alpha = 1,
#'     point_size = 5,
#'     desc = TRUE
#'   ) +
#'   ggsci::scale_color_lancet()


massqc_pca_pc1 <- function(object,
                           color_by,
                           order_by,
                           point_alpha = 0.8,
                           point_size = 3,
                           desc = FALSE) {
  if(!is(object = object, class2 = "mass_dataset")){
    stop("obejct should be mass_dataset class.\n")
  }
  
  if (sum(is.na(object@expression_data)) > 0) {
    warning("MVs in you object,
            \nwill remove variables > 50% and imputate with zero.\n")
    object <-
      object %>%
      massdataset::mutate_variable_na_freq()
    object <-
      object %>%
      massdataset::activate_mass_dataset(what = "variable_info") %>%
      dplyr::filter(na_freq < 0.5)
  }
  
  sample_info <- object@sample_info
  expression_data <- object@expression_data
  
  expression_data <-
    expression_data %>%
    apply(1, function(x) {
      x[is.na(x)] = min(x[!is.na(x)])
      x
    }) %>%
    t()
  
  if (missing(color_by)) {
    color_by <- "no"
  } else{
    if (all(colnames(object@sample_info) != color_by)) {
      stop("no ", color_by, " in sample_info, please check.\n")
    }
  }
  
  if (missing(order_by)) {
    order_by <- "sample_id"
  } else{
    if (all(colnames(object@sample_info) != order_by) &
        order_by != "na") {
      stop("no ", order_by, " in sample_info, please check.\n")
    }
  }
  
  pca_object <- prcomp(x = t(as.matrix(expression_data)),
                       center = FALSE,
                       scale. = FALSE)
  sample_info <-
    data.frame(sample_info, pca_object$x)
  
  if (desc) {
    temp_data <-
      sample_info %>%
      dplyr::arrange(desc(get(order_by))) %>%
      dplyr::mutate(sample_id = factor(sample_id,
                                       levels = sample_id))
    
  } else{
    temp_data <-
      sample_info %>%
      dplyr::arrange(get(order_by)) %>%
      dplyr::mutate(sample_id = factor(sample_id,
                                       levels = sample_id))
  }
  
  plot <-
    temp_data %>%
    ggplot(aes(sample_id, PC1)) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        vjust = 1
      )
    ) +
    labs(x = "")
  
  
  if (color_by == "no") {
    plot <-
      plot +
      geom_point(size = point_size,
                 alpha = point_alpha)
  } else{
    plot <-
      plot +
      ggplot2::geom_point(aes(color = get(color_by)),
                          size = point_size,
                          alpha = point_alpha) +
      guides(color = guide_legend(title = color_by))
  }
  
  return(plot)
}
