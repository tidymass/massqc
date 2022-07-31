#' @title massqc_sample_boxplot
#' @description Boxplot for each samples.
#' @author Xiaotao Shen
#' \email{shenxt1990@@163.com}
#' @param object tidymass-class object.
#' @param color_by which column (sample_info) is used to color samples
#' @param fill_by which column (sample_info) is used to fill samples
#' @param order_by which column (sample_info) is used to order samples
#' @param outlier.shape outlier.shape from ggplot2.
#' @param point point or not.
#' @param point_alpha point_alpha
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
#'   massqc_sample_boxplot()
#'
#' object %>%
#'   log(10) %>%
#'   massqc_sample_boxplot()
#'
#' object %>%
#'   log(10) %>%
#'   massqc_sample_boxplot(color_by = "class")
#'
#' object %>%
#'   log(10) %>%
#'   massqc_sample_boxplot(fill_by = "class") +
#'   ggsci::scale_fill_lancet()
#'
#' object %>%
#'   log(10) %>%
#'   massqc_sample_boxplot(
#'     fill_by = "class",
#'     color_by = "class",
#'     point = TRUE,
#'     point_alpha = 0.3
#'   ) +
#'   ggsci::scale_fill_lancet()
#'
#' object %>%
#'   log(10) %>%
#'   massqc_sample_boxplot(color_by = "class",
#'                  point = TRUE,
#'                  point_alpha = 0.3) +
#'   ggsci::scale_color_lancet()

massqc_sample_boxplot <-
  function(object,
           color_by,
           fill_by,
           order_by = "sample_id",
           outlier.shape = NA,
           point = FALSE,
           point_alpha = 0.8) {
    if (!is(object = object, class2 = "mass_dataset")) {
      stop("obejct should be mass_dataset class.\n")
    }
    sample_info <-
      object %>%
      extract_sample_info()
    
    expression_data <-
      object %>%
      extract_expression_data()
    
    if (missing(color_by)) {
      color_by = "no"
    } else{
      if (all(colnames(object@sample_info) != color_by)) {
        stop("no ", color_by, " in sample_info, please check.\n")
      }
    }
    
    if (color_by == "batch") {
      sample_info <-
        extract_sample_info(object)
      sample_info$batch <-
        as.character(sample_info$batch)
      slot(object, "sample_info") <- 
        sample_info
    }
    
    if (missing(fill_by)) {
      fill_by <- "no"
    } else{
      if (all(colnames(object@sample_info) != fill_by)) {
        stop("no ", fill_by, " in sample_info, please check.\n")
      }
    }
    
    if (missing(order_by)) {
      order_by <- "sample_id"
    } else{
      if (all(colnames(object@sample_info) != order_by)) {
        stop("no ", order_by, " in sample_info, please check.\n")
      }
    }
    
    if (sum(is.na(expression_data)) > 0) {
      warning("NA will be removed in the boxplot.\n")
    }
    
    plot <-
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
      theme_bw() +
      labs(x = "", y = "Intensity")
    
    #######1
    if (color_by == "no" & fill_by == "no") {
      plot <-
        plot +
        geom_boxplot(outlier.shape = outlier.shape)
      
      if (point) {
        plot <-
          plot +
          geom_jitter(alpha = point_alpha)
      }
    }
    
    ######2
    if (color_by != "no" & fill_by == "no") {
      plot <-
        plot +
        geom_boxplot(aes(color = get(color_by)),
                     outlier.shape = outlier.shape) +
        guides(color = guide_legend(title = color_by))
      
      if (point) {
        plot <-
          plot +
          geom_jitter(aes(color = get(color_by)),
                      alpha = point_alpha,
                      show.legend = FALSE)
      }
    }
    
    
    ######3
    if (color_by == "no" & fill_by != "no") {
      plot <-
        plot +
        geom_boxplot(aes(fill = get(fill_by)),
                     outlier.shape = outlier.shape) +
        guides(fill = guide_legend(title = fill_by))
      
      if (point) {
        plot <-
          plot +
          geom_jitter(aes(fill = get(fill_by)),
                      alpha = point_alpha,
                      show.legend = FALSE)
      }
    }
    
    ###4
    if (color_by != "no" & fill_by != "no") {
      plot <-
        plot +
        geom_boxplot(aes(fill = get(fill_by),
                         color = get(color_by)),
                     outlier.shape = outlier.shape) +
        guides(fill = guide_legend(title = fill_by),
               color = guide_legend(title = color_by))
      
      if (point) {
        plot <-
          plot +
          geom_jitter(aes(fill = get(fill_by),
                          color = get(color_by)),
                      alpha = point_alpha,
                      show.legend = FALSE)
      }
    }
    
    return(plot)
  }