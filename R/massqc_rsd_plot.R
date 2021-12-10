#' @title massqc_rsd_plot
#' @description RSD plot for each variable.
#' @author Xiaotao Shen
#' \email{shenxt1990@@163.com}
#' @param object tidymass-class object.
#' @param color_by which column (sample_info) is used to color samples
#' @param order_by which column (sample_info) is used to order samples
#' @param show_x_text show_x_text
#' @param show_x_ticks show_x_ticks
#' @param desc desc
#' @param point_alpha point_alpha
#' @param point_size point_size
#' @return ggplot2 plot.
#' @export
#' @examples
library(massdataset)
library(ggplot2)
library(tidyverse)
data("expression_data")
data("sample_info")
data("variable_info")

object =
  create_mass_dataset(
    expression_data = expression_data,
    sample_info = sample_info,
    variable_info = variable_info
  )

object %>% 
  massqc_rsd_plot()

object %>% 
  massqc_rsd_plot(color_by = "rsd")

object %>% 
  massqc_rsd_plot(color_by = "rsd", order_by = "rsd")

object %>% 
  massqc_rsd_plot(color_by = "rsd", point_alpha = 1) +
  scale_color_gradient(low = "skyblue", high = "red") +
  geom_hline(yintercept = 0.3, color = "red")

object %>% 
  activate_mass_dataset(what = "sample_info") %>% 
  filter(class == "Subject") %>% 
  massqc_rsd_plot(color_by = "rsd", point_alpha = 1) +
  scale_color_gradient(low = "skyblue", high = "red") +
  geom_hline(yintercept = 0.3, color = "red")



massqc_rsd_plot = function(object,
                           color_by,
                           order_by = "variable_id",
                           show_x_text = FALSE,
                           show_x_ticks = FALSE,
                           desc = FALSE,
                           point_alpha = 0.8,
                           point_size = 3) {
  massdataset::check_object_class(object = object, class = "mass_dataset")
  
  object =
    object %>%
    massdataset::activate_mass_dataset(what = "variable_info") %>%
    dplyr::select(-dplyr::contains("rsd")) %>%
    massdataset::mutate_rsd()
  
  variable_info = object@variable_info
  
  if (missing(color_by)) {
    color_by = "no"
  } else{
    if (all(colnames(object@variable_info) != color_by)) {
      stop("no ", color_by, " in variable_info, please check.\n")
    }
  }
  
  if (missing(order_by)) {
    order_by = "variable_id"
  } else{
    if (all(colnames(object@variable_info) != order_by) &
        order_by != "rsd") {
      stop("no ", order_by, " in variable_info, please check.\n")
    }
  }
  
  if (desc) {
    temp_data =
      variable_info %>%
      dplyr::arrange(desc(get(order_by))) %>%
      dplyr::mutate(variable_id = factor(variable_id,
                                         levels = variable_id))
    
  } else{
    temp_data =
      variable_info %>%
      dplyr::arrange(get(order_by)) %>%
      dplyr::mutate(variable_id = factor(variable_id,
                                         levels = variable_id))
  }
  
  plot =
    temp_data %>%
    ggplot2::ggplot(aes(variable_id, rsd)) +
    guides(color = guide_legend(title = color_by),
           size = guide_legend(title = "RSD (%)")) +
    labs(x = "",
         y = "RSD (%)") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(
            angle = 45,
            hjust = 1,
            vjust = 1
          ))
  
  if (!show_x_text) {
    plot =
      plot +
      theme(axis.text.x = element_blank()) +
      labs(x = "Variables")
  }
  
  if (!show_x_ticks) {
    plot =
      plot +
      theme(axis.ticks.x = element_blank())
  }
  
  if (color_by == "no") {
    plot =
      plot +
      ggplot2::geom_point(aes(size = rsd), size = point_size, alpha = point_alpha)
  } else{
    plot =
      plot +
      ggplot2::geom_point(aes(color = get(color_by)),
                          size = point_size, alpha = point_alpha) +
      guides(color = guide_legend(title = color_by))
  }
  
  
  return(plot)
}
