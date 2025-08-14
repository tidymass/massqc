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
#'   massqc_rsd_plot()
#'
#' object %>%
#'   massqc_rsd_plot(color_by = "rsd")
#'
#' object %>%
#'   massqc_rsd_plot(color_by = "rsd", order_by = "rsd")
#'
#' object %>%
#'   massqc_rsd_plot(color_by = "rsd", point_alpha = 1) +
#'   scale_color_gradient(low = "skyblue", high = "red") +
#'   geom_hline(yintercept = 0.3, color = "red")
#'
#' object %>%
#'   activate_mass_dataset(what = "sample_info") %>%
#'   filter(class == "Subject") %>%
#'   massqc_rsd_plot(color_by = "rsd", point_alpha = 1) +
#'   scale_color_gradient(low = "skyblue", high = "red") +
#'   geom_hline(yintercept = 0.3, color = "red")



massqc_rsd_plot <-
  function(object,
           color_by,
           order_by = "variable_id",
           show_x_text = FALSE,
           show_x_ticks = FALSE,
           desc = FALSE,
           point_alpha = 0.8,
           point_size = 3) {
    if (!is(object = object, class2 = "mass_dataset")) {
      stop("obejct should be mass_dataset class.\n")
    }
    
    object <-
      object %>%
      massdataset::activate_mass_dataset(what = "variable_info") %>%
      dplyr::select(-dplyr::contains("rsd")) %>%
      massdataset::mutate_rsd()
    
    object@variable_info$rsd[object@variable_info$rsd < 0] <- 0
    
    variable_info <- object@variable_info
    
    if (missing(color_by)) {
      color_by <- "no"
    } else{
      if (all(colnames(object@variable_info) != color_by)) {
        stop("no ", color_by, " in variable_info, please check.\n")
      }
    }
    
    if (missing(order_by)) {
      order_by <- "variable_id"
    } else{
      if (all(colnames(object@variable_info) != order_by) &
          order_by != "rsd") {
        stop("no ", order_by, " in variable_info, please check.\n")
      }
    }
    
    if (desc) {
      temp_data <-
        variable_info %>%
        dplyr::arrange(desc(get(order_by))) %>%
        dplyr::mutate(variable_id = factor(variable_id,
                                           levels = variable_id))
      
    } else{
      temp_data <-
        variable_info %>%
        dplyr::arrange(get(order_by)) %>%
        dplyr::mutate(variable_id = factor(variable_id,
                                           levels = variable_id))
    }
    
    temp_data$rsd[temp_data$rsd < 0] < 0
    
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
        ggplot2::geom_point(aes(size = rsd),
                            size = point_size, alpha = point_alpha)
    } else{
      plot =
        plot +
        ggplot2::geom_point(aes(color = get(color_by)),
                            size = point_size, alpha = point_alpha) +
        guides(color = guide_legend(title = color_by))
    }
    
    
    return(plot)
  }











#' @title massqc_cumulative_rsd_plot
#' @description massqc_cumulative_rsd_plot
#' @author Xiaotao Shen
#' \email{shenxt1990@@163.com}
#' @param ... one or more mass_dataset object
#' @param title missing or same length (character vector) with ...
#' @param rsd_cutoff numeric
#' @param color when one mass_dataset, the line color
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
#' massqc_cumulative_rsd_plot(object, rsd_cutoff = 30, color = "blue")
#'
#' object1 =
#'   object %>%
#'   activate_mass_dataset(what = "sample_info") %>%
#'   dplyr::filter(class == "QC")
#'
#' object2 =
#'   object %>%
#'   activate_mass_dataset(what = "sample_info") %>%
#'   dplyr::filter(class == "Subject")
#'
#' massqc_cumulative_rsd_plot(object1, object2,
#'                            title = c("QC", "Subject")) +
#'   ggsci::scale_color_lancet()
#'
#' massqc_cumulative_rsd_plot(object1,
#'                            object2,
#'                            rsd_cutoff = 30,
#'                            title = c("QC", "Subject")) +
#'   ggsci::scale_color_lancet()
#'
#' massqc_cumulative_rsd_plot(
#'   object1,
#'   title = c("QC"),
#'   color = "red",
#'   rsd_cutoff = 30
#' )
#' massqc_cumulative_rsd_plot(
#'   object2,
#'   title = c("Subject"),
#'   color = "blue",
#'   rsd_cutoff = 50
#' )

massqc_cumulative_rsd_plot = function(...,
                                      title,
                                      rsd_cutoff,
                                      color) {
  object = list(...)
  if (missing(title)) {
    title = paste("object", seq_along(object), sep = "_")
  } else{
    if (length(title) != length(object)) {
      stop("objects should be same length with title.\n")
    }
  }
  object %>%
    purrr::walk(function(x) {
      if (!is(object = x, class2 = "mass_dataset")) {
        stop("obejct should be mass_dataset class.\n")
      }
    })
  
  object =
    object %>%
    purrr::map(function(x) {
      x %>%
        massdataset::activate_mass_dataset(what = "variable_info") %>%
        dplyr::select(-dplyr::contains("rsd")) %>%
        massdataset::mutate_rsd()
    })
  
  rsd =
    object %>%
    lapply(function(x) {
      x@variable_info$rsd[!is.na(x@variable_info$rsd)]
    })
  
  commulative_distributation =
    rsd %>%
    lapply(function(x) {
      ecdf(x = x)
    })
  
  temp_data =
    purrr::map2(.x = rsd, .y = title, function(x, y) {
      commulative_distributation =
        ecdf(x = x)
      data.frame(rsd = seq(
        from = 0,
        to = max(x),
        length.out = 1000
      )) %>%
        dplyr::mutate(freq = commulative_distributation(rsd) * 100,
                      title = y)
    }) %>%
    dplyr::bind_rows()
  
  if (length(unique(temp_data$title)) == 1) {
    if (!missing(color)) {
      color = color[1]
    } else{
      color = "black"
    }
    plot =
      temp_data %>%
      ggplot2::ggplot(aes(rsd, freq)) +
      geom_line(aes(group = title),
                color = color)
  } else{
    plot =
      temp_data %>%
      ggplot2::ggplot(aes(rsd, freq)) +
      geom_line(aes(group = title, color = title))
  }
  
  plot =
    plot +
    labs(x = "RSD (%)",
         y = "Percentage (%)") +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    guides(color = guide_legend(title = ""))
  
  if (!missing(rsd_cutoff)) {
    plot =
      plot +
      geom_vline(xintercept = 30, color = "red")
    
    value =
      commulative_distributation %>%
      purrr::map(function(x) {
        x(rsd_cutoff)
      }) %>%
      unlist() %>%
      `*`(100) %>%
      round(2)
    
    annotate_text =
      data.frame(value, x = rsd_cutoff,
                 title = title)
    
    plot =
      plot +
      ggrepel::geom_label_repel(
        aes(x, value,
            label = value,
            color = title),
        data = annotate_text,
        show.legend = FALSE
      )
  }
  return(plot)
}
