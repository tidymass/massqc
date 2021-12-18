#' @title Report data quality
#' @description Report data quality
#' @author Xiaotao Shen
#' \email{shenxt1990@@163.com}
#' @param object tidymass-class object.
#' @param path path
#' @param type type html, pdf or all.
#' @return reporting result
#' @export
massqc_report = function(object,
                         path = ".",
                         type = c("html", "pdf", "all")) {
  dir.create(path, showWarnings = FALSE)
  type = match.arg(type)
  massdataset::check_object_class(object = object, class = "mass_dataset")
  
  ###path
  if (length(grep("Report", dir(path))) > 0) {
    output_path = file.path(path, paste('Report', length(grep(
      "Report", dir(path)
    )) + 1, sep = "_"))
  }
  
  # dir.create(output_path, showWarnings = FALSE)
  
  ####get the template
  rmarkdown::draft(
    file = output_path,
    template = "massqc",
    package = "massqc",
    create_dir = TRUE,
    edit = FALSE
  )
  
  ###parameters
  parameters =
    object@process_info %>%
    lapply(function(x) {
      data.frame(
        pacakge_name = x@pacakge_name,
        function_name = x@function_name,
        parameter = purrr::map2(names(x@parameter), x@parameter, function(name, value) {
          if (length(value) > 5) {
            value = head(value, 5)
            value = paste(c(value, "..."), collapse = ',')
          } else{
            value = paste(value, collapse = ',')
          }
          paste(name, value, sep = ":")
        }) %>% unlist(),
        time = x@time
      )
    }) %>%
    dplyr::bind_rows()
  
  save(parameters, file = file.path(output_path, "parameters.rda"))
  
  ####sample information
  save(object, file = file.path(output_path, "object.rda"))
  
  if (ncol(object) * nrow(object) > 10000) {
    hex = TRUE
  } else{
    hex = FALSE
  }
  
  plot =
    massdataset::show_mz_rt_plot(object = log(object), hex = hex)
  
  ggplot2::ggsave(
    filename = file.path(output_path, "mz_rt_plot.png"),
    plot = plot,
    width = 8,
    height = 6,
    dpi = 600
  )
  
  ggplot2::ggsave(
    filename = file.path(output_path, "mz_rt_plot.pdf"),
    plot = plot,
    width = 8,
    height = 6,
    dpi = 600
  )
  
  ####missing value
  ####in dataset
  plot =
    massdataset::show_missing_values(
      object = object,
      show_row_names =
        ifelse(nrow(object) < 20, TRUE, FALSE),
      show_column_names = ifelse(ncol(object) < 20, TRUE, FALSE),
      percentage = TRUE
    )
  
  ggplot2::ggsave(
    filename = file.path(output_path, "mv_plot.png"),
    plot = plot,
    width = 8,
    height = 6,
    dpi = 600
  )
  
  ggplot2::ggsave(
    filename = file.path(output_path, "mv_plot.pdf"),
    plot = plot,
    width = 8,
    height = 6,
    dpi = 600
  )
  
  ####in variables
  if (nrow(object) > 1000) {
    plot =
      massdataset::show_variable_missing_values(
        object = object[1:10000, ],
        order_by = "rt",
        show_x_text = ifelse(nrow(object) < 20, TRUE, FALSE),
        show_x_ticks = ifelse(nrow(object) < 20, TRUE, FALSE),
        percentage = TRUE
      ) +
      labs(title = "Only first 10000 variables.")
  } else{
    plot =
      massdataset::show_variable_missing_values(
        object = object,
        order_by = "rt",
        show_x_text = ifelse(nrow(object) < 20, TRUE, FALSE),
        show_x_ticks = ifelse(nrow(object) < 20, TRUE, FALSE),
        percentage = TRUE
      )
  }
  
  ggplot2::ggsave(
    filename = file.path(output_path, "variable_mv_plot.png"),
    plot = plot,
    width = 8,
    height = 6,
    dpi = 600
  )
  
  ggplot2::ggsave(
    filename = file.path(output_path, "variable_mv_plot.pdf"),
    plot = plot,
    width = 8,
    height = 6,
    dpi = 600
  )
  
  
  
  ####in samples
  plot =
    massdataset::show_sample_missing_values(
      object = object,
      color_by = "class",
      order_by = "injection.order",
      percentage = TRUE
    )
  
  ggplot2::ggsave(
    filename = file.path(output_path, "sample_mv_plot.png"),
    plot = plot,
    width = 8,
    height = 6,
    dpi = 600
  )
  
  ggplot2::ggsave(
    filename = file.path(output_path, "sample_mv_plot.pdf"),
    plot = plot,
    width = 8,
    height = 6,
    dpi = 600
  )
  
  ###RSD
  if (sum(object@sample_info$class == "QC") >= 3) {
    plot1 =
      massqc_rsd_plot(
        object = object %>%
          massdataset::activate_mass_dataset(what = "sample_info") %>%
          dplyr::filter(class == "QC"),
        order_by = "rt",
        show_x_text = ifelse(nrow(object) < 20, TRUE, FALSE),
        show_x_ticks = ifelse(nrow(object) < 20, TRUE, FALSE)
      ) +
      labs(title = "All QC samples")
    
    
    plot2 =
      massqc_cumulative_rsd_plot(
        object =  object %>%
          massdataset::activate_mass_dataset(what = "sample_info") %>%
          dplyr::filter(class == "QC"),
        rsd_cutoff = 30
      ) +
      labs(title = "All QC samples")
    
  } else{
    plot1 =
      massqc_rsd_plot(
        object = object %>%
          massdataset::activate_mass_dataset(what = "sample_info") %>%
          dplyr::filter(class == "Subject"),
        order_by = "rt",
        show_x_text = ifelse(nrow(object) < 20, TRUE, FALSE),
        show_x_ticks = ifelse(nrow(object) < 20, TRUE, FALSE)
      ) +
      labs(title = "All Subject samples")
    
    plot2 =
      massqc_cumulative_rsd_plot(
        object =  object %>%
          massdataset::activate_mass_dataset(what = "sample_info") %>%
          dplyr::filter(class == "Subject"),
        rsd_cutoff = 30
      ) +
      labs(title = "All Subject samples")
    
  }
  
  ggplot2::ggsave(
    filename = file.path(output_path, "variable_rsd.pdf"),
    plot = plot1,
    width = 8,
    height = 6,
    dpi = 600
  )
  
  ggplot2::ggsave(
    filename = file.path(output_path, "variable_cumulative_rsd.pdf"),
    plot = plot2,
    width = 8,
    height = 6,
    dpi = 600
  )
  
  plot = plot1 + plot2 + patchwork::plot_layout(nrow = 1)
  
  ggplot2::ggsave(
    filename = file.path(output_path, "rsd.png"),
    plot = plot,
    width = 12,
    height = 6,
    dpi = 600
  )
  
  
  
  ####sample box plot
  if (sum(object@sample_info$class == "QC") >= 3) {
    plot =
      massqc_sample_boxplot(
        object = object %>%
          massdataset::activate_mass_dataset(what = "sample_info") %>%
          dplyr::filter(class == "QC"),
        color_by = ifelse(any(
          colnames(object@sample_info) == "batch"
        ), "batch", "class")
      )
  } else{
    plot =
      massqc_sample_boxplot(object = object[, 1:30],
                            color_by = ifelse(any(
                              colnames(object@sample_info) == "batch"
                            ), "batch", "class")) +
      labs(title = ifelse(ncol(object) > 30, "First 30 samples", ""))
  }
  
  ggplot2::ggsave(
    filename = file.path(output_path, "sample_box.pdf"),
    plot = plot2,
    width = 8,
    height = 6,
    dpi = 600
  )
  
  ggplot2::ggsave(
    filename = file.path(output_path, "sample_box.png"),
    plot = plot,
    width = 8,
    height = 6,
    dpi = 600
  )
  
  
  
  ####sample correlation
  if (sum(object@sample_info$class == "QC") >= 3) {
    plot =
      massqc_sample_correlation(
        object = object %>%
          massdataset::activate_mass_dataset(what = "sample_info") %>%
          dplyr::filter(class == "QC")
      )
  } else{
    plot =
      massqc_sample_correlation(object = object[1:100]) +
      labs(title = ifelse(ncol(object) > 100, "First 100 samples", ""))
  }
  
  ggplot2::ggsave(
    filename = file.path(output_path, "sample_correlation.png"),
    plot = plot,
    width = 8,
    height = 6,
    dpi = 600
  )
  
  ggplot2::ggsave(
    filename = file.path(output_path, "sample_correlation.pdf"),
    plot = plot,
    width = 8,
    height = 6,
    dpi = 600
  )
  
  ###PCA
  plot =
    massqc_pca(
      object = scale(log(object)),
      color_by = ifelse(any(
        colnames(object@sample_info) == "batch"
      ),
      "batch", "class"),
      frame = TRUE
    )
  
  
  ggplot2::ggsave(
    filename = file.path(output_path, "pca.pdf"),
    plot = plot,
    width = 8,
    height = 6,
    dpi = 600
  )
  
  ggplot2::ggsave(
    filename = file.path(output_path, "pca.png"),
    plot = plot,
    width = 8,
    height = 6,
    dpi = 600
  )
  
  
  
  ##transform rmd to HTML or pdf
  if (type == "html" | type == "all") {
    rmarkdown::render(file.path(output_path, "massqc.template.Rmd"),
                      rmarkdown::html_document())
    file.rename(
      from = file.path(output_path, "massqc.template.html"),
      to = file.path(output_path, "massqc_report.html")
    )
  }
  
  
  ########render rmarkddown to html or pdf
  if (type == "pdf" | type == "all") {
    rmarkdown::render(file.path(output_path, "massqc.template.Rmd"),
                      rmarkdown::pdf_document())
    file.rename(
      from = file.path(output_path, "massqc.template.pdf"),
      to = file.path(output_path, "massqc_report.pdf")
    )
  }
  
  ####remove some files
  file = dir(output_path)
  remove_file = grep("png|Rmd|parameters|rda", file, value = TRUE)
  unlink(
    x = file.path(output_path, remove_file),
    recursive = TRUE,
    force = TRUE
  )
}