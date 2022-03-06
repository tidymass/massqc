#' @title Report data quality
#' @description Report data quality
#' @author Xiaotao Shen
#' \email{shenxt1990@@163.com}
#' @param object tidymass-class object.
#' @param path path
#' @param type type html, pdf or all.
#' @return reporting result
#' @export
#'@examples
#'\dontrun{
#' library(massdataset)
#' data("expression_data")
#' data("sample_info")
#' data("sample_info_note")
#' data("variable_info")
#' data("variable_info_note")
#' object =
#'   create_mass_dataset(
#'     expression_data = expression_data,
#'     sample_info = sample_info,
#'     variable_info = variable_info,
#'     sample_info_note = sample_info_note,
#'     variable_info_note = variable_info_note
#'   )
#' object
#' massqc_report(object, path = "demo_data")
#'}

massqc_report <-
  function(object,
           path = ".",
           type = c("html", "pdf", "all")) {
    dir.create(path, showWarnings = FALSE)
    type = match.arg(type)
    if(!is(object = object, class2 = "mass_dataset")){
      stop("obejct should be mass_dataset class.\n")
    }
    options(warn = -1)
    ###path
    if (length(grep("Report", dir(path))) > 0) {
      output_path = file.path(path, paste('Report', length(grep(
        "Report", dir(path)
      )) + 1, sep = "_"))
    } else{
      output_path = file.path(path, "Report")
    }
    
    # dir.create(output_path, showWarnings = FALSE)
    
    ####get the template
    cat("Get report template.\n")
    rmarkdown::draft(
      file = output_path,
      template = "massqc",
      package = "massqc",
      create_dir = TRUE,
      edit = FALSE
    )
    
    ###parameters
    cat("Parameters.\n")
    parameters <-
      object@process_info %>%
      lapply(function(x) {
        if (length(x) == 1) {
          parse_tidymass_parameter(object = x)
        } else{
          x %>%
            lapply(function(y) {
              massdataset::parse_tidymass_parameter(object = y)
            }) %>%
            dplyr::bind_rows()
        }
      }) %>%
      dplyr::bind_rows() %>%
      dplyr::arrange(time)
    
    save(parameters, file = file.path(output_path, "parameters.rda"))
    
    ####sample information
    cat("Sample infromation.\n")
    save(object, file = file.path(output_path, "object.rda"))
    
    if (nrow(object) >= 1000) {
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
    cat("Missing values in dataset.\n")
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
    cat("Missing values in all variables.\n")
    if (nrow(object) > 10000) {
      plot <-
        massdataset::show_variable_missing_values(
          object = object[seq_len(10000), ],
          order_by = "rt",
          show_x_text = ifelse(nrow(object) < 20, TRUE, FALSE),
          show_x_ticks = ifelse(nrow(object) < 20, TRUE, FALSE),
          percentage = TRUE
        ) +
        labs(title = "Only first 10000 variables.")
    } else{
      plot <-
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
    cat("Missing values in all samples.\n")
    plot <-
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
    cat("RSD for variables.\n")
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
      plot1 <-
        massqc_rsd_plot(
          object = object %>%
            massdataset::activate_mass_dataset(what = "sample_info") %>%
            dplyr::filter(class == "Subject"),
          order_by = "rt",
          show_x_text = ifelse(nrow(object) < 20, TRUE, FALSE),
          show_x_ticks = ifelse(nrow(object) < 20, TRUE, FALSE)
        ) +
        labs(title = "All Subject samples")
      
      plot2 <-
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
    cat("Intensity for all the variables in all samples.\n")
    if (sum(object@sample_info$class == "QC") >= 3) {
      plot <-
        massqc_sample_boxplot(
          object = log(object) %>%
            massdataset::activate_mass_dataset(what = "sample_info") %>%
            dplyr::filter(class == "QC"),
          color_by = ifelse(any(
            colnames(object@sample_info) == "batch"
          ), "batch", "class")
        )
    } else{
      plot <-
        massqc_sample_boxplot(object = log(object)[, seq_len(30)],
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
    cat("Sample correlation.\n")
    
    if (sum(object@sample_info$class == "QC") >= 3) {
      plot <-
        massqc_sample_correlation(
          object = object %>%
            massdataset::activate_mass_dataset(what = "sample_info") %>%
            dplyr::filter(class == "QC"),
          method = "square"
        )
    } else{
      plot <-
        massqc_sample_correlation(object = object[seq_len(100)],
                                  method = "square") +
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
    cat("PCA score plot.\n")
    plot <-
      object %>%
      `+`(1) %>%
      log(2) %>%
      massqc_pca(
        color_by = ifelse(any(
          colnames(object@sample_info) == "batch"
        ),
        "batch", "class"),
        frame = TRUE,
        line = FALSE
      )
    cat("PCA done.\n")
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
    
    cat("Render report.\n")
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
    cat("Remove some files.\n")
    file = dir(output_path)
    remove_file = grep("png|Rmd|parameters|rda", file, value = TRUE)
    unlink(
      x = file.path(output_path, remove_file),
      recursive = TRUE,
      force = TRUE
    )
  }
