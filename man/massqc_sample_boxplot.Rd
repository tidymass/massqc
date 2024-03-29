% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/massqc_sample_boxplot.R
\name{massqc_sample_boxplot}
\alias{massqc_sample_boxplot}
\title{massqc_sample_boxplot}
\usage{
massqc_sample_boxplot(
  object,
  color_by,
  fill_by,
  order_by = "sample_id",
  outlier.shape = NA,
  point = FALSE,
  point_alpha = 0.8
)
}
\arguments{
\item{object}{tidymass-class object.}

\item{color_by}{which column (sample_info) is used to color samples}

\item{fill_by}{which column (sample_info) is used to fill samples}

\item{order_by}{which column (sample_info) is used to order samples}

\item{outlier.shape}{outlier.shape from ggplot2.}

\item{point}{point or not.}

\item{point_alpha}{point_alpha}
}
\value{
ggplot2 plot.
}
\description{
Boxplot for each samples.
}
\examples{
library(massdataset)
library(ggplot2)
data("expression_data")
data("sample_info")
data("variable_info")

object =
  create_mass_dataset(
    expression_data = expression_data,
    sample_info = sample_info,
    variable_info = variable_info
  )

object \%>\%
  massqc_sample_boxplot()

object \%>\%
  log(10) \%>\%
  massqc_sample_boxplot()

object \%>\%
  log(10) \%>\%
  massqc_sample_boxplot(color_by = "class")

object \%>\%
  log(10) \%>\%
  massqc_sample_boxplot(fill_by = "class") +
  ggsci::scale_fill_lancet()

object \%>\%
  log(10) \%>\%
  massqc_sample_boxplot(
    fill_by = "class",
    color_by = "class",
    point = TRUE,
    point_alpha = 0.3
  ) +
  ggsci::scale_fill_lancet()

object \%>\%
  log(10) \%>\%
  massqc_sample_boxplot(color_by = "class",
                 point = TRUE,
                 point_alpha = 0.3) +
  ggsci::scale_color_lancet()
}
\author{
Xiaotao Shen
\email{shenxt1990@163.com}
}
