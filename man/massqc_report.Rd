% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/massqc_report.R
\name{massqc_report}
\alias{massqc_report}
\title{Report data quality}
\usage{
massqc_report(object, path = ".", type = c("html", "pdf", "all"))
}
\arguments{
\item{object}{tidymass-class object.}

\item{path}{path}

\item{type}{type html, pdf or all.}
}
\value{
reporting result
}
\description{
Report data quality
}
\examples{
\dontrun{
library(massdataset)
data("expression_data")
data("sample_info")
data("sample_info_note")
data("variable_info")
data("variable_info_note")
object =
  create_mass_dataset(
    expression_data = expression_data,
    sample_info = sample_info,
    variable_info = variable_info,
    sample_info_note = sample_info_note,
    variable_info_note = variable_info_note
  )
object
massqc_report(object, path = "demo_data")
}
}
\author{
Xiaotao Shen
\email{shenxt1990@163.com}
}
