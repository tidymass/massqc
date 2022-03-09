data("expression_data")
data("sample_info")
data("variable_info")
library(massqc)

object =
  create_mass_dataset(
    expression_data = expression_data,
    sample_info = sample_info,
    variable_info = variable_info
  )

result <- 
object %>%
  massqc_pca()

test_that("massqc_pca", {
  testthat::expect_s3_class(result, "gg")
})
