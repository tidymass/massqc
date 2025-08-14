#' @title massqc_logo
#' @description massqc_logo
#' @author Xiaotao Shen
#' \email{shenxt@@stanford.edu}
#' @importFrom crayon yellow red green bold bgRed
#' @import ggplot2
#' @importFrom dplyr filter select pull everything distinct
#' @importFrom dplyr one_of left_join mutate bind_cols arrange
#' @importFrom tibble as_tibble enframe tibble rownames_to_column
#' @importFrom cli rule col_cyan tree
#' @importFrom utils packageVersion object.size write.csv tail head
#' @importFrom purrr map map2
#' @importFrom masstools get_os
#' @importFrom magrittr %>%
#' @importFrom patchwork plot_layout
#' @importFrom ggcorrplot ggcorrplot
#' @importFrom grid gpar
#' @import patchwork
#' @import ggfortify
#' @importFrom stats coefficients lm loess median predict ecdf
#' @importFrom stats rgamma rt sd cor p.adjust prcomp t.test wilcox.test
#' @importFrom ggrepel geom_text_repel geom_label_repel
#' @importFrom massdataset parse_tidymass_parameter get_mv_number activate_mass_dataset mutate_rsd
#' @importFrom massdataset show_missing_values show_sample_missing_values show_variable_missing_values
#' @importFrom grDevices grey
#' @importFrom tidyr pivot_longer
#' @importFrom methods slot
#' @export
#' @return logo
#' @examples
#' massqc_logo()

massqc_logo <- function() {
  message("Thank you for using massqc!")
  message("Version", massqc_version, "(", update_date, ')')
  message("More information: massqc.tidymass.org")
  cat(
    c(
      "                           ____   _____ ",
      "                          / __ \\ / ____|",
      "  _ __ ___   __ _ ___ ___| |  | | |     ",
      " | '_ ` _ \\ / _` / __/ __| |  | | |     ",
      " | | | | | | (_| \\__ \\__ \\ |__| | |____ ",
      " |_| |_| |_|\\__,_|___/___/\\___\\_\\\\_____|",
      "                                        ",
      "                                        "
    ),
    sep = "\n"
  )
}

massqc_version <-
  as.character(utils::packageVersion(pkg = "massqc"))
update_date <-
  as.character(Sys.time())

#' # library(cowsay)
#' #https://onlineasciitools.com/convert-text-to-ascii-art
#' # writeLines(capture.output(say("Hello"), type = "message"), con = "ascii_art.txt")
#' # art <- readLines("logo.txt")
#' # dput(art)
