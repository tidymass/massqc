#' @title massqc_logo
#' @description massqc_logo
#' @author Xiaotao Shen
#' \email{shenxt@@stanford.edu}
#' @importFrom Biobase featureData
#' @importFrom crayon yellow red green bold bgRed
#' @import ggplot2
#' @importFrom pbapply pblapply pboptions
#' @importFrom stringr str_split str_replace_all str_trim str_detect str_extract
#' @importFrom dplyr filter select pull everything distinct one_of left_join mutate bind_cols arrange
#' @importFrom tibble as_tibble enframe tibble rownames_to_column
#' @importFrom clisymbols symbol
#' @importFrom cli rule col_cyan tree
#' @importFrom utils packageVersion object.size write.csv tail head
#' @importFrom purrr map map2
#' @importFrom plyr dlply .
#' @importFrom RColorBrewer brewer.pal
#' @importFrom readr read_csv cols
#' @importFrom readxl read_excel
#' @importFrom tinytools get_os mz_rt_match
#' @importFrom BiocParallel MulticoreParam SnowParam bplapply
#' @importFrom magrittr %>%
#' @importFrom plotly ggplotly
#' @importFrom BiocGenerics basename
#' @importFrom patchwork plot_layout
#' @importFrom ggcorrplot ggcorrplot
#' @import patchwork
#' @import ggfortify
#' @importFrom stats coefficients lm loess median predict ecdf
#' @importFrom stats rgamma rt sd cor p.adjust prcomp t.test wilcox.test
#' @importFrom ggrepel geom_text_repel geom_label_repel
#' @export
#' @examples 
#' massqc_logo()

massqc_logo <- function(){
  cat(crayon::green("Thank you for using massqc_logo!\n"))
  cat(crayon::green("Version 0.0.1 (2021-11-30)\n"))
  cat(crayon::green("Bug fixing\n"))
  cat(crayon::green("More information can be found at https://tidymass.github.io/massqc_logo/\n"))
  cat(crayon::green(
    c("                           ____   _____ ", "                          / __ \\ / ____|", 
      "  _ __ ___   __ _ ___ ___| |  | | |     ", " | '_ ` _ \\ / _` / __/ __| |  | | |     ", 
      " | | | | | | (_| \\__ \\__ \\ |__| | |____ ", " |_| |_| |_|\\__,_|___/___/\\___\\_\\\\_____|", 
      "                                        ", "                                        "
    )
    
  ), sep = "\n")
}


#' # library(cowsay)
#' #https://onlineasciitools.com/convert-text-to-ascii-art
#' # writeLines(capture.output(say("Hello"), type = "message"), con = "ascii_art.txt")
#' # art <- readLines("logo.txt")
#' # dput(art)
