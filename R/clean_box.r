# clean_box <- function() {
#   box::use(stringr)
#   box::use(readr)
#   box::use(fs)
#   
#   files <- fs$dir_ls('./R', type = 'file', regexp = '.r$')
#   files <- './R/app.R'
#   
#   code <- readLines(files)
#   
#   stringr$str_detect(
#     code, 'box::'
#   )
# }
