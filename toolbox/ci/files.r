#' @export
determine_files_to_keep <- function(local_dir_path = "/tmp/github.com-fdrennan-bmrn-test.git/",
                                    files_include = c("renv", ".git"),
                                    exts_include = c("r", "R")) {
  {
    box::use(dplyr)
    box::use(stringr)
    box::use(fs)
    box::use(purrr)
    box::use(glue = glue[glue, glue_collapse])
    box::use(cli)
    box::use(crayon)
    box::use(urltools)
    box::use(../utilities/strings)
  }


  files <- fs$dir_info(local_dir_path, all = TRUE, recurse = TRUE, type = "file")


  
  files <-
    files |>
    dplyr$filter(!stringr$str_detect(path, '(.+/[.]git)|(.+/renv/)|(.+/[.]idea)')) |> 
    dplyr$transmute(
      path, type, birth_time
    ) |>
    dplyr$mutate(
      path_file = fs$path_file(path),
      path_dir = fs$path_dir(path),
      ext = fs$path_ext(path),
      local_path = fs$path_rel(path, local_dir_path),
      keep_by_choice = strings$str_detect_any(local_path, files_include),
      keep_by_ext = ext %in% exts_include,
      delete = !(keep_by_choice | keep_by_ext)
    )
  
  fs$file_delete(files[files$delete,]$path)
  dirs <- fs$dir_info(path = local_dir_path, recurse = T, type = 'directory')
  fs$dir_delete(dirs[purrr$map_dbl(dirs$path, function(x) length(fs$dir_ls(x)))==0, ]$path)
  
  files <- files[!files$delete,]

  files <- dplyr$transmute(files, from = fs$path_rel(path, start = local_dir_path), to = file.path("/root", from))

  files
}
