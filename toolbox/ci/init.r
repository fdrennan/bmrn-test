{
  box::use(. / toolbox / ci / build[build_pipeline])
  # box::use(. / build[build_pipeline])
}

results <- build_pipeline(
  app_dir = ".",
  repository = "git@github.com:fdrennan/bmrn-test.git",
  seed = NULL,
  destroy = FALSE,
  # branch = "ndexr-27",
  branch = 'ndexr-27',
  toolbox = TRUE,
  build_container = TRUE,
  app_tag = "ndextestca",
  randomize_tag = FALSE,
  push = TRUE,
  command = "R -e \"shiny::runApp('/root', port=8000, host='0.0.0.0')\"",
  expose = 8000,
  copy_all = TRUE,
  files_include = c(
     'Dockerfile'
    # '.Rprofile', '.Rbuildignore'
  ),
  exts_include = c("r",'R', "lock", "js", "css", "sass", "Rmd", "png", "jpg", "svg"),
  notify = FALSE,
  docker_add_renv = FALSE
)
