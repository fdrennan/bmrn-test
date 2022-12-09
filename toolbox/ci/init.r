
# rstudioapi::restartSession()

box::use(. / toolbox / ci / build[build_pipeline])

# results <- build_pipeline(
#   app_dir = "./.",
#   repository = "git@gitlab.com:fdrennan/root.git",
#   seed = NULL,
#   toolbox = TRUE,
#   destroy = FALSE,
#   branch = "main"
# )
#
# debug(build_pipeline)


results <- build_pipeline(
  app_dir = ".",
  repository = "git@github.com:fdrennan/bmrn-test.git",
  seed = NULL,
  destroy = FALSE,
  branch = "ndexr-27",
  toolbox = TRUE,
  build_container = FALSE,
  app_tag = "ndextest",
  randomize_tag = FALSE,
  push = TRUE,
  command = "R -e \"shiny::runApp('/root', port=8000, host='0.0.0.0')\"",
  expose = 8000,
  copy_all = FALSE,
  regexp = '([.]r$)|(functions/*[.]R$)',
  notify = FALSE
  # files = fs::dir_info('/tmp/github.com-fdrennan-bmrn-test.git/app.R')
)
# 
# results <- build_pipeline(
#   app_dir = ".",
#   repository = "git@github.com:fdrennan/shiny_docker.git",
#   seed = NULL,
#   destroy = FALSE,
#   branch = "master",
#   toolbox = TRUE,
#   build_container = TRUE,
#   app_tag = 'ndexrtesting',
#   randomize_tag = TRUE,
#   push = FALSE,
#   command = "R -e \"shiny::runApp('/root', port=8000, host='0.0.0.0')\"",
#   expose = 8000,
#   copy_all = FALSE,
#   files = NULL
# )

# results <- build_pipeline(
#   app_dir = "inst/examples/supported_shinyWidgets_app",
#   repository = "https://github.com/merlinoa/shinyFeedback.git",
#   seed = NULL,
#   destroy = FALSE,
#   branch = "master",
#   toolbox = TRUE,
#   build_container = TRUE,
#   app_tag = paste0("127", paste0(sample(1:10, 4), collapse = '')),
#   randomize_tag = TRUE,
#   push = FALSE,
#   command = "R -e \"shiny::runApp('/root', port=8000, host='0.0.0.0')\"",
#   expose = 8000
# )


# results <- build_pipeline(
#   app_dir = "inst/examples/supported_shinyWidgets_app",
#   repository = "https://github.com/merlinoa/shinyFeedback.git",
#   seed = NULL,
#   destroy = FALSE,
#   branch = "master",
#   toolbox = TRUE,
#   build_container = TRUE,
#   app_tag = paste0("127", paste0(sample(1:10, 4), collapse = '')),
#   randomize_tag = TRUE,
#   push = FALSE,
#   command = "R -e \"shiny::runApp('/root', port=8000, host='0.0.0.0')\""
# )

# results <- build_pipeline(
#   app_dir = ".",
#   repository = "https://github.com/mayank7jan/live-Knit-Rmd.git",
#   seed = NULL,
#   destroy = FALSE,
#   branch = "main",
#   toolbox = TRUE,
#   build_container = TRUE,
#   app_tag = paste0("liveknit", paste0(sample(1:10, 4), collapse = "")),
#   randomize_tag = TRUE,
#   push = FALSE,
#   command = "R -e \"shiny::runApp('/root', port=8000, host='0.0.0.0')\"",
#   expose = 8000
# )

# system('docker container prune')
# system('docker prune prune')

# docker run -v ~/.aws:/root/.aws -v ~/.cache/gargle:/root/.cache/gargle -v ~/.credentials.json:/root/.credentials.json -p 8000:8000 -it ndt1-1670444435 R -e "shiny::runApp('/root', port=8000, host='0.0.0.0')"
# docker run -v ~/.aws:/root/.aws -v ~/.cache/gargle:/root/.cache/gargle -v ~/.credentials.json:/root/.credentials.json -p 9045:8000 -it registry.gitlab.com/fdrennan/root R -e "source('./toolbox/templates/app/app.r')"
