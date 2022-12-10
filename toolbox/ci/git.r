#' @export
checkout_new_branch_and_make_pipeline_tag <- function(project_dir, branch, app_tag) {{
  box::use(gert)
  box::use(cli)
  box::use(stringr)
  box::use(glue[glue, glue_col])
  box::use(purrr)
  box::use(crayon)
}
if (!dir.exists(project_dir)) {
  cli$cli_alert_info("Cloning {repository} into {project_dir}")
  gert$git_clone(
    url = repository,
    path = project_dir,
    bare = FALSE,
    branch = branch
  )
} else {
  current_branch <- gert$git_branch(project_dir)
  local_branch <- gert$git_branch_list(TRUE, project_dir)
  current_existing_branches <- local_branch$name[stringr$str_detect(local_branch$name, glue("^{app_tag}"))]
  cli$cli_alert_info("{project_dir} reset --hard")
  gert$git_reset_hard(repo = project_dir)
  if (all(is.na(current_existing_branches))) current_existing_branches <- 0
  print(length(current_existing_branches))
  if (any(stringr$str_detect(unlist(current_existing_branches), "-1$"))) {
    current_existing_branches <- stringr$str_split(current_existing_branches, "-")
    current_existing_branches <- purrr$map_int(current_existing_branches, function(x) tryCatch(as.integer(x[[2]]), error = NA_integer_))
    tag_number <- max(current_existing_branches, na.rm = T) + 1
    app_tag <- glue("{app_tag}-{tag_number}")
  } else {
    app_tag <- glue("{app_tag}-1")
  }
  cli$cli_alert_info("{crayon::green} Checking out {app_tag}")
  gert$git_branch_create(app_tag, repo = project_dir, ref = branch, checkout = TRUE)
  app_tag
}}
