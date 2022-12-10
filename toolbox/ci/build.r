#' @export
build_pipeline <- function(repository = "git@gitlab.com:fdrennan/root.git",
                           app_dir = "toolbox/templates/app-basic",
                           app_tag = "develdestroy",
                           branch = "main",
                           destroy = FALSE,
                           seed = 4325,
                           toolbox = FALSE,
                           build_container = FALSE,
                           randomize_tag = TRUE,
                           store_s3 = FALSE,
                           push = FALSE,
                           copy_all,
                           bucketname = "ndexrpipelines",
                           files,
                           command, notify,
                           expose,
                           docker_add_renv,
                           files_include = c("renv", "./.git", "toolbox"),
                           exts_include = c("r", "R")) {
  {
    box::use(. / build)
    box::use(. / dockerfile)
    box::use(urltools)
    box::use(renv)
    box::use(gert)
    box::use(dplyr)
    box::use(stringr)
    box::use(fs)
    box::use(reticulate)
    box::use(cli)
    box::use(glue[glue])
    box::use(readr)
    box::use(.. / utilities / send_email)
    box::use(utils)
    box::use(. / git)
    box::use(purrr)
    box::use(. / files)
    box::use(snakecase)
  }

  url <- urltools$url_parse(repository)

  if (is.numeric(seed)) set.seed(seed)
  # url$path <- sub('[]git', '', url$path)
  project_dir <- file.path("/tmp", with(url, glue("{domain}-{port}-{path}")))


  app_tag <- git$checkout_new_branch_and_make_pipeline_tag(project_dir, branch, app_tag)
  app_dir <- file.path(project_dir, app_dir)


  files <- files$determine_files_to_keep(
    local_dir_path = project_dir,
    files_include = files_include,
    exts_include = exts_include
  )

  docker_images <- dockerfile$docker_get_images(app_tag)
  
  dockerfile$dockerfile_build(
    from = "ubuntu:jammy",
    workdir = "/root",
    app_dir = app_dir,
    expose = expose,
    author = "Freddy Drennan",
    email = "fdrennan@ndexr.com",
    files = files,
    copy_all = copy_all,
    docker_add_renv = docker_add_renv
  )


  if (build_container) {
    container_clean_name <- glue("cd {app_dir} && docker build -t {app_tag} .")
    system(container_clean_name)
  }


  if (destroy) {
    cli$cli_alert_info("Deleting images with tag {app_tag}")
    dockerfile$docker_delete_images(app_tag)
  }


  if (toolbox) {
    cli$cli_alert_info("Copying current toolbox into {project_dir}")
    fs$dir_copy("~/root/toolbox", file.path(project_dir, "toolbox"), overwrite = TRUE)
  }


  if (store_s3) {
    box::use(.. / aws / client)
    box::use(.. / aws / s3_create_bucket)
    box::use(purrr)
    s3 <- client$client("s3")


    cli$cli_alert_info("Pushing to {bucketname}")
    bucketObjects <- unlist(s3$list_objects(Bucket = bucketname), recursive = TRUE, use.names = TRUE)
    unlist(bucketObjects[names(bucketObjects) == "Contents.Key"])
    buckets <- purrr$map_chr(s3$list_buckets()$Buckets, function(x) x$Name)
    if (!bucketname %in% buckets) {
      s3_create_bucket$s3_create_bucket(s3, bucketname)
    }

    attachments <- fs$dir_ls(app_dir,
      type = "file",
      invert = TRUE,
      regexp = "^[.]git",
      recurse = TRUE,
      all = TRUE
    )

    if (file.exists("attachments.tar.gz")) file.remove("attachments.tar.gz")

    utils$tar("attachments.tar.gz", attachments)

    s3$upload_file(
      Filename = "attachments.tar.gz",
      Bucket = bucketname,
      Key = "attachments.tar.gz",
      ExtraArgs = list(ACL = "public-read")
    )
  }

  subject <- glue(
    "{app_tag} {repository} {app_dir} destroy {destroy}"
  )


  if (push) {
    purrr$walk(
      c(glue("cd {project_dir} && git add --all"),
        glue('cd {project_dir} && git commit -m "ndexr update" ')),
      function(x) {
        cli$cli_code(x)
        system(x, show.output.on.console = FALSE)
      }
    )
    gert$git_push(repo = project_dir, remote = "origin")
  }


  if (notify) {
    send_email$send_email(subject, payload)
  }

  {
    box::use(crayon)
    box::use(glue)
    cli$cli_alert_success(glue$glue_col("{crayon::magenta} Run Shiny"))

    cli$cli_alert(
      glue$glue_collapse(
        c(
          "{crayon::white} docker run -v ~/.aws:/root/.aws",
          "-v ~/.cache/gargle:/root/.cache/gargle",
          "-v ~/.credentials.json:/root/.credentials.json",
          "-p 8000:8000",
          "-it {app_tag} {command}"
        ),
        sep = " "
      )
    )

    cli$cli_alert_success("{crayon::magenta} Run zsh")
    cli$cli_alert(
      glue$glue_collapse(
        c(
          "{crayon::white}",
          "docker run -v ~/.aws:/root/.aws",
          "-v ~/.cache/gargle:/root/.cache/gargle",
          "-v ~/.credentials.json:/root/.credentials.json",
          "-p 8000:8000",
          "-it {app_tag} zsh"
        ),
        sep = " "
      )
    )
  }

  cli$cli_alert_success("init_project complete")
  docker_images
}
