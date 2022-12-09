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
                           # files=NULL,
                           regexp,
                           command, notify,
                           expose) {
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
    box::use(purrr)
  }

  url <- urltools$url_parse(repository)

  if (is.numeric(seed)) set.seed(seed)

  project_dir <- file.path("/tmp", with(url, glue("{domain}-{port}-{path}")))

  if (!dir.exists(project_dir)) {
    cli$cli_alert_info("Cloning {repository} to {project_dir}")
    gert$git_clone(
      url = repository,
      path = project_dir,
      bare = FALSE,
      branch = branch
    )
  } else {
    
    current_branch <- gert$git_branch(project_dir)
    local_paths <- gert$git_branch_list(TRUE, project_dir)
    
    current_existing_branches <- local_paths$name[stringr$str_detect(local_paths$name, glue("^{app_tag}"))]
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
    cli$cli_alert_info("Checking out {app_tag}")
    gert$git_branch_create(app_tag, repo = project_dir, ref = branch, checkout = TRUE)
  }

  app_dir <- file.path(project_dir, app_dir)
  cli$cli_alert_info("Building Dockerfile for {app_dir} from branch {branch}")


  files <- fs$dir_info(project_dir, recurse = T, regexp = regexp)
  files <- dplyr$transmute(files, from = fs$path_rel(path, start = project_dir), to = file.path("/root", from))


  dockerfile$dockerfile_build(
    from = "ubuntu:jammy", workdir = "/root", app_dir = app_dir,
    expose = expose,
    author = "Freddy Drennan",
    email = "fdrennan@ndexr.com",
    files = files,
    copy_all = copy_all
  )
  container_clean_name <- glue("cd {app_dir} && docker build -t {app_tag} .")

  if (build_container) {
    system(container_clean_name)
    cli$cli_alert_success(container_clean_name)
  }

  docker_images <- dockerfile$docker_get_images(app_tag)

  if (destroy) {
    cli$cli_alert_info("Deleting images with tag {app_tag}")
    dockerfile$docker_delete_images(app_tag)
  }

  cli$cli_alert_info("Attaching files in {app_dir}")


  if (toolbox) {
    cli$cli_alert_info("Using toolbox code to project.")
    fs$dir_copy("~/root/toolbox", file.path(project_dir, "toolbox"), overwrite = TRUE)
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

  if (store_s3) {
    box::use(.. / aws / client)
    box::use(.. / aws / s3_create_bucket)
    box::use(purrr)
    s3 <- client$client("s3")
    bucketname <- "ndexrpipelines"
    bucketObjects <- unlist(s3$list_objects(Bucket = bucketname), recursive = TRUE, use.names = TRUE)
    unlist(bucketObjects[names(bucketObjects) == "Contents.Key"])
    buckets <- purrr$map_chr(s3$list_buckets()$Buckets, function(x) x$Name)
    if (!bucketname %in% buckets) {
      s3_create_bucket$s3_create_bucket(s3, bucketname)
    }

    cli$cli_alert_info("Pushing to {bucketname}")

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
  container_run_command <- glue("docker run -it {app_tag} zsh")
  payload <- list(
    container_run_command = container_run_command,
    docker_images = docker_images,
    # get_data_here = glue("https://ndexrpipelines.s3.us-east-2.amazonaws.com/attachments.tar.gz"),
    container_clean_name = glue("docker build -t {app_tag} .")
  )

  if (push) {
    system(glue("cd {project_dir} && git add --all"))
    system(glue('cd {project_dir} && git commit -m "ndexr update" '))
    gert$git_push(repo = project_dir, remote = "origin")
  }

  cli$cli_alert_info(container_run_command)


  if (notify) {
    tryCatch(
      {
        send_email$send_email(subject, payload)
      },
      error = function(err) {
        send_email$send_email(subject, payload)
      }
    )
  }
  cli::cli_code(
    paste0(
      c(
        "\nCommand to build\n",
        "docker run -v ~/.aws:/root/.aws",
        "-v ~/.cache/gargle:/root/.cache/gargle",
        "-v ~/.credentials.json:/root/.credentials.json",
        "-p 8000:8000",
        glue("-it {app_tag} {command}")
      ),
      collapse = " "
    )
  )
  cli$cli_alert_success("init_project complete")
  docker_images
}
