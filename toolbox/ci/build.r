#' @export
init_project <- function(local_repo_path, app_dir, repository, branch, app_tag,
                         randomize_tag = TRUE,
                         build_container = TRUE,
                         destroy = TRUE,
                         toolbox = TRUE,
                         push = FALSE,
                         expose = 8000,
                         store_s3 = FALSE,
                         copy_all=NULL,
                         regexp=NULL,
                         notify=FALSE,
                         command = "R -e shiny::runApp('/root/app.R', port=8000, host='0.0.0.0')") {
  {
    box::use(. / dockerfile)
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

  if (!dir.exists(local_repo_path)) {
    cli$cli_alert_info("Cloning {repository} to {local_repo_path}")
    gert$git_clone(
      url = repository,
      path = local_repo_path,
      bare = FALSE,
      branch = branch
    )
  } else {
    current_branch <- gert$git_branch(local_repo_path)
    local_paths <- gert$git_branch_list(TRUE, local_repo_path)
    
    
    print(local_paths$name)
    
    current_existing_branches <- local_paths$name[stringr$str_detect(local_paths$name, glue("^{app_tag}"))]
    gert$git_reset_hard(repo = local_repo_path)
    if (all(is.na(current_existing_branches))) current_existing_branches <- 0
    # print(current_existing_branches)
    print(length(current_existing_branches))
    
    if (any(stringr$str_detect(unlist(current_existing_branches), '-1$'))) {
      current_existing_branches <- stringr$str_split(current_existing_branches, "-")
      # print(current_existing_branches)
      current_existing_branches <- purrr$map_int(current_existing_branches, function(x) tryCatch(as.integer(x[[2]]), error = NA_integer_))
      # print(current_existing_branches)
      tag_number <- max(current_existing_branches, na.rm = T) + 1
      # print(tag_number)
      app_tag <- glue("{app_tag}-{tag_number}")
    } else {
      app_tag <- glue("{app_tag}-1")
    }
    cli$cli_alert_info("Checking out {app_tag}")
    gert$git_branch_create(app_tag, repo = local_repo_path, ref = branch, checkout = TRUE)
  }

  app_dir <- file.path(local_repo_path, app_dir)
  cli$cli_alert_info("Building Dockerfile for {app_dir} from branch {branch}")

  
  files <- fs$dir_info(local_repo_path, recurse = T, regexp = regexp)
  files <- dplyr$transmute(files, from= fs$path_rel(path, start = local_repo_path),  to = file.path('/root', from))
  
  
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
    fs$dir_copy("~/root/toolbox", file.path(local_repo_path, "toolbox"), overwrite = TRUE)
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
    system(glue("cd {local_repo_path} && git add --all"))
    system(glue('cd {local_repo_path} && git commit -m "ndexr update" '))
    gert$git_push(repo = local_repo_path, remote = "origin")
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
    box::use(.. / utilities / send_email)
    box::use(glue[glue])
  }

  url <- urltools$url_parse(repository)


  if (is.numeric(seed)) set.seed(seed)

  project_dir <- file.path("/tmp", with(url, glue("{domain}-{port}-{path}")))

  build$init_project(
    repository = repository,
    app_dir = app_dir,
    app_tag = app_tag,
    randomize_tag = randomize_tag,
    local_repo_path = project_dir,
    build_container = build_container,
    destroy = destroy,
    toolbox = toolbox,
    branch = branch,
    push = push,
    command = command,
    expose = expose,
    copy_all=copy_all,
    regexp=regexp,
    notify=notify
  )
}
