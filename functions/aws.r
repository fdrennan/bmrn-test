#' @export
list_buckets <- function() {
  box::use(utils[capture.output])
  box::use(sys)
  box::use(unix)
  box::use(jsonlite[toJSON, flatten])
  box::use(fpeek[peek_count_lines])
  box::use(uuid[UUIDgenerate])
  aws_path <- getOption("aws_path", "/usr/local/aws-cli/v2/current/bin/aws")
  params <- "--output json"
  user_info <- unix$user_info()
  std_out <- function(x) {
    writeLines(rawToChar(x), )
  }
  iam_users <- sys$exec_wait(aws_path, c("iam", "list-users", params), std_out = std_out)
  s3_ls <- capture.output(sys$exec_wait(aws_path, c("s3", "ls"), std_out = std_out))
  s3_ls <- s3_ls[nchar(s3_ls) > 0]
  # print(s3_ls)
  out <- list(
    id = UUIDgenerate(),
    CURRENT_TIME = as.character(Sys.time()),
    user_info = user_info,
    iam_users = iam_users,
    s3_ls = s3_ls
  )
  out <- toJSON(out, pretty = TRUE, auto_unbox = T)
  if (!dir.exists("logs/aws")) {
    dir.create("logs/aws", recursive = TRUE)
  }
  if (file.exists("logs/aws/dashboard.txt")) {
    n_lines <- peek_count_lines("logs/aws/dashboard.txt")
    if (n_lines > 3000) {
      file.remove("logs/aws/dashboard.txt")
    }
  }
  write(out, file = "logs/aws/dashboard.txt", append = TRUE)
  cat(out)
  out
}

#' @export
ec2_instance_create <- function(ImageId = "ami-097a2df4ac947655f",
                                InstanceType = "t2.xlarge",
                                min = 1,
                                max = 1,
                                KeyName = Sys.getenv("AWS_PEM"),
                                SecurityGroupId = Sys.getenv("AWS_SECURITY_GROUP"),
                                InstanceStorage = 10,
                                DeviceName = "/dev/sda1",
                                user_data = NA) {
  box::use(paws[ec2])
  box::use(readr[read_file])
  box::use(. / state / updateState[updateState])
  box::use(base64[encode])
  # browser()
  base_64_user_data <- read_file(encode("./shell/install_docker_ec2.sh"))
  ec2 <- ec2()
  response <-
    ec2$run_instances(
      ImageId = ImageId,
      InstanceType = InstanceType,
      MinCount = as.integer(min),
      MaxCount = as.integer(max),
      UserData = base_64_user_data,
      KeyName = KeyName,
      SecurityGroupIds = list(SecurityGroupId),
      BlockDeviceMappings = list(
        list(
          Ebs = list(
            VolumeSize = as.integer(InstanceStorage)
          ),
          DeviceName = DeviceName
        )
      )
    )
  instanceData <- list(response$Instances[[1]])
  names(instanceData) <- response$Instances[[1]]$InstanceId
  updateState(instanceData, "aws-ec2")
  # a$terminate_instances(response$Instances[[1]]$InstanceId)
  response
}

#' @export
ec2_instance_destroy <- function() {
  box::use(. / connections / storr)
  box::use(paws[ec2])
  ec2 <- ec2()
  con <- storr$connection_storr()
  ids <- con$get("aws-ec2")
  lapply(ids, function(x) {
    try(ec2$terminate_instance(x$InstanceId))
  })
  con$del("aws-ec2")
}

#' @export
s3_upload_proj <- function() {
  box::use(paws[s3])
  s3 <- s3()
  bucket_names <- sapply(s3$list_buckets()$Buckets, function(x) {
    x[["Name"]]
  })
  if (!"ndexrapp" %in% bucket_names) {
    s3$create_bucket(
      Bucket = "ndexrapp",
      CreateBucketConfiguration = list(
        LocationConstraint = "us-east-2"
      )
    )
  }
  s3$put_object(
    Body = "nginx.conf",
    Bucket = "ndexrapp",
    Key = "nginx.conf"
  )

  s3$put_object(
    Body = "docker-compose-prod.yaml",
    Bucket = "ndexrapp",
    Key = "docker-compose.yaml"
  )
  s3$put_object(
    Body = "Makefile",
    Bucket = "ndexrapp",
    Key = "Makefile"
  )
}

#' @export
s3_download_proj <- function() {
  box::use(paws[s3])
  s3 <- s3()
  s3$get_object(
    Bucket = "ndexrapp",
    Key = "nginx.conf"
  )

  s3$get_object(
    Bucket = "ndexrapp",
    Key = "docker-compose-prod.yaml"
  )
}
