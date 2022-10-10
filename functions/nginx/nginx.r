#' @export
update_conf <- function(ip, showDoc = FALSE) {
  box::use(readr[read_file, write_file])
  box::use(stringr[str_replace_all])
  nginx_conf <- read_file("nginx.conf.default")
  ip_vec <- c(getOption("domain"), getOption("localhost"))
  configs <- str_replace_all(nginx_conf, "localhost", ip_vec)
  configs[[1]] <- str_replace_all(configs[[1]], "servername", "server_name ndexr.com www.ndexr.com;")
  configs[[2]] <- str_replace_all(configs[[2]], "servername", "")
  write_file(configs[[1]], "ec2.nginx.conf")
  write_file(configs[[2]], "localhost.nginx.conf")
  if (all(interactive(), showDoc)) {
    rstudioapi::documentOpen("nginx.conf.default")
    rstudioapi::documentOpen("ec2.nginx.conf")
    rstudioapi::documentOpen("localhost.nginx.conf")
  }
}
