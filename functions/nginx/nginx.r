#' @export
update_conf <- function(ip) {
  box::use(readr[read_file, write_file])
  box::use(stringr[str_replace_all])
  nginx_conf <- read_file("./nginx.conf.default")
  ip_vec <- c(getOption("domain"), getOption("localhost"))

  configs <- str_replace_all(nginx_conf, "localhost", ip_vec)
  write_file(configs[[1]], "ec2.nginx.conf")
  write_file(configs[[2]], "localhost.nginx.conf")
}
