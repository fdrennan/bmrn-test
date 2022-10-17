#' store_input
#' @export store_input
store_input <- function(data, id, filename = "storr") {
  bd <- Sys.getenv("BASE_DIR")
  if (!dir_exists(bd)) {
    cli_alert_info("directory {bd} created")
    dir_create(bd, recurse = T)
  }
  storr_path <- file.path(bd, filename)
  dr <- storr::driver_rds(storr_path)
  st <- storr::storr(dr)
  update_val <- as.list(data)
  if (st$exists(id)) {
    update_val <- update.list(st$get(id), update_val)
    st$set(id, update_val)
  } else {
    st$set(id, update_val)
  }
  update_val
}

#' drop_input
#' @export drop_input
drop_input <- function(id, filename = "storr") {
  bd <- Sys.getenv("BASE_DIR")
  if (!dir_exists(bd)) {
    cli_alert_info("directory {bd} created")
    dir_create(bd, recurse = T)
  }
  storr_path <- file.path(bd, filename)
  dr <- storr::driver_rds(storr_path)
  st <- storr::storr(dr)
  st$del(id)
  # update_val
}


#' get_input
#' @export get_input
get_input <- function(id) {
  bd <- Sys.getenv("BASE_DIR")
  if (!dir_exists(bd)) {
    cli_alert_info("directory {bd} created")
    dir_create(bd, recurse = T)
  }
  storr_path <- file.path(bd, "storr")
  dr <- storr::driver_rds(storr_path)
  st <- storr::storr(dr)
  if (st$exists(id)) {
    return(st$get(id))
  }
  list()
}
