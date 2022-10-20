#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

library(test)
library(plumber)

#
# #* Plot a histogram
# #* @serializer svg
# #* @get /logo
# function(req, res) {
#   test_logo <- 'C:/Users/drenn/Desktop/test/TEST_logo v3b.png'
#   readBin(test_logo,'raw',n = file.info(test_logo)$size)
#
# }
#
#
# # Programmatically alter your API
# #* @plumber
# function(pr) {
#   pr
# }


device_size <- function() {
  h_ <- 7
  w_ <- 7
  list(
    h = function() h_,
    w = function() w_,
    set_h = function(h) {
      if (!is.null(h)) {
        h_ <<- as.numeric(h)
      }
    },
    set_w = function(w) {
      if (!is.null(w)) {
        w_ <<- as.numeric(w)
      }
    }
  )
}

output_size <- device_size()

serializer_dynamic_svg <- function(..., type = "image/svg+xml") {
  serializer_device(
    type = type,
    dev_on = function(filename) {
      grDevices::svg(filename,
        width = output_size$w(),
        height = output_size$h()
      )
    }
  )
}

register_serializer("svg", serializer_dynamic_svg)

#* @filter dynamic_size
function(req) {
  if (req$PATH_INFO == "/plot") {
    output_size$set_w(req$args$width)
    output_size$set_h(req$args$height)
  }
  plumber::forward()
}

#* @get /plot
#* @param width
#* @param height
#* @serializer svg
function() {
  plot(image_read(rsvg_svg("TEST_logo v3b.png")))
}
