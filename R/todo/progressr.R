test = function() {
  p = progressr::progressor(message = "When is this displayed?", steps = 1000)
  lapply(1:1000, \(i) {
    p()
    Sys.sleep(time = 0.001)
    return(i)
  })
}

progressr::handlers(global = TRUE)
test()
