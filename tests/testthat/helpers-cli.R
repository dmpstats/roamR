capture_cli_messages <- function(expr) {
  msgs <- character()
  withCallingHandlers(
    expr,
    cliMessage = function(e) {
      msgs <<- c(msgs, conditionMessage(e))
      invokeRestart("muffleMessage")
    }
  )
  msgs
}


fix_times <- function(out) {
  out <- sub("[(][ ]*[.0-9]+ [Mk]B/s[)]", "(8.5 MB/s)", out)
  out <- sub("[(][.0-9]+/s[)]", "(100/s)", out)
  out <- sub(" [.0-9]+(ms|s|m)", " 3ms", out)
  out <- sub("ETA:[ ]*[.0-9]+m?s", "ETA:  1s", out)
  out <- gsub("\\[[.0-9]+m?s\\]", "[1s]", out)
  out
}
