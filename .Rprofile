options(Ncpus = 4L,
        mc.cores = 4L,
        future.plan = "multisession")

if (interactive()) {
  require("colorout", quietly = T)

  q <- function(save = "no", ...) {
    quit(save = save, ...)
  }

  exit <- q

  utils::rc.settings(ipck = TRUE)

  options(max.print = 100)

  options(prompt = "> ",
          continue = "... ")
}
