#' Save a graphic or plot as a tikz file.
#'
#' @param plot the object to be saved.
#' @param filename A character string indicating the desired path to the output
#' file. If both arguments are used in the function call, file will be
#' preferred.
#' @param width The width of the output figure, in **inches**.
#' @param height The height of the output figure, in **inches**.
#' @param asp The aspect ratio of the saved figure.
#'
#' @return save_tikz_plot returns no values.
#' @export
save_tikz_plot <- function(
    plot, filename, width = NA, height = NA, asp = NA
) {
  loadNamespace("tikzDevice")
  loadNamespace("grDevices")
  # automatic scaling
  if (is.na(asp)) asp <- 1.618
  if (is.na(width) && is.na(height)) {
    height <- 3.71
    width <- height * asp
  }
  else if (is.na(width)) {
    width <- height * asp
  }
  else if (is.na(height)) {
    height <- width / asp
  }

  # make tex
  tikzDevice::tikz(file = filename, width = width, height = height)
  # try to print the plot, allowing for error due to unescaped
  # characters
  try(
    print(plot)
  )
  grDevices::dev.off()

  # patch cropping issues
  lines <- readLines(con = filename)
  lines <- lines[-which(grepl("\\path\\[clip\\]*", lines))]
  lines <- lines[-which(grepl("\\path\\[use as bounding box*", lines))]
  writeLines(lines, con = filename)
}
