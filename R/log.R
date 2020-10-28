#' @export
log <- function(flow,
                level = c("DEBUG", "WARNING", "INFO", "ERROR")) {

  all_lines <- flow$log_lines

  lines <- c()

  if ("DEBUG" %in% level)
    level <- c("DEBUG", "INFO", "WARNING", "ERROR")

  for (i in seq_along(level)) {

    lines <- c(lines, grep(all_lines, pattern = level[i]))

  }

  lines <- sort(lines)

  lines <- flow$log_lines[lines]

  cat(lines, sep = "\n")

}
