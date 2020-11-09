#' Create an R Package from a Flow
#'
#' @param flow           The flow to export as a package
#' @param path           Root folder where to create the package
#' @param package_name   Package name. The default is the name assgined to the workflow.
#' @param open           Logical value indicating if the new project should be opened in a new RStudio window.
#'
#' @return Invisibly \code{TRUE}.
#' @export
#'
pack <- function(flow, path,
                 package_name = flow$name,
                 open = FALSE) {

  my_outputs <- flow$outputs
  my_inputs <- flow$inputs

  my_env <- rlang::env_clone(flow$env)

  # Add specific functions to compute outputs
  for (output  in setdiff(my_outputs, my_inputs)) {

    my_env[[output]] <- flow$processes[[output]]

  }

  # Create package
  suppressMessages(utils::package.skeleton(name = package_name,
                   path = path,
                   environment = my_env,
                   encoding = "UTF-8"))

  pkg <- file.path(path, package_name)

  # Remove "Read-and-delete-me" file
  unlink(x = file.path(pkg, "Read-and-delete-me"), force = TRUE)

  # Edit description file
  desc_file <- file.path(pkg, "DESCRIPTION")
  description <- desc::description$new(file = desc_file)

  description$set("Title", paste0(flow$name, " Flow"))
  description$set("Description", paste0("The ", flow$name, " flow created using tidyneuro."))

  if (requireNamespace("whoami", quietly = TRUE)) {

    try(suppressWarnings(description$add_me()))

  }

  # Add dependencies to the DESCRIPTION file
  flow_deps <- flow$pkgs %>%
    unlist() %>%
    unique()

  pkgs_id <- flow_deps %>%
    stringr::str_which(pattern = stringr::fixed("namespace:"))

  if (length(pkgs_id) > 0) {

    flow_deps <- flow_deps[pkgs_id] %>%
      stringr::str_remove_all(pattern = stringr::fixed("namespace:"))

    invisible(sapply(flow_deps, description$set_dep))

  }

  description$set_dep("magrittr", "Imports")
  description$set_dep("tidyneuro", "Imports")

  description$normalize()

  description$write(file = desc_file)

  # Write the calls to rebuild the flow
  flow_txt <- flow %>%
    .to_call() %>%
    as.character()

  flow_txt <- c("library(tidyneuro)",
                flow_txt) %>%
    stringr::str_flatten(collapse = "\n")

  # Save the flow
  # Add zzz.R which loads the flow as "flow"
  zzz_file <- file.path(pkg, "R", "zzz.R")

  cat(flow_txt, file = zzz_file)

  # Create project infrastructure
  usethis::create_project(path = pkg, open = open)

  return(invisible(TRUE))

}
