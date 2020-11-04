# Get Dependencies of a Function
#
# This function allows to obtain the dependencies of a given function.
# h     (a function) A function to determine its dependencies
# this_env An environment where to add the functions not stored in any package.
#
# returns: A vector with the package and function dependencies of the given function.
#
get_deps <- function(h, this_env) {

  # Get environment/package of function h
  # browser()
  # if (is.character(h)) {
  #
  #   h <- dynGet(h)
  #
  # }
  # h <- match.fun(h)
  str <- env_name_fun(h)

  if (stringr::str_detect(str,
                          pattern = stringr::fixed("namespace:"))) return(str)

  # Get function calls and corresponding namespaces
  funs <- get_f_calls(h)
  str <- sapply(funs, env_name_fun, USE.NAMES = FALSE)
  id_null <- sapply(str, is.null)
  str <- str[!id_null] %>% unlist()
  funs <- funs[!id_null]

  # Collect package dependencies
  id_pkgs <- str %>%
    stringr::str_detect(pattern = stringr::fixed("namespace:"))

  pkgs <- str[id_pkgs]
  funs <- funs[!id_pkgs]

  # If not is missing this_env, add the functions
  # to the environment
  if (!missing(this_env)) {

    for (fu in funs) {

      assign(x = fu,
             value = pryr::fget(fu,
                                env = pryr::enclosing_env(h)),
             envir = this_env)

    }

  }

  # Recurse on functions not in any package
  str <- sapply(funs, get_deps) %>% unlist()

  return(c(pkgs, funs, str))

}

env_name_fun <- function(h) {

  # h <- match.fun(h)
  # print(h)
  if (is.character(h)) h <- dynGet(h, ifnotfound = c)
  env <- pryr::enclosing_env(h)
  if (!rlang::is_environment(env)) return("")

  return(rlang::env_name(env))

}

get_f_calls <- function (f) {
  if (is.function(f)) {
    get_f_calls(body(f))
  }
  else if (is.call(f)) {
    fname <- as.character(f[[1]])
    if (identical(fname, ".Internal"))
      return(fname)
    if (identical(fname[1], "::"))
      fname <- paste0(fname[2], "::", fname[3])
    unique(c(fname, unlist(lapply(f[-1], get_f_calls), use.names = FALSE)))
  }
}
