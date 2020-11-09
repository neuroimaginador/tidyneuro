# Get Dependencies of a Function
#
# This function allows to obtain the dependencies of a given function.
# h     (a function) A function to determine its dependencies
# this_env An environment where to add the functions not stored in any package.
#
# returns: A vector with the package and function dependencies of the given function.
#
get_deps <- function(h, this_env,
                     where = rlang::env_parent()) {

  # Get environment/package of function h
  str <- env_name_fun(h, search = where)

  if (stringr::str_detect(str$ns,
                          pattern = stringr::fixed("namespace:"))) return(str)

  # Get function calls and corresponding namespaces
  funs <- get_f_calls(h)

  str <- lapply(funs,
                function(x)
                  env_name_fun(x,
                               search = pryr::enclosing_env(h)))
  fun_def <- lapply(str, function(xx) xx$fun)
  str <- sapply(str, function(xx) xx$ns)
  id_null <- sapply(str, is.null)
  str <- str[!id_null] %>% unlist()
  funs <- funs[!id_null]
  fun_def <- fun_def[!id_null]
  names(fun_def) <- funs

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
             value = fun_def[[fu]],
             envir = this_env)

    }

  }

  # Recurse on functions not in any package
  str <- sapply(seq_along(funs),
                function(i) get_deps(fun_def[[i]], this_env, where = where)) %>% unlist()

  # All dependencies
  deps <- setdiff(unique(c(pkgs, funs, str)), "namespace:base")

  return(deps)

}

env_name_fun <- function(h,
                         search = rlang::current_env()) {

  if (is.character(h)) {

    if (stringr::str_detect(h, pattern = stringr::fixed("::"))) {

      return(list(fun = eval(parse(text = h)),
                  ns = paste0("namespace:",
                         stringr::str_split(h,
                                            pattern = stringr::fixed("::"))[[1]][1])))

    }

    if (h == "%>%") {

      return(list(fun = magrittr::`%>%`,
                  ns = "namespace:magrittr"))

    }

    if (h %in% ls(envir = rlang::as_environment("base"))) {

      return(list(fun = NULL,
                  ns = NULL))

    }

    h1 <- rlang::env_get(nm = h,
                        default = NULL,
                        inherit = TRUE,
                        env = search)

    if (!is.null(h1)) {

      h <- h1

    } else {

      L <- utils::help.search(pattern = h, fields = "name")

      if (length(L) > 0) {

        ii <- which(L$matches$Entry == h)[1]

        if (is.na(ii)) ii <- 1

        pkg <- L$matches$Package[ii]

        fun <- eval(parse(text = paste0(pkg, "::", h)))
        return(list(fun = fun,
               ns = paste0("namespace:", pkg)))

      }


    }

  }

  if (is.null(h)) {

    return(list(fun = NULL,
                ns = NULL))

  }

  pkg <- methods::packageSlot(h)

  if (!is.null(pkg)) return(list(fun = h,
                                 ns = paste0("namespace:", pkg)))

  env <- pryr::enclosing_env(h)
  if (!rlang::is_environment(env)) return(NULL)

  return(list(fun = h, ns = rlang::env_name(env)))

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
