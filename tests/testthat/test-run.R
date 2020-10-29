context("Plot")

test_that("tidyneuro runs simple workflows", {

  f <- function(x, y) {x * y + 1}
  g <- function(x, y) {x + y - 1}

  fg <- function(x, y) {list(f(x, y), g(x, y))}

  wf <- workflow("test") %>%
    inputs(A = element_matrix(),
           B = element_matrix()) %>%
    step(f,
         inputs = c("A", "B"),
         output = "C")

  wf %>%
    step(f, inputs = c("A", "C"), output = "D")

  wf %>%
    step(g, inputs = c("B", "D"), output = "E")

  wf %>%
    step(fg,
         inputs = c("A", "B"),
         output = c("F", "G"))

  wf %>%
    step(fg,
         inputs = c("A", "F"),
         output = c("AF", "GEA"))

  expect_error(res <- wf %>%
                 run(inputs = c(A = 1, B = 2),
                     desired_outputs = c("F", "G")),
               NA)
  expect_named(res, c("F", "G"))

})
