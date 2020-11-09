test_that("tidyneuro manages dependencies", {

  f <- function(x, y) {x * y + 1}
  g <- function(x, y) {x + y - 1}
  z <- function(x, y) {t.test(x, y)}

  fg <- function(x, y) {list(f(x, y), g(x, y))}
  fz <- function(x, y) {list(f(x, y), z(x, y))}

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
    step(fz,
         inputs = c("A", "F"),
         output = c("AF", "GEA"))

  expect_named(as.list(wf$env), expected = c("f", "g", "z"), ignore.order = TRUE)

})
