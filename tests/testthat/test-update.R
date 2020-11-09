test_that("a flow can be updated", {


  f <- function(x, y) {x * y + 1}
  g <- function(x, y) {x + y - 1}

  fg <- function(x, y) {list(f(x, y), g(x, y))}

  wf <- workflow("test") %>%
    inputs(A = element_matrix(),
           B = element_matrix()) %>%
    step(f,
         inputs = c("A", "B"),
         output = "C")

  wf %>% update(output = "C",
                new_function = g)

  expect_identical(wf$processes[["C"]], g)

})
