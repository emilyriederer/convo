context("util functions")

test_that("Safe formatting functions work", {
  expect_equal(convo:::fmt_safe_chr(NULL), NA_character_)
  expect_equal(convo:::fmt_safe_int(NULL), NA_integer_)
  expect_equal(convo:::fmt_safe_lgl(NULL), NA)
  expect_equal(convo:::fmt_safe_lst(NULL), NA)
  expect_equal(convo:::fmt_safe_chr("a"), "a")
  expect_equal(convo:::fmt_safe_int(1), 1)
  expect_equal(convo:::fmt_safe_lgl(TRUE), TRUE)
  expect_equal(convo:::fmt_safe_lst(list(1))[[1]], 1)
})

test_that("Missing packages are flagged", {
  expect_error(convo:::stop_suggest("z", "a"),
               "The package 'z' is required to use the convo function a")
  expect_error(convo:::stop_suggest("convo", "a"), NA)
})

test_that("Get functions return correct values when exist", {
    convo <- read_convo(system.file("ex-convo.yml", package = "convo"))
    stubs <- convo:::get_stubs(convo)
    descs <- convo:::get_desc(convo)
    valid <- convo:::get_valid(convo)
    expect_equal(stubs[[2]], c("A", "C", "D"))
    expect_equal(names(descs[[1]]), stubs[[1]])
    expect_equal(as.character(descs[[2]]), paste("Type", c("A", "C", "D")))
    expect_equal(valid[[1]][[1]],
                 c("col_vals_not_null()", "col_is_numeric()", "col_vals_between(1000, 99999)"))
})
