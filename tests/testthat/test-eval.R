context("evaluate function")

convo <- list(c("ind"), letters[1:3], c("pre", "post"))
vbl_names <- c("ind_a", "ind_d", "amt_c", "cat_c_pre", "cat_c_post")
evaluation <- evaluate_convo(convo, vbl_names)

test_that(
  "Evaluation identifies correct number of discrepancies", {
  expect_length(evaluation, length(convo))
  expect_length(evaluation[[1]], 3)
  expect_length(evaluation[[2]], 1)
  expect_length(evaluation[[3]], 0)
})

test_that(
  "Evaluation works with regex", {
    convo[[4]] <- "\\d{4}"
    expect_length(evaluate_convo(convo, "ind_a_pre_1")[[1]], 0)
    expect_length(evaluate_convo(convo, "ind_a_pre_1")[[2]], 0)
    expect_length(evaluate_convo(convo, "ind_a_pre_1")[[3]], 0)
    expect_length(evaluate_convo(convo, "ind_a_pre_1")[[4]], 1)
    expect_equal(evaluate_convo(convo, "ind_a_pre_1")[[4]], "ind_a_pre_1")
    expect_length(evaluate_convo(convo, "ind_a_pre_1234")[[4]], 0)
    expect_equal(evaluate_convo(convo, "ind_a_pre_12345")[[4]], "ind_a_pre_12345")
  }
)
