context("describe function")

# describe names ----
vars <- c("AMT_A_2019", "IND_B_2020")
filepath <- system.file("", "ex-convo.yml", package = "convo")
convo <- read_convo(filepath)
desc_name_df <- describe_names(vars, convo, desc_str = "{level1} of {level2} in given year")

# describe_convo ----
filepath <- system.file("", "ex-convo.yml", package = "convo")
convo <- read_convo(filepath)
desc_convo_df <- describe_convo(convo)
desc_convo_pb_df <- describe_convo(convo, include_valid = TRUE)

# test basic equality ----
test_that("describe names works as expected in base case", {
  expect_s3_class(desc_name_df, "data.frame")
  expect_equal(nrow(desc_name_df), length(vars))
  expect_equal(ncol(desc_name_df), 5)
  expect_named(desc_name_df, c("var_name", paste0('level', 1:3), "desc"))
})

test_that("describe convo works as expected in base case", {
  expect_s3_class(desc_convo_df, "data.frame")
  expect_equal(nrow(desc_convo_df), length(unlist(convo:::get_stubs(convo))))
  expect_equal(ncol(desc_convo_df), 3)
  expect_named(desc_convo_df, c("level", "stub", "stub_desc"))
  expect_equal(desc_convo_df$level, c(rep(1,7), rep(2,3), 3))
  expect_equal(nrow(desc_convo_pb_df), length(unlist(convo:::get_stubs(convo))))
  expect_equal(ncol(desc_convo_pb_df), 4)
  expect_named(desc_convo_pb_df, c("level", "stub", "stub_desc", "checks"))
})
