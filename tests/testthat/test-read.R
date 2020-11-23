context("read function")

path <- system.file("ex-convo.yml", package = "convo")
convo <- read_convo(path)

test_that(
  "convo reads in from YAML with correct structure", {
  expect_length(convo, 3)
  expect_length(convo[[1]], 7)
  expect_length(convo[[2]], 3)
  expect_length(convo[[3]], 1)
  expect_length(convo[[1]][[1]], 2)
})

test_that(
  "convo reads in from YAML with correct values", {
    expect_named(convo[[2]], c("A", "C", "D"))
    expect_named(convo[[3]], "\\d{4}")
    expect_named(convo[[1]][[1]], c("desc", "valid"))
    expect_named(convo[[2]][[1]], c("desc"))
  }
)
