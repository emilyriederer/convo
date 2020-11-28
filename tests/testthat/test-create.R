context("create function")

stubs <- list(letters[1:3], letters[4:5], letters[6:7])
convo <- create_convo(stubs)

test_that(
  "basic create function works as expected", {
    stubs_rt <- convo:::get_stubs(convo)
    expect_length(convo, 3)
    expect_s3_class(convo, "convo")
    expect_equal(stubs[[1]], stubs_rt[[1]])
    expect_equal(stubs[[2]], stubs_rt[[2]])
    expect_equal(stubs[[3]], stubs_rt[[3]])
  })

test_that(
  "basic add function works as expected", {
    convo_add <- add_convo_stub(convo, 1, "z")
    stubs_add <- convo:::get_stubs(convo_add)
    expect_length(convo_add, 3)
    expect_s3_class(convo_add, "convo")
    expect_equal(stubs_add[[1]], c("a", "b", "c", "z"))
    expect_equal(stubs_add[[2]], stubs[[2]])
    expect_equal(stubs_add[[3]], stubs[[3]])
  })

test_that(
  "add throws error on bad input / unclear intent", {
    expect_error(add_convo_stub(convo, "b", 1))
    expect_error(add_convo_stub(convo, 1, "b"), "Provided stub already exists")
  })


test_that(
  "basic remove function works as expected", {
  convo_rem <- remove_convo_stub(convo, 1, "b")
  stubs_rem <- convo:::get_stubs(convo_rem)
  expect_length(convo_rem, 3)
  expect_s3_class(convo_rem, "convo")
  expect_equal(stubs_rem[[1]], c("a", "c"))
  expect_equal(stubs_rem[[2]], stubs[[2]])
  expect_equal(stubs_rem[[3]], stubs[[3]])
  })

test_that(
  "remove throws error on bad input / unclear intent", {
    expect_error(remove_convo_stub(convo, "b", 1))
    expect_error(remove_convo_stub(convo, 1, "z"))
  })
