context("print functions")

test_that(
  "print.convo produces expected results in base case", {
    convo <- create_convo(list(letters[1:3], letters[4:6]))
    expect_equal(capture.output(print(convo)),
                 c("Level 1", "- a", "- b", "- c", "Level 2", "- d", "- e", "- f"))
  }
)

test_that(
  "print.convomin produces expected results in case case", {
    x <- c("a_b")
    convo <- parse_stubs(x)
    expect_equal(capture.output(print(convo)),
                 c("Level 1", "- a", "Level 2", "- b"))
  }
)
