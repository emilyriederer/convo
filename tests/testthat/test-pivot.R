context("pivot function")

test_that(
  "convo pivots as expected given normal input", {
    convo <- list(letters[1:3], letters[2:4], letters[3:6])
    pivot_all <- pivot_convo(convo, repeats_only = FALSE)
    pivot_rep <- pivot_convo(convo, repeats_only = TRUE)
    expect_length(pivot_all, length(unique(unlist(convo))))
    expect_length(pivot_rep, 3)
    expect_named(pivot_all, letters[1:6])
    expect_named(pivot_rep, letters[2:4])
    expect_equal(pivot_rep[[1]], 1:2)
    expect_equal(pivot_rep[[2]], 1:3)
    expect_equal(pivot_rep[[3]], 2:3)
  })

test_that(
  "pivot gracefully handles cases with no overlap", {
    convo <- list(letters[1:2], letters[3:4], letters[5:6])
    pivot_all <- pivot_convo(convo, repeats_only = FALSE)
    pivot_rep <- pivot_convo(convo, repeats_only = TRUE)
    expect_length(pivot_all, length(unique(unlist(convo))))
    expect_length(pivot_rep, 0)
    expect_named(pivot_all, letters[1:6])
    expect_equal(setNames(unlist(pivot_all), NULL), rep(1:3, each = 2))
    expect_type(pivot_rep, "list")
  }
)
