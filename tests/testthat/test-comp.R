context("comparison function")

# basic usage ----
convo <- list(c("ind"), letters[1:3], c("pre", "post"))
vbl_names <- c("ind_a", "ind_d", "amt_c", "cat_c_pre", "cat_c_post")
vbl_stubs <- parse_stubs(vbl_names, sep = "_")
comp_uni <- compare_convo(vbl_stubs, convo, "union")
comp_int <- compare_convo(vbl_stubs, convo, "intersect")
comp_set <- compare_convo(vbl_stubs, convo, "setdiff")

# usage with regex ----
rgx_uni <- compare_convo(list(c(1, 12, 123)), list(c("1", "\\d{3}")), "union")
rgx_int <- compare_convo(list(c(1, 12, 123)), list(c("1", "\\d{3}")), "intersect")
rgx_set <- compare_convo(list(c(1, 12, 123)), list(c("1", "\\d{3}")), "setdiff")

# tests ----
test_that(
  "Comparison returns object of correct length", {
    expect_length(comp_uni, max(length(vbl_stubs), length(convo)))
    expect_length(comp_int, max(length(vbl_stubs), length(convo)))
    expect_length(comp_set, max(length(vbl_stubs), length(convo)))
  })

test_that(
  "Union returns all relevant stubs", {
    expect_setequal(comp_uni[[1]], c("ind", "amt", "cat"))
    expect_setequal(comp_uni[[2]], c("a", "b", "c", "d"))
    expect_setequal(comp_uni[[3]], c("pre", "post"))
  })

test_that(
  "Comparisons with regex are nonredundant", {
    expect_length(rgx_uni[[1]], 4)
    expect_length(rgx_int[[1]], 2)
    expect_length(rgx_set[[1]], 1)
  }
)

test_that(
  "Comparisons with regex return relevant stubs", {
    expect_setequal(rgx_uni[[1]], c("1", "12", "123", "\\d{3}"))
    expect_setequal(rgx_int[[1]], c(1, 123))
    expect_setequal(rgx_set[[1]], c(12))
  }
)
