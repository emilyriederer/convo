context("parse function")

perms_df <- expand.grid(l1 = letters[1:3],
                        l2 = letters[4:6],
                        l3 = letters[7:8])
make_vbls <- function(sep) {with(perms_df, paste(l1, l2, l3, sep = sep))}


# test parse_stubs for various separators ----
test_that(
  "parse_stubs creates expected structure for _ seperator", {
    res <- parse_stubs(make_vbls("_"), sep = "_")
    expect_s3_class(res, "convomin")
    expect_length(res, 3)
  })

test_that(
  "parse_stubs creates expected structure for - seperator", {
    res <- parse_stubs(make_vbls("-"), sep = "-")
    expect_s3_class(res, "convomin")
    expect_length(res, 3)
  })

test_that(
  "parse_stubs creates expected structure for \\. seperator", {
    res <- parse_stubs(make_vbls("\\."), sep = "\\.")
    expect_s3_class(res, "convomin")
    expect_length(res, 3)
  })

test_that(
  "parse_stubs creates expected structure for . seperator", {
    res <- parse_stubs(make_vbls("."), sep = ".")
    expect_s3_class(res, "convomin")
    expect_length(res, 3)
  })

# test parse_df for various seperators ----
test_that(
  "parse_df creates expected structure for _ seperator", {
    res <- parse_df(make_vbls("_"), sep = "_")
    expect_s3_class(res, "data.frame")
    expect_equal(nrow(res), nrow(perms_df))
    expect_equal(ncol(res), ncol(perms_df) + 1)
    expect_equal(sum(is.na(res)), 0)
  })

test_that(
  "parse_df creates expected structure for - seperator", {
    res <- parse_df(make_vbls("-"), sep = "-")
    expect_s3_class(res, "data.frame")
    expect_equal(nrow(res), nrow(perms_df))
    expect_equal(ncol(res), ncol(perms_df) + 1)
    expect_equal(sum(is.na(res)), 0)
  })

test_that(
  "parse_df creates expected structure for \\. seperator", {
    res <- parse_df(make_vbls("."), sep = "\\.")
    expect_s3_class(res, "data.frame")
    expect_equal(nrow(res), nrow(perms_df))
    expect_equal(ncol(res), ncol(perms_df) + 1)
    expect_equal(sum(is.na(res)), 0)
  })

test_that(
  "parse_df creates expected structure for . seperator", {
    res <- parse_df(make_vbls("."), sep = ".")
    expect_s3_class(res, "data.frame")
    expect_equal(nrow(res), nrow(perms_df))
    expect_equal(ncol(res), ncol(perms_df) + 1)
    expect_equal(sum(is.na(res)), 0)
  })

# test miscellaneous edge cases ----
test_that(
  "parse_decomp can handle multiple splitters combined with regex", {
    res <- convo:::parse_decomp(c("a/x-y", "a/w-z"), sep = "(/|-)")
    expect_setequal(res[[1]], c("a", "x", "y"))
    expect_setequal(res[[2]], c("a", "w", "z"))
  }
)

test_that(
  "parse_stubs output unchanged when sort = TRUE", {
    vbls <- c("a_1", "b_1", "b_2", "b_3", "c_4")
    res_f <- parse_stubs(vbls, sep = "_", sort = FALSE)
    res_t <- parse_stubs(vbls, sep = "_", sort = TRUE)
    expect_s3_class(res_t, "convomin")
    expect_length(res_t, 2)
    expect_setequal(res_f[[1]], res_t[[1]])
    expect_setequal(res_f[[2]], res_t[[2]])
    expect_equal(res_t[[1]], c("b", "a", "c"))
    expect_equal(res_t[[2]], as.character(1:4))
  })
