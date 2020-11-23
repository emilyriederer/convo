context("parse function")

perms_df <- expand.grid(l1 = letters[1:3],
                        l2 = letters[4:6],
                        l3 = letters[7:8])
make_vbls <- function(sep) {with(perms_df, paste(l1, l2, l3, sep = sep))}

test_that(
  "parse_stubs creates expected structure for _ seperator", {
    expect_s3_class(parse_stubs(make_vbls("_")), "convomin")
    expect_length(parse_stubs(make_vbls("_")), 3)
  })

test_that(
  "parse_stubs creates expected structure for - seperator", {
    expect_s3_class(parse_stubs(make_vbls("-"), sep = "-"), "convomin")
    expect_length(parse_stubs(make_vbls("-"), sep = "-"), 3)
  })

test_that(
  "parse_stubs creates expected structure for - seperator", {
    expect_s3_class(parse_stubs(make_vbls("."), sep = "\\."), "convomin")
    expect_length(parse_stubs(make_vbls("."), sep = "\\."), 3)
  })

test_that(
  "parse_df creates expected structure for _ seperator", {
    expect_s3_class(parse_df(make_vbls("_")), "data.frame")
    expect_equal(nrow(parse_df(make_vbls("_"))), nrow(perms_df))
    expect_equal(ncol(parse_df(make_vbls("_"))), ncol(perms_df) + 1)
  })

test_that(
  "parse_df creates expected structure for - seperator", {
    expect_s3_class(parse_df(make_vbls("-"), sep = "-"), "data.frame")
    expect_equal(nrow(parse_df(make_vbls("-"), sep = "-")), nrow(perms_df))
    expect_equal(ncol(parse_df(make_vbls("-"), sep = "-")), ncol(perms_df) + 1)
  })

test_that(
  "parse_df creates expected structure for _ seperator", {
    expect_s3_class(parse_df(make_vbls("."), sep = "\\."), "data.frame")
    expect_equal(nrow(parse_df(make_vbls("."), sep = "\\.")), nrow(perms_df))
    expect_equal(ncol(parse_df(make_vbls("."), sep = "\\.")), ncol(perms_df) + 1)
  })
