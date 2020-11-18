context("write function")

dir <- tempdir()
file <- "test-convo.yml"
path <- paste0(dir, "/", file)
valid_stubs <- list(letters[1:3], letters[4:5], letters[6:7])
write_convo(valid_stubs, file, dir)
convo <- read_convo(path)
unlink(path)
res <- evaluate_convo(convo, c("a", "a_b", "a_d", "a_d_f", "a_d_h", "q", "q_c"))

test_that(
  "Written convo object is readable and executes correctly", {
    expect_length(res, 3)
    expect_setequal(res[[1]], c("q", "q_c"))
    expect_setequal(res[[2]], c("a_b", "q_c"))
    expect_setequal(res[[3]], c("a_d_h"))
  }
)
