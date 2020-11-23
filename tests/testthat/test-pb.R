context("pointblank functions")

library(pointblank)

tmp <- gsub("\\\\", "/", tempdir())
filepath <- system.file("", "ex-convo.yml", package = "convo")

# validation checks at level 1 ----
convo <- read_convo(filepath)
df <- data.frame(IND_A = 1, IND_B = 5, DT_B = as.Date("2020-01-01"))
write_pb(convo, names(df), filename = "convo-validation.yml", path = tmp)
agent_yaml <-
  yaml_read_agent(file.path(tmp, "convo-validation.yml")) %>%
  remove_tbl() %>%
  set_tbl(df)
agent_obj  <- create_pb_agent(convo, df)

# validation checks at level 2 ----
convo2 <- convo
convo2[[1]] <- convo[[2]]
convo2[[2]] <- convo[[1]]
df2 <- data.frame(A_IND = 1, B_IND = 5, B_DT = as.Date("2020-01-01"))
write_pb(convo2, names(df2), filename = "convo-validation2.yml", path = tmp, level = 2)
agent_yaml2 <-
  yaml_read_agent(file.path(tmp, "convo-validation2.yml")) %>%
  remove_tbl() %>%
  set_tbl(df2)
agent_obj2  <- create_pb_agent(convo2, df2, level = 2)

# tests when validation checks at level 1 ----
test_that(
  "YAML agent find correct variables and data checks ", {
    expect_equal(nrow(agent_yaml$validation_set), 5)
    expect_equal(sort(unlist(agent_yaml$validation_set$column)),
                 c("DT_B", rep(c("IND_A", "IND_B"), each = 2)))
    expect_equal(sort(agent_yaml$validation_set$assertion_type),
                 c("col_is_date", rep(c("col_is_numeric", "col_vals_in_set"), each = 2)))
  }
)

test_that(
  "Object agent find correct variables and data checks ", {
    expect_equal(nrow(agent_obj$validation_set), 5)
    expect_equal(sort(unlist(agent_obj$validation_set$column)),
                 c("DT_B", rep(c("IND_A", "IND_B"), each = 2)))
    expect_equal(sort(agent_obj$validation_set$assertion_type),
                 c("col_is_date", rep(c("col_is_numeric", "col_vals_in_set"), each = 2)))
  }
)

test_that(
  "Fail gracefully when no validation checks available for pointblank", {
    convo_null <- create_convo(list(letters[1:3], letters[4:6]))
    expect_error(create_pb_agent(convo_null, data.frame(a_d = 1)),
                 "convo object supplied has no validation checks")
    expect_error(write_pb(convo_null, "a_d", filename = "x.yml", path = tmp),
                 "convo object supplied has no validation checks")
  }
)

# tests when validation checks at level 2 ----
test_that(
  "YAML agent find correct variables and data checks ", {
    expect_equal(nrow(agent_yaml2$validation_set), 5)
    expect_equal(sort(unlist(agent_yaml2$validation_set$column)),
                 c(rep("A_IND", 2), "B_DT", rep("B_IND", 2)))
    expect_equal(sort(agent_yaml2$validation_set$assertion_type),
                 c("col_is_date", rep(c("col_is_numeric", "col_vals_in_set"), each = 2)))
  }
)

test_that(
  "Object agent find correct variables and data checks ", {
    expect_equal(nrow(agent_obj2$validation_set), 5)
    expect_equal(sort(unlist(agent_obj2$validation_set$column)),
                 c(rep("A_IND", 2), "B_DT", rep("B_IND", 2)))
    expect_equal(sort(agent_obj2$validation_set$assertion_type),
                 c("col_is_date", rep(c("col_is_numeric", "col_vals_in_set"), each = 2)))
  }
)


