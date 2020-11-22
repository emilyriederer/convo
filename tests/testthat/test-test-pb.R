context("pointblank functions")

library(pointblank)

filepath <- system.file("", "ex-convo.yml", package = "convo")
convo <- read_convo(filepath)
df <- data.frame(IND_A = 1, IND_B = 5, DT_B = as.Date("2020-01-01"))

tmp <- gsub("\\\\", "/", tempdir())
write_pb(convo, names(df), filename = "convo-validation.yml", path = tmp)

agent_yaml <-
  yaml_read_agent(file.path(tmp, "convo-validation.yml")) %>%
  remove_tbl() %>%
  set_tbl(df)
agent_obj  <- create_pb_agent(convo, df)

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
