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


# validation checks with flipped schema (measure in level 2/3) ---------------

dt_msr_in_lvl3 <- data.frame(car_name_id = numeric(), car_name_name = character(),
                             passenger_name_cnt = numeric())
dt_msr_in_lvl2 <- data.frame(car_id = numeric(), passenger_name_first = character(),
                             passenger_cnt = numeric())


cnv_msr_in_lvl3 <- structure(list(
  level1 = list(car = NULL,
                passenger = NULL),
  level2 = list(name = NULL),
  level3 = list(id = list(valid = "col_is_numeric()"),
                cnt = list(valid = "col_is_numeric()"),
                name = NULL)),
  class = c("convo", "list"))

cnv_msr_in_lvl2 <- structure(list(
  level1 = list(car = NULL,
                passenger = NULL),
  level2 = list(id = list(valid = "col_is_numeric()"),
                cnt = list(valid = "col_is_numeric()"),
                name = NULL),
  level3 = list(last = NULL,
                first = NULL)),
  class = c("convo", "list"))

cagent3 <- create_pb_agent(cnv_msr_in_lvl2, dt_msr_in_lvl2, level = 2)
cagent2 <- create_pb_agent(cnv_msr_in_lvl3, dt_msr_in_lvl3, level = 3)

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

# tests for validation checks with flipped schema (measure in level 2/3) ---------------

test_that("Level 2 and 3 data frames match their respective convos", {
  expect_equal(evaluate_convo(cnv_msr_in_lvl2, names(dt_msr_in_lvl2)) %>% unlist(),
               character())
  expect_equal(evaluate_convo(cnv_msr_in_lvl3, names(dt_msr_in_lvl3)) %>% unlist(),
               character())
})

test_that("columns are captured for level 3 checks in pointblank", {
  expect_equal(nrow(cagent3$validation_set), 2)
  expect_setequal(sort(unlist(cagent3$validation_set$column)),
                  c("car_id", "passenger_cnt"))
})

test_that("columns are captured for level 2 checks in pointblank", {
  expect_equal(nrow(cagent2$validation_set), 2)
  expect_setequal(sort(unlist(cagent2$validation_set$column)),
                  c("car_name_id", "passenger_name_cnt"))
})


