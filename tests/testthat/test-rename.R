context("read function")

path <- system.file("ex-convo.yml", package = "convo")
convo <- read_convo(path)


test_that(
  "convo reads in from YAML with correct structure", {
  expect_equal(rename_convo("IND_A_SUM", convo), "N_A")
  expect_equal(rename_convo("IND_A_AVG", convo), "P_A")
  expect_equal(rename_convo("VAL_A_AVG", convo), "VALAV_A")
})

test_that(
  "rename makes no changes when no relevant stubs are affected", {
  expect_equal(rename_convo(c("A_B", "C_D"), convo), c("A_B", "C_D"))
  expect_equal(rename_convo(c("IND_B", "IND_D"), convo), c("IND_B", "IND_D"))
  expect_equal(rename_convo(c("A_SUM", "C_SUM"), convo), c("A_SUM", "C_SUM"))
  }
)
