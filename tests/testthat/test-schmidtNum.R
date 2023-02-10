test_that("schmidtNum() returns the Schmidt number for a specific gas at a given temperature", {
  expect_equal(schmidtNum(20, gas = "O2", method = "Wan2014"), 510.2472)
})
