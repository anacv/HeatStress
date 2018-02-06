####################################################################
#
# Package HeatStress
#
# Test for heat stress indices computation
#
###################################################################
library("HeatStress")
context("Heat Stress indices computation: WBT")

test_that("test if the wbt.Stull function computes WBT properly",{
  data("data_obs", envir = environment())
  tas <- data_obs$tasmean
  hurs <- data_obs$hurs

  data("data_wbt.Stull", envir = environment())
  WBT <- data_wbt.Stull
  
  WBT.new <- wbt.Stull(tas, hurs)
  
  expect_equal(WBT.new,WBT, tolerance = 1e-3)
})
