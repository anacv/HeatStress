####################################################################
#
# Package HeatStress
#
# Test for heat stress indices computation
#
###################################################################
library("HeatStress")
context("Heat Stress indices computation: WBGT Bernard")


test_that("test if the wbgt.Bernard function computes WBGTshade properly",{
  data("data_obs", envir = environment())
  tas <- data_obs$tasmean
  hurs <- data_obs$hurs
  dewp <- data_obs$dewp
  wind <- data_obs$wind
  solar <- data_obs$solar
  dates <- data_obs$Dates

  data("data_wbgt.Bernard", envir = environment())
  WBGT.shade <- data_wbgt.Bernard$data
  Tpwb <- data_wbgt.Bernard$Tpwb
  
  WBGTshade.new <- wbgt.Bernard(tas,dewp)
  
  expect_equal(WBGTshade.new$data,WBGT.shade, tolerance = 1e-3)
  
  expect_equal(WBGTshade.new$Tpwb,Tpwb, tolerance = 1e-3)
})

