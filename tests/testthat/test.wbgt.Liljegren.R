####################################################################
#
# Package HeatStress
#
# Test for heat stress indices computation
#
###################################################################
library("HeatStress")
context("Heat Stress indices computation: WBGT Liljegren")


test_that("test if the wbgt.Liljegren function computes WBGTsun properly",{
  data("data_obs", envir = environment())
  tas <- data_obs$tasmean
  hurs <- data_obs$hurs
  dewp <- data_obs$dewp
  wind <- data_obs$wind
  solar <- data_obs$solar
  dates <- data_obs$Dates
  data("data_wbgt.Liljegren", envir = environment())
  WBGT.sun <- data_wbgt.Liljegren$data
  Tnwb <- data_wbgt.Liljegren$Tnwb
  Tg <- data_wbgt.Liljegren$Tg
  
  WBGTsun.new <- wbgt.Liljegren(tas, dewp, wind, solar, dates, -5.66, 40.96) # coordinates Salamanca
  
  expect_equal(WBGTsun.new$data,WBGT.sun, tolerance = 1e-3)
  expect_equal(WBGTsun.new$Tnwb,Tnwb, tolerance = 1e-3)
  expect_equal(WBGTsun.new$Tg,Tg, tolerance = 1e-3)
  
})
