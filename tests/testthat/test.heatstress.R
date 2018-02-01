####################################################################
#
# Package HeatStress
#
# Test for heat stress indices computation
#
###################################################################

context("Heat Stress indices computation")

data("eca_salam_jja_2003")
tas <- eca_salam_jja_2003$tasmean
hurs <- eca_salam_jja_2003$hurs
dewp <- eca_salam_jja_2003$dewp
wind <- eca_salam_jja_2003$wind
solar <- eca_salam_jja_2003$solar
dates <- eca_salam_jja_2003$Dates


test_that("test if the wbt.Stull function computes WBT properly",{
  data("wbt")
  WBT <- wbt
  
  WBT.new <- wbt.Stull(tas, hurs)
  
  expect_equal(WBT.new,WBT, tolerance = 1e-3)
})

test_that("test if the wbgt.Bernard function computes WBGTshade properly",{
  data("wbgt.indoors")
  WBGT.shade <- wbgt.indoors$data
  Tpwb <- wbgt.indoors$Tpwb
  
  WBGTshade.new <- wbgt.Bernard(tas,dewp)
  
  expect_equal(WBGTshade.new$data,WBGT.shade, tolerance = 1e-3)
  
})

test_that("test if the wbgt.Bernard function computes Tpwb properly",{
  data("wbgt.indoors")
  Tpwb <- wbgt.indoors$Tpwb
  
  WBGTshade.new <- wbgt.Bernard(tas,dewp)
  
  expect_equal(WBGTshade.new$Tpwb,Tpwb, tolerance = 1e-3)
  
})

test_that("test if the wbgt.Liljegren function computes WBGTsun properly",{
  data("wbgt.outdoors")
  WBGT.sun <- wbgt.outdoors$data
  Tnwb <- wbgt.outdoors$Tnwb
  Tg <- wbgt.outdoors$Tg
  
  WBGTsun.new <- wbgt.Liljegren(tas, dewp, wind, solar, dates, -5.66, 40.96) # coordinates Salamanca
  
  expect_equal(WBGTsun.new$data,WBGT.sun, tolerance = 1e-3)
  
})
test_that("test if the wbgt.Liljegren function computes WBGTsun properly",{
  data("wbgt.outdoors")
  Tnwb <- wbgt.outdoors$Tnwb
  
  WBGTsun.new <- wbgt.Liljegren(tas, dewp, wind, solar, dates, -5.66, 40.96) # coordinates Salamanca
  
  expect_equal(WBGTsun.new$Tnwb,Tnwb, tolerance = 1e-3)
  
})
test_that("test if the wbgt.Liljegren function computes WBGTsun properly",{
  data("wbgt.outdoors")
  Tg <- wbgt.outdoors$Tg
  
  WBGTsun.new <- wbgt.Liljegren(tas, dewp, wind, solar, dates, -5.66, 40.96) # coordinates Salamanca
  
  expect_equal(WBGTsun.new$Tg,Tg, tolerance = 1e-3)
  
})
