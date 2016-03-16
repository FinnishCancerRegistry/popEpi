context('rate')

# simultate test data
set.seed(5)
p18 <- data.table( OBS=round(runif(36)*10), PYRS=round(runif(36)*10000), AGEGROUP=1:18, COV = rep(c(1,2), each = 18))
set.seed(5)
p20 <- data.table( OBS=round(runif(20)*10), PYRS=round(runif(20)*10000), AGEGROUP=1:20, COV = rep(c(1,2), each = 20))
set.seed(5)
p101 <- data.table( OBS=round(runif(101)*10), PYRS=round(runif(101)*10000), AGEGROUP=1:101, COV = rep(c(1,2), each = 101))
p18b <- data.table(p18)
setnames(p18b, c('OBS','PYRS','AGEGROUP'), c('obs','pyrs','agegroup'))

test_that("rate works with different weights", {
  w1 <- rate(data = p18, obs = 'OBS', pyrs = 'PYRS', print = 'COV', adjust = 'AGEGROUP', weights = c(1:18))
  w2 <- rate(data = p20, obs = 'OBS', pyrs = 'PYRS', print = 'COV', adjust = 'AGEGROUP', weights = 'world_2000_20of5')
  w3 <- rate(data = p20, obs = 'OBS', pyrs = 'PYRS', print = 'COV', adjust = 'AGEGROUP', weights = 'cohort')
  w4 <- rate(data = p20, obs = 'OBS', pyrs = 'PYRS', print = 'COV', adjust = NULL, weights = NULL)
  w5 <- rate(data = p20, obs = 'OBS', pyrs = 'PYRS', print = NULL, adjust = NULL, weights = NULL)
  
  expect_equal(sum(w1$PYRS), p18[,sum(PYRS)])
  expect_equal(sum(w2$OBS), p20[,sum(OBS)])
  expect_equal(sum(w3$OBS), p20[,sum(OBS)])
  expect_equal(w4$rate, p20[,list(sum(OBS)/sum(PYRS)), by ='COV'][, V1])
  expect_equal(w5$rate, p20[,list(sum(OBS)/sum(PYRS))][, V1])
  expect_is(w1, 'rate')
  expect_is(w2, 'rate')
  expect_is(w2, 'data.frame')
  if(getOption("popEpi.datatable")) {
    expect_is(w2, 'data.table')
  }
})

test_that("names dont cause problems", {
  w1 <- rate(data = p18b, obs = 'obs', pyrs = 'pyrs', print = 'COV', adjust = 'agegroup', weights = 'nordic')
  w2 <- rate(data = p18b, obs = 'obs', pyrs = 'pyrs', print = 'COV', adjust = 'agegroup', weights = 'cohort')
  w3 <- rate(data = p18b, obs = obs, pyrs = pyrs, print = COV, adjust = agegroup, weights = 'cohort')
  w4 <- rate(data = p18b, obs = 'obs', pyrs = 'pyrs', print = 'COV', adjust = 'agegroup', weights = 'cohort')
  wr <- p18b[,list(obs=sum(obs),pyrs =sum(pyrs)), by ='COV']
  
  expect_equal(w2$obs, w1$obs)
  expect_equal(w2$pyrs, w1$pyrs)
  expect_equal(wr$obs, w1$obs)
  expect_equal(wr$pyrs, w1$pyrs)
  expect_equal(w4, w3)
})


test_that("rate works with different weights an subset", {
  s0 <- rate(data = p18, obs = 'OBS', pyrs = 'PYRS', print = NULL, adjust = 'AGEGROUP', weights = c(1:18))
  s0 <- rate(data = p18, obs = 'OBS', pyrs = 'PYRS', print = NULL, adjust = 'AGEGROUP', weights = c(1:18), subset = COV==1)
  s1 <- rate(data = p18, obs = 'OBS', pyrs = 'PYRS', print = 'COV', adjust = 'AGEGROUP', weights = c(1:18), subset = COV==1)
  s2 <- rate(data = p20, obs = 'OBS', pyrs = 'PYRS', print = 'COV', adjust = 'AGEGROUP',  weights = 'world_2000_20of5', subset = COV == 2)
  s3 <- rate(data = p20, obs = 'OBS', pyrs = 'PYRS', print = 'COV', adjust = 'AGEGROUP', weights = 'cohort', subset = COV==1)
  s4 <- rate(data = p20, obs = 'OBS', pyrs = 'PYRS', print = 'COV', adjust = NULL, weights = NULL, subset = AGEGROUP != 1)
  s5 <- rate(data = p20, obs = 'OBS', pyrs = 'PYRS', print = NULL, adjust = NULL, weights = NULL, subset = COV == 1)
  
  
  expect_equal(sum(s1$PYRS), p18[COV==1,sum(PYRS)])
  expect_equal(sum(s2$OBS), p20[COV==2,sum(OBS)])
  expect_equal(sum(s3$OBS), p20[COV==1,sum(OBS)])
  expect_equal(s4$rate, p20[AGEGROUP!= 1,list(sum(OBS)/sum(PYRS)), by ='COV'][, V1])
  expect_equal(s5$rate, p20[COV==1,list(sum(OBS)/sum(PYRS))][, V1])
  expect_is(s3, 'rate')
})


test_that("warnings and stops works properly", {
  expect_error(
    rate(data = p18, obs = 'OBS', pyrs = 'PYRS', print = 'COV', adjust = 'AGEGROUP', weights = list(1:18, 2:19))
    )
  expect_error( stdr.weights(c('wold00_1','world66_5')) )
  expect_error( stdr.weights(c('wold00_20of5')) )
})



test_that("stdr.weights returns correct datasets", {
  al <- c('world_1966_18of5','europe','nordic',
          "world_2000_18of5","world_2000_101of1", 
          "world_2000_20of5")
  le <- c(18,18,18,18,101,20)
  expect_equal( stdr.weights(al[1])[,.N], le[1])
  expect_equal( stdr.weights(al[2])[,.N], le[2])
  expect_equal( stdr.weights(al[3])[,.N], le[3])
  expect_equal( stdr.weights(al[4])[,.N], le[4])
  expect_equal( stdr.weights(al[5])[,.N], le[5])
  expect_equal( stdr.weights(al[6])[,.N], le[6])
})
