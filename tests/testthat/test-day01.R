test_that("calibration value decoding works", {

  expect_equal(  f01a(c("1abc2","pqr3stu8vwx","a1b2c3d4e5f","treb7uchet")), 142)

})

test_that("f01b_helper works", {

  expect_equal(  f01b_helper(c("1abc2one")), 11)

})


test_that("f01b works", {

  expect_equal(  f01b(c("1abc2one","xbhcfqvplfive9one77","jjn1drdffhs")), 79)
  expect_equal(  f01b(c("eightsevenvqvzlqxkbm6rqhsgqpnine7twonex")), 81)


})

