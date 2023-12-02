test_that("f02 parser works", {

  input_string <- "Game 1: 4 red, 5 blue, 4 green; 7 red, 8 blue, 2 green; 9 blue, 6 red; 1 green, 3 red, 7 blue; 3 green, 7 red"

  out <- f02_parser(input_string)

  expect_equal(nrow(out), 13)

})

test_that("f02 checker works", {

  input_string <- "Game 1: 4 red, 5 blue, 4 green; 7 red, 8 blue, 2 green; 9 blue, 6 red; 1 green, 3 red, 7 blue; 3 green, 7 red"

  f02_pull_checker(input_string)


  expect_equal( f02_pull_checker(input_string), 1)

})
