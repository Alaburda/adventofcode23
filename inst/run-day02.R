library(adventofcode23)
x <- readLines("./inst/input02.txt")

p1 <- f02_pull_checker(x, color_limit = list(red = 12, green = 13, blue = 14))
p2 <- f02b(x)

stopifnot(p1 == aoc_solutions$day02a)
stopifnot(p2 == aoc_solutions$day02b)
