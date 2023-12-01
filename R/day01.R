#' Day 01: Trebuchet?!
#'
#' [Trebuchet?!](https://adventofcode.com/2023/day/1)
#'
#' @name day01
#' @rdname day01
#' @details
#'
#' **Part One**
#'
#' Something is wrong with global snow production, and you\'ve been
#' selected to take a look. The Elves have even given you a map; on it,
#' they\'ve used stars to mark the top fifty locations that are likely to
#' be having problems.
#'
#' You\'ve been doing this long enough to know that to restore snow
#' operations, you need to check all *fifty stars* by December 25th.
#'
#' Collect stars by solving puzzles. Two puzzles will be made available on
#' each day in the Advent calendar; the second puzzle is unlocked when you
#' complete the first. Each puzzle grants *one star*. Good luck!
#'
#' You try to ask why they can\'t just use a [weather machine](/2015/day/1)
#' (\"not powerful enough\") and where they\'re even sending you (\"the
#' sky\") and why your map looks mostly blank (\"you sure ask a lot of
#' questions\")
#' [and]{title="My hope is that this abomination of a run-on sentence somehow conveys the chaos of being hastily loaded into a trebuchet."}
#' hang on did you just say the sky (\"of course, where do you think snow
#' comes from\") when you realize that the Elves are already loading you
#' into a
#' [trebuchet](https://en.wikipedia.org/wiki/Trebuchet){target="_blank"}
#' (\"please hold still, we need to strap you in\").
#'
#' As they\'re making the final adjustments, they discover that their
#' calibration document (your puzzle input) has been *amended* by a very
#' young Elf who was apparently just excited to show off her art skills.
#' Consequently, the Elves are having trouble reading the values on the
#' document.
#'
#' The newly-improved calibration document consists of lines of text; each
#' line originally contained a specific *calibration value* that the Elves
#' now need to recover. On each line, the calibration value can be found by
#' combining the *first digit* and the *last digit* (in that order) to form
#' a single *two-digit number*.
#'
#' For example:
#'
#'     1abc2
#'     pqr3stu8vwx
#'     a1b2c3d4e5f
#'     treb7uchet
#'
#' In this example, the calibration values of these four lines are `12`,
#' `38`, `15`, and `77`. Adding these together produces *`142`*.
#'
#' Consider your entire calibration document. *What is the sum of all of
#' the calibration values?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f01a(x)` returns .... For Part Two,
#'   `f01b(x)` returns ....
#' @export
#' @examples
#' f01a(example_data_01())
#' f01b()
f01a <- function(x) {

  calibration_values <- purrr::map_dbl(x, f01_helper)

  sum(calibration_values)

}


#' @rdname day01
#' @export
f01b <- function(x) {

  int_codes <- purrr::map_dbl(x, f01b_helper)

  out <- sum(int_codes)

  return(out)

}



f01_helper <- function(x) {


  first_digit <- as.numeric(regmatches(x, regexpr("\\d", x)))

  x_reversed <- paste(rev(strsplit(x, split = "")[[1]]), collapse = "")

  second_digit <- as.numeric(regmatches(x_reversed, regexpr("\\d", x_reversed)))

  out <- first_digit*10+second_digit

  return(out)

}

f01b_helper <- function(x) {

  elements <- unlist(stringr::str_match_all(x, "(?=(one|two|three|four|five|six|seven|eight|nine|[1-9]))")[[1]][, 2])

  elements <- c(head(elements, 1), tail(elements, 1))

  integers <- as.numeric(word2num(elements))

  int_code <- integers[1]*10+integers[2]

  return(int_code)

}

word2num <- function(x) {


  x <- gsub("one",1,x)
  x <- gsub("two",2,x)
  x <- gsub("three",3,x)
  x <- gsub("four",4,x)
  x <- gsub("five",5,x)
  x <- gsub("six",6,x)
  x <- gsub("seven",7,x)
  x <- gsub("eight",8,x)
  x <- gsub("nine",9,x)

  return(x)

}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day01
#' @export
example_data_01 <- function(example = 1) {
  l <- list(
    a = c(


    )
  )
  l[[example]]
}
