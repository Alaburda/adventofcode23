#' Day 02: Cube Conundrum
#'
#' [Cube Conundrum](https://adventofcode.com/2023/day/2)
#'
#' @name day02
#' @rdname day02
#' @details
#'
#' **Part One**
#'
#' You\'re launched high into the atmosphere! The apex of your trajectory
#' just barely reaches the surface of a large island floating in the sky.
#' You gently land in a fluffy pile of leaves. It\'s quite cold, but you
#' don\'t see much snow. An Elf runs over to greet you.
#'
#' The Elf explains that you\'ve arrived at *Snow Island* and apologizes
#' for the lack of snow. He\'ll be happy to explain the situation, but
#' it\'s a bit of a walk, so you have some time. They don\'t get many
#' visitors up here; [would you like to play a
#' game]{title="No, the Elf's name is not 'WOPR'. It's Joshua."} in the
#' meantime?
#'
#' As you walk, the Elf shows you a small bag and some cubes which are
#' either red, green, or blue. Each time you play this game, he will hide a
#' secret number of cubes of each color in the bag, and your goal is to
#' figure out information about the number of cubes.
#'
#' To get information, once a bag has been loaded with cubes, the Elf will
#' reach into the bag, grab a handful of random cubes, show them to you,
#' and then put them back in the bag. He\'ll do this a few times per game.
#'
#' You play several games and record the information from each game (your
#' puzzle input). Each game is listed with its ID number (like the `11` in
#' `Game 11: ...`) followed by a semicolon-separated list of subsets of
#' cubes that were revealed from the bag (like `3 red, 5 green, 4 blue`).
#'
#' For example, the record of a few games might look like this:
#'
#'     Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
#'     Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
#'     Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
#'     Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
#'     Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
#'
#' In game 1, three sets of cubes are revealed from the bag (and then put
#' back again). The first set is 3 blue cubes and 4 red cubes; the second
#' set is 1 red cube, 2 green cubes, and 6 blue cubes; the third set is
#' only 2 green cubes.
#'
#' The Elf would first like to know which games would have been possible if
#' the bag contained *only 12 red cubes, 13 green cubes, and 14 blue
#' cubes*?
#'
#' In the example above, games 1, 2, and 5 would have been *possible* if
#' the bag had been loaded with that configuration. However, game 3 would
#' have been *impossible* because at one point the Elf showed you 20 red
#' cubes at once; similarly, game 4 would also have been *impossible*
#' because the Elf showed you 15 blue cubes at once. If you add up the IDs
#' of the games that would have been possible, you get *`8`*.
#'
#' Determine which games would have been possible if the bag had been
#' loaded with only 12 red cubes, 13 green cubes, and 14 blue cubes. *What
#' is the sum of the IDs of those games?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f02a(x)` returns .... For Part Two,
#'   `f02b(x)` returns ....
#' @export
#' @examples
#' f02a(example_data_02())
#' f02b()
f02a <- function(x) {




}


#' @rdname day02
#' @export
f02b <- function(x) {

  pulls <- purrr::map_df(x, f02_parser)

  out <- pulls %>%
    dplyr::group_by(game_id,pull_color) %>%
    dplyr::summarise(fewest_cubes = max(pull_amount)) %>%
    dplyr::summarise(cube_product = prod(fewest_cubes)) %>%
    dplyr::summarise(cube_sum = sum(cube_product))

  return(out)

}


f02_parser <- function(x) {

  game_split <- stringr::str_split(x, ": ")
  game_number <- as.numeric(gsub("Game ","",game_split[[1]][1]))
  pull_matrix <- stringr::str_split(stringr::str_split(game_split[[1]][2], "; ")[[1]],", ", simplify = TRUE)
  pulls_wide <- data.frame(game_id = game_number, pull_id = 1:nrow(pull_matrix), pull_matrix)

  pulls <- pulls_wide %>%
    tidyr::pivot_longer(names_to = "tmp", values_to = "pull_string", cols = -c(game_id,pull_id)) %>%
    tidyr::separate(pull_string, c("pull_amount","pull_color"," ")) %>%
    dplyr::mutate(pull_amount = as.numeric(pull_amount)) %>%
    dplyr::select(game_id,pull_id,pull_color,pull_amount) %>%
    dplyr::filter(!is.na(pull_amount))

  return(pulls)

}

f02_pull_checker <- function(x, color_limit = list(red = 12, green = 13, blue = 14)) {

  pulls <- purrr::map_df(x, f02_parser)

  pulls$limit <- unlist(color_limit[match(pulls$pull_color, names(color_limit))])

  out <- pulls %>%
    dplyr::group_by(game_id) %>%
    dplyr::filter(all(pull_amount <= limit)) %>%
    dplyr::pull(game_id) %>%
    unique() %>%
    sum()

  return(out)

}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day02
#' @export
example_data_02 <- function(example = 1) {
  l <- list(
    a = c(


    )
  )
  l[[example]]
}
