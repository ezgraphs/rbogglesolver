context("Testing board generation")
set.seed(1)

test_that("4 x 4 board is generated as expected", {

  standard_board <- matrix(data=c(
    "E","D","O","K",
    "C","S","R","P",
    "B","B","L","O",
    "V","I","I","N"), nrow = 4, ncol = 4, byrow = TRUE)

  board <- random_board()
  expect_equal(standard_board, board)
  expect_equal(4, board_size(board))

})

test_that("5 x 5 board is generated as expected", {

  extended_board <- matrix(data=c(
    "N","G","I","T","I",
    "O","R","Y","I","E",
    "K","L","N","O","Y",
    "I","O","E","P","E",
    "O","T","M","E","G"), nrow = 5, ncol = 5, byrow = TRUE)

  board <- random_board(5)
  expect_equal(extended_board, board)
  expect_equal(5, board_size(board))

})

