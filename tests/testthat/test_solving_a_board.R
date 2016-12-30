context("Testing solve: ~2 min... < 3 min standard game")
print('')
set.seed(100)

test_that("Test solving a board", {

  board        <- random_board()
  # TODO - extract this code into a "solve" function
  # that takes a board and returns a data frame with the
  # results.  Also extract out a "score_board" function.
  padded_board <- cbind(NA,rbind(NA,board,NA),NA)
  nb           <- neighbors(padded_board)
  results      <- list(words=c(),indices=c())
  df           <- NULL

  print(paste(Sys.time(),' - Starting'))
  for (idx in 1:16){
    results <- word_search(idx, board[idx], c(idx), results, board, nb) # Solve each cell
    if (idx==1){
      df <- data.frame(results$words, results$word_indices)
    }else
      df <- rbind(df, data.frame(results$words, results$word_indices))
  }
  print(paste(Sys.time(),' - Complete'))

  # Cleanup... calculate score for all rows
  df$score <- sapply(df$results.words, function(word) score_word(as.character(word)))
  # Order by score in decending order
  df<-unique(df[order(c(-df$score, df$results.words)),])
  # Remove NAs
  df<-df[!is.na(df$results.words),]

  #Sum up the scrore - excluding duplicate words spelled using different cells
  total_score <- sum(unique(df[,c(1,3)])$score)

  expect_equal(58, total_score)
  expect_equal(1,  length(df$results.words[df$results.words=='BROIL']))
  expect_equal(1,  length(df$results.words[df$results.words=='BIAS']))

})

