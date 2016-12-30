# Dev Note: storing data: devtools::use_data(boggle_dice, boggle_words, overwrite = TRUE)
# Dev Note - set up dictionary:
#    curl https://raw.githubusercontent.com/dolph/dictionary/master/ospd.txt > ~/dictionary.txt
#
# This is a list of words, excluding those with capital letters (proper nounds)
# between 3 and 16 characters in length.
#
# boggle_words <-
#  system("egrep '^[[:lower:]]{3,16}$' ~/dictionary.txt", intern=TRUE)
#
#
# Ideas
# - ocr and read real boggle boards
# - render boards graphically ( and spin letters around randomly)
# - Generate and store boards... rank highest scoring boards.
#
# - http://www.hasbro.com/common/instruct/boggle.pdf

# TODO - take out board size function call and use board_size variable
# TODO - document

#'
#' Generate a new boggle board
#'
#' Effectively "shake up" a new boggle board which results in a set
#' of dice being rolled and place in a square grid.
#'
#' @export
#' @param size Dimensions of the boggle board.  Default is 4 x 4.
#' @param dice A data frame containing the sides of each dice used in the game.  Default is the 16 six-sided dice data frame "boggle_dice"
#' @return a matrix containing a new boggle board
#' @examples
#' random_board()
#' random_board(size = 5)
#'
#
random_board <- function(size = 4, dice=boggle_dice){
  slots <- size^2 # Use a square grid as the playing area (4 by default)
  dice_sides <- dim(dice)[2] # Derive the number of faces on each dice (6 by default)
  dice <- dice[rep(seq_len(nrow(dice)), 10), ][1:slots,] # Free additional dice!  In case not enough dice defined - we circle back and create duplicates to fill in the remaining slots
  tmp <- dice[sample(1:slots, slots, replace=FALSE), ] # Dice in different slots
  tmp <- apply(tmp, 1, function(x)x[round(runif(1,1,dice_sides))]) # Roll each dice
  matrix(tmp, nrow=size, ncol=size, byrow=TRUE) # Format results as a matrix
}

#' Board Size
#'
#' Return the size of a square boggle board.
#' @export
#' @param b A square matrix representing a boggle board.
#' @return The size of the board.
#'
#' @examples
#' board <- random_board()
#' board_size(board)
#'
#
board_size <- function (b){
  dim(b)[1]
}

#' Neighbors
#'
#' Returns a matrix representing all neighbors for a given board
#' @export
#' @param padded_board A square matrix representing a boggle board surrounded by NA cells.
#' @return an adjacency matrix where each column represents one
#' cell in the boggle board matrix, and each row represents one
#' of 8 possible adjacent cells.
#'
#' @examples
#' board        <- random_board()
#' padded_board <- cbind(NA,rbind(NA,board,NA),NA)
#' neighbors(padded_board)
#'
#
neighbors = function(padded_board) {

  a <- expand.grid(x = 1:4, y = 1:4)
  results <- c()
  # i and j check + 1 and -1 for each row
  for(i in 1:-1){
    for(j in 1:-1){
      if(i!=0 || j !=0){ # don't look at self,
        results <- rbind(results,padded_board[a$x+i+1+nrow(padded_board)*(a$y+j)])
      }
    }
  }

  return(results)
}

#' Starts with
#'
#' Returns a list of words that have the specified character string
#' at the beginning of the word.
#'
#' @export
#' @param letters The character string being tested.
#' @return A list of words that match the beginning of the character string.
#' @examples
#' starts_with('wow')
starts_with<- function(letters){
  boggle_words[grep(paste0('^',tolower(letters)), boggle_words)]
}

#' Is string a word?
#'
#' Returns a boolean value indicating whether the specified
#' character string is a word
#'
#' @export
#' @param letters The character string being tested.
#' @return A boolean indcating whether or not the character string
#' passed in as an argument is a word.
#' @examples
#' is_word('wow')
is_word <- function(letters){
  length(boggle_words[grep(paste0('^',tolower(letters),'$'), boggle_words)]) > 0
}

#' Index of neighbor
#'
#' This function could probably be minimized or done away with
#' by encoding the data differently when checking neighboring
#' cells.
#'
#' @export
#' @param idx The index of the matrix as a vector
#' starting with the top left and proceeding down then right.
#' @param nr relative cell reference.  If a cell is at position
#' idx, the corresponding cell references are as follows.
#'
#' \tabular{rrrrr}{
#'   8 \tab 7   \tab   6\cr
#'   5 \tab idx \tab   4\cr
#'   3 \tab 2   \tab   1\cr
#' }
#'
#' @param bs Board size.
#' @return Returns a the index of a neighboring cell.
#'
#' @examples
#' board        <- random_board()
#' bs           <- board_size(board)
#'
#' # This example returns 5.  The index of the first cell in the
#' # second column of a 4x4 matrix.  The first arg (idx) is set
#' # to 1, representing the first cell in the first row.  The
#' # second argument (nr) which is set to 4 indicating the cell
#' # to the immediate right (east).  Finally the board size is set
#' # to 4.
#' index_of_neighbor(1, 4, 4)
#'
index_of_neighbor <- function (idx, nr, bs){
  if (nr==1) return(idx + bs + 1)
  if (nr==2) return(idx + 1)
  if (nr==3) return(idx - bs + 1)
  if (nr==4) return(idx + bs)
  if (nr==5) return(idx - bs)
  if (nr==6) return(idx + bs - 1)
  if (nr==7) return(idx - 1)
  if (nr==8) return(idx - bs - 1)
}

#' Word search
#'
#' This function recursively searches for all words available
#' when starting with a given cell.
#'
#' @export
#' @param idx The cell where the character string (fragment) originates.
#' @param fragment A character string being tested.
#' @param indices The indexes of the cells of the character string being tested.
#' @param results A list of words and indices.
#' @param board The scrabble board in play.
#' @param nb The matrix representing neighboring cells.
#' @return A list of words and their indices on the boggle board.
#' @examples
#' # Search for all words originating with the first upper
#' # left-hand cell.
#' board        <- random_board()
#' padded_board <- cbind(NA,rbind(NA,board,NA),NA)
#' nb           <- neighbors(padded_board)
#' results      <- list(words=c(),indices=c())
#' idx          <- 1
#' word_search(idx, board[idx], c(idx), results, board, nb)
#'
word_search <- function(idx, fragment, indices, results, board, nb){
  # TODO indices enough - fragments redundant.
  size <- board_size(board)
  for (i in 1:8){
    ni = index_of_neighbor(idx, i, size)
    if(!is.na(nb[i,idx]) & # Not off the board
       !any(indices==ni)   # Index of letter not already used
    ){
      test_word <- paste0(fragment, board[ni])
      new_indices <- c(indices, ni)

      # TODO modify dictionary to only contain > 2 letter words
      if (is_word(test_word) & nchar(test_word) > 2){
        results$words <- c(results$words, test_word)
        results$word_indices <- c(results$word_indices, paste(new_indices,collapse=','))
      }

      if(length(starts_with(test_word) > 0)){ # Continue searching
        results <- word_search(ni, test_word, new_indices, results, board, nb)
      }
    }
  }
  return(results) # No more words to find
}


#' Score word
#'
#' Returns the score for a given word.
#'
#' Scoring follows standard boggle rules:
#'
#' \itemize{
#'  \item{4 or fewer letters:  }{ 1 point}
#'  \item{5-letter words:      }{ 2 points}
#'  \item{6-letter words:      }{ 3 points}
#'  \item{7-letter words:      }{ 5 points}
#'  \item{7 letters or longer: }{ 11 points}
#' }
#'
#'
#' Also takes into consideration the "Qu" face on the dice which
#' represents a single character.
#' @export
#' @param word The word being scored.
#' @return The score for the word passed in as an argument.
#' @examples
#' score_word('CAT')

score_word <- function(word){
  word <- gsub('qu','q', tolower(word))
  if(nchar(word) < 5){return(1)}
  if(nchar(word) == 5){return(2)}
  if(nchar(word) == 6){return(3)}
  if(nchar(word) == 7){return(5)}
  if(nchar(word) > 7){return(11)}
}

# Wordnet implementation
#is_word <- function(letters){
#  x <- as.numeric(system(paste0('wn ',letters,' |wc -l'), intern=TRUE))
#  x > 8
#}
#
#
# has_word<- function(word, board){
#   word_letters <- unlist(strsplit(toupper(word),''))
# }
#
# # Index for a letter
# vector_index <- function(letter, board){
#   which(board==letter)
# }
#
# matrix_index <- function (letter , board){
#   vi <- vector_index(letter,board)
#   to_matrix_index(vi, board)
# }
#
# # Convert between index types
# to_vector_index <- function(m, board){
#   (m['column',] - 1) * board_size(board) + m['row',]
# }
#
# to_matrix_index <- function(i, board){
#   column <- ceiling(i / board_size(board))
#   row <- ((i / board_size(board)) %% 1) * board_size(board)
#   row[row==0] <- 4
#   results <- mapply(function(x,y){c(x,y)}, row, column)
#   rownames(results) <- c('row', 'column')
#   results
# }
#

#has_adjacent <- function(idx, letter, nb) {
#  which(nb[,idx] %in% letter)
#}
