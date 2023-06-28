
#' Four-Square Cipher
#'
#' @description The Four-Square cipher is a polyalphabetic substitution cipher that maps digraphs of
#' some plaintext to two encryption matrices through their position in a square alphabet matrix.
#'
#'
#' @param message a character vector used as the plaintext to be encrypted or the ciphertext to be decrypted
#' @param key1 a character vector used as the encryption key for the first encryption matrix
#' @param key2 a character vector used as the encryption key for the second encryption matrix
#' @param encrypt (Default: `TRUE`) TRUE will encrypt the message, while FALSE will decrypt the message.
#'
#' @return A character vector of either plaintext that has been encrypted or ciphertext that has been decrypted.
#' @export
#'
#' @examples four_square("supersecretmessage", "safety", "first", encrypt = TRUE)
four_square <- function(message, key1, key2, encrypt = TRUE) {

  # generating encryption matrices
  encryption.matrix1 <- KeyMatrix(key1)
  encryption.matrix2 <- KeyMatrix(key2)

  # generating square matrix
  square.matrix <- matrix(LETTERS[!LETTERS == "J"], ncol = 5, nrow = 5, byrow = TRUE)

  # converting the message into it's text digraphs
  digraphs <- square_digraph(message)

  # storing the dimensions of the digraphs position in the square matrix
  digraphs.row <- vector("list", length(digraphs))
  digraphs.col <- vector("list", length(digraphs))

  # calling encryption method
  if (encrypt == TRUE) {

    # inputting dimensions of digraphs positions from the square matrix
    for (i in 1:length(digraphs)) {
      digraphs.row[[i]] <- c(which(square.matrix == digraphs[[i]][1], arr.ind=TRUE)[1],
                             which(square.matrix == digraphs[[i]][2], arr.ind=TRUE)[1])
    }

    for (i in 1:length(digraphs)) {
      digraphs.col[[i]] <- c(which(square.matrix == digraphs[[i]][1], arr.ind=TRUE)[2],
                             which(square.matrix == digraphs[[i]][2], arr.ind=TRUE)[2])
    }

    # creating object to store ciphertext
    ciphertext <- vector("character", length(digraphs)*2)

    for (i in 1:length(digraphs)) {
      ciphertext[(i*2)-1] <- encryption.matrix1[digraphs.row[[i]][1], digraphs.col[[i]][2]]
      ciphertext[i*2] <- encryption.matrix2[digraphs.row[[i]][2], digraphs.col[[i]][1]]
    }
    output <- paste(ciphertext, collapse = "")
  }

  # calling decryption method
  if (encrypt == FALSE) {

    # inputting dimensions of digraphs positions in the corresponding encryption matrix
    for (i in 1:length(digraphs)) {
      digraphs.row[[i]] <- c(which(encryption.matrix1  == digraphs[[i]][1], arr.ind=TRUE)[1],
                             which(encryption.matrix2 == digraphs[[i]][2], arr.ind=TRUE)[1])
    }

    for (i in 1:length(digraphs)) {
      digraphs.col[[i]] <- c(which(encryption.matrix1  == digraphs[[i]][1], arr.ind=TRUE)[2],
                             which(encryption.matrix2 == digraphs[[i]][2], arr.ind=TRUE)[2])
    }

    # creating object to store plaintext
    plaintext <- vector("character", length(digraphs)*2)

    for (i in 1:length(digraphs)) {
      plaintext[(i*2)-1] <- square.matrix[digraphs.row[[i]][1], digraphs.col[[i]][2]]
      plaintext[i*2] <- square.matrix[digraphs.row[[i]][2], digraphs.col[[i]][1]]
    }
    output <- paste(plaintext, collapse = "")
  }
  return(output)
}


# function to convert input text into digraphs for square cipher
square_digraph <- function(message) {

  # only taking [a-zA-Z] characters
  message <- toupper(gsub("[^A-Za-z]", "", message))

  # splitting into individual strings
  characters <- strsplit(message,"")[[1]]

  # replacing "J"s with "I"s
  characters[characters == "J"] <- "I"

  # adding an "X" if the number of characters is odd
  if (length(characters)%%2) {
    characters <- c(characters, "X")
  }

  # putting the resulting characters into pairs
  digraphs <- vector("list", length(characters)/2)
  for (i in 1:(length(characters)/2)) {
    digraphs[[i]] <- c(characters[i*2-1], characters[i*2])
  }
  return(digraphs)
}
