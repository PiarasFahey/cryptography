
#' Four-Square Cipher
#'
#' @description This can be used to encrypt or decrypt a Four-Square cipher. The Four-Square cipher is a polygraphic
#' substitution cipher that maps digrams of text to two encryption matrices through their position in a square alphabet matrix.
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
#' @examples
#' four_square("THEPRISONERSHAVEESCAPED", "HACK", "SAFE", encrypt = TRUE)
#' four_square("SHBOTDTMPFSQDFZSCUHFPBCY", "HACK", "SAFE", encrypt = FALSE)
#'
four_square <- function(message, key1, key2, encrypt = TRUE) {

  # stop if message is not a character vector
  if (!is.character(message) || !is.vector(message) || length(message) != 1) {
    stop("message must be a character vector")
  }

  # stop if the key is not a character vector
  if (!is.character(key1) || !is.vector(key1) || length(key1) != 1) {
    stop("key1 must be a character vector")
  }

  # stop if the key is not a character vector
  if (!is.character(key2) || !is.vector(key2) || length(key2) != 1) {
    stop("key2 must be a character vector")
  }

  # stop if encrypt is not boolean
  if (!is.logical(encrypt)) {
    stop("encrypt must be TRUE or FALSE")
  }

  # generating encryption matrices
  encryption.matrix1 <- KeyMatrix(key1)
  encryption.matrix2 <- KeyMatrix(key2)

  # generating square matrix
  square.matrix <- matrix(LETTERS[!LETTERS == "J"], ncol = 5, nrow = 5, byrow = TRUE)

  # converting the message into it's text digrams
  digrams <- square_digram(message)

  # storing the dimensions of the digrams position in the square matrix
  digrams.row <- vector("list", length(digrams))
  digrams.col <- vector("list", length(digrams))

  # calling encryption method
  if (encrypt == TRUE) {

    # inputting dimensions of digrams positions from the square matrix
    for (i in 1:length(digrams)) {
      digrams.row[[i]] <- c(which(square.matrix == digrams[[i]][1], arr.ind=TRUE)[1],
                             which(square.matrix == digrams[[i]][2], arr.ind=TRUE)[1])
    }

    for (i in 1:length(digrams)) {
      digrams.col[[i]] <- c(which(square.matrix == digrams[[i]][1], arr.ind=TRUE)[2],
                             which(square.matrix == digrams[[i]][2], arr.ind=TRUE)[2])
    }

    # creating object to store ciphertext
    ciphertext <- vector("character", length(digrams)*2)

    for (i in 1:length(digrams)) {
      ciphertext[(i*2)-1] <- encryption.matrix1[digrams.row[[i]][1], digrams.col[[i]][2]]
      ciphertext[i*2] <- encryption.matrix2[digrams.row[[i]][2], digrams.col[[i]][1]]
    }
    output <- paste(ciphertext, collapse = "")
  }

  # calling decryption method
  if (encrypt == FALSE) {

    # inputting dimensions of digrams positions in the corresponding encryption matrix
    for (i in 1:length(digrams)) {
      digrams.row[[i]] <- c(which(encryption.matrix1  == digrams[[i]][1], arr.ind=TRUE)[1],
                             which(encryption.matrix2 == digrams[[i]][2], arr.ind=TRUE)[1])
    }

    for (i in 1:length(digrams)) {
      digrams.col[[i]] <- c(which(encryption.matrix1  == digrams[[i]][1], arr.ind=TRUE)[2],
                             which(encryption.matrix2 == digrams[[i]][2], arr.ind=TRUE)[2])
    }

    # creating object to store plaintext
    plaintext <- vector("character", length(digrams)*2)

    for (i in 1:length(digrams)) {
      plaintext[(i*2)-1] <- square.matrix[digrams.row[[i]][1], digrams.col[[i]][2]]
      plaintext[i*2] <- square.matrix[digrams.row[[i]][2], digrams.col[[i]][1]]
    }
    output <- paste(plaintext, collapse = "")
  }
  return(output)
}


# function to convert input text into digrams for square cipher
square_digram <- function(message) {

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
  digrams <- vector("list", length(characters)/2)
  for (i in 1:(length(characters)/2)) {
    digrams[[i]] <- c(characters[i*2-1], characters[i*2])
  }
  return(digrams)
}
