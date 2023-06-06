#' Scytale cipher
#'
#' @description This can be used to create (encrypt) and solve (decrypt) a
#'   Scytale Cipher. A Scytale Cipher is an ancient form of cryptography that wraps a message (typically written
#'   on a long thing piece of paper) around a device to create a matrix with a fixed number of columns that transposes
#'   the text.
#'
#'
#' @param message A character vector
#' @param col A positive integer, this determines the number of columns in the encryption matrix. 1 column will have no effect
#' @param encrypt (Default: `TRUE`) TRUE will encrypt the message, while FALSE will decrypt the message.
#'
#' @return A character vector of either plaintext that has been encrypted or ciphertext that has been decrypted.
#' @export
#'
#' @examples scytale("very secret message!", col = 4, encrypt = TRUE)
scytale <- function(message, col, encrypt=TRUE) {

  # stop if message is not a character vector
  stopifnot(inherits(message,"character"))

  # stop if cols is not a positive integer
  if (col %% 1 != 0 | col < 1) {
    stop("col must be a single integer greater than or equal to 1")
  }

  # calling the encryption or decryption method
  if (encrypt == TRUE) {
    message <- scytale_encrypt(message, col)
  }
  else if (encrypt == FALSE) {
    message <- scytale_decrypt(message, col)
  }
  return(message)
}


scytale_encrypt <- function(message, col) {

  # splitting the message into individual characters
  substrings <- strsplit(message, "")[[1]]

  # if the length of the message is not divisible by the number of columns must append with empty values to fill matrix
  if (length(substrings) %% col != 0) {
    for (i in 1:(col-(length(substrings) %% col))) {
      substrings <- append(substrings, "", after = length(substrings))
    }
  }

  # filling the cipher matrix with the individual characters
  cipher.matrix <- matrix(substrings, ncol = col, byrow = TRUE)

  # paste encrypted message from cipher matrix
  paste(cipher.matrix, collapse = "")
}


scytale_decrypt <- function(message, col) {

  # splitting the message into individual characters
  substrings <- strsplit(message, "")[[1]]

  # if the length of the message is not divisible by the number of columns must append with empty values in the correct
  # places to fill matrix
  if (length(substrings) %% col != 0) {
    for (i in (length(substrings) %% col):(col-1)) {
      substrings <- append(substrings, "", after = ((i+1)*(ceiling(length(substrings)/col)))-1)
    }
  }

  # filling the decryption matrix with the substrings
  decrypted.matrix <- matrix(substrings, ncol = col, byrow = FALSE)

  # paste decrypted message from decrypted matrix
  paste(t(decrypted.matrix), collapse = "")
}
