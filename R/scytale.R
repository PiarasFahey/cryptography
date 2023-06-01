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
  if (encrypt == TRUE) {
    message <- scytale_encrypt(message, col)
  }
  else if (encrypt == FALSE) {
    message <- scytale_decrypt(message, col)
  }
  return(message)
}


scytale_encrypt <- function(message, col) {
  substrings <- strsplit(message, "")[[1]]
  cipher.matrix <- matrix(substrings, ncol = col, byrow = TRUE)
  if (length(substrings) %% col != 0) {
    for (i in 0:((col-(length(substrings) %% col)-1))) {
    cipher.matrix[1+floor(length(substrings)/col),col-i] <- ""
    }
  }
  encrypted.matrix <- gsub("", "", c(cipher.matrix))
  paste(encrypted.matrix, collapse = "")
}


scytale_decrypt <- function(message, col) {
  substrings <- strsplit(message, "")[[1]]
  decrypted.matrix <- matrix(substrings, ncol = col, byrow = FALSE)
  paste(t(decrypted.matrix), collapse = "")
}
