#' Columnar Transposition Cipher
#'
#' @description This can be used to create (encrypt) or solve (decrypt) a columnar transposition cipher.
#' This method is a development of the Scytale cipher that rearranges the encryption matrix used in the
#' Scytale method by the alphabetical ordering of the encryption key.
#'
#'
#' @param message A character vector
#' @param key An "a-zA-Z" character vector used as the encryption key
#' @param encrypt (Default: `TRUE`) TRUE will encrypt the message, while FALSE will decrypt the message.
#'
#' @return A character vector of either plaintext that has been encrypted or ciphertext that has been decrypted
#' using the columnar transposition cryptographic method.
#' @export
#'
#' @examples columnar_transposition("Hidden message", "hack", encrypt = TRUE)

columnar_transposition <- function(message, key, encrypt=TRUE) {

  # stop if message is not a character vector
  if (!is.character(message) || !is.vector(message)) {
    stop("message must be a character vector")
  }

  # stop if the key is not a character vector
  if (!is.character(key) || any(grepl("[^a-zA-Z]", key))) {
    stop("key must only contain [a-zA-Z] characters")
  }

  # stop if encrypt is not boolean
  if (!is.logical(encrypt)) {
    stop("encrypt must be TRUE or FALSE")
  }

  # isolating message characters and key characters/length
  characters <- strsplit(message, "")[[1]]
  columns <- nchar(key)
  key.chars <- strsplit(tolower(key), "")[[1]]

  # calling encryption method
  if (encrypt == TRUE) {

    # checking if characters fit into the encryption matrix
    if (length(characters) %% columns != 0)
    {
      # adding a blank element to the characters object to ensure encryption matrix is filled correctly
      for (i in 1:(columns-(length(characters) %% columns)))
      {
        characters <- append(characters, "")
      }
    }
    # placing characters into encryption matrix
    encryption.matrix <- data.frame(matrix(characters, ncol = columns, byrow = TRUE))

    # ordering encryption matrix according to key characters order
    colnames(encryption.matrix) <- key.chars
    encryption.matrix <- as.matrix(encryption.matrix[,order(names(encryption.matrix))])

    # pasting the characters from the encryption matrix into the encrypted message
    message <- paste(encryption.matrix, collapse = "")
  }

  # calling decryption method
  if (encrypt == FALSE) {

    # checking if characters fit into the encryption matrix
    if (length(characters) %% columns != 0)
    {
      # creating object to put in values of columns that need an empty cell
      incomplete.cols <- c()
      # finding which columns need an empty cell to be filled
      for (i in (((length(characters)%%columns)+1):columns))
      {
        incomplete.cols[columns-i+1] <- (which(order(key.chars)==i))
      }
      # placing the empty character in the correct place of the characters object
      for (i in (order(incomplete.cols)))
      {
        characters <- append(characters, "", after = (incomplete.cols[i]*ceiling(length(characters)/columns)-1))
      }
    }
    # placing characters object in the encryption matrix
    encryption.matrix <- t(matrix(characters, ncol = columns, byrow = FALSE))

    # ordering the encryption matrix by the key characters
    row.names(encryption.matrix) <- order(key.chars)
    encryption.matrix <- encryption.matrix[order(row.names(encryption.matrix)), ]

    # pasting the characters from the encryption matrix into the decrypted message
    message <- paste(encryption.matrix, collapse = "")
  }
  return(message)
}
