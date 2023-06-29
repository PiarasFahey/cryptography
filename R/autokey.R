#' Autokey Cipher
#'
#' @description The Autokey Cipher is derived from the Vigenere Cipher where the key and plaintext are bound to generate a new
#' encryption key for the Vigenere method. This Vigenere method uses only letters and number, as such only these
#' characters may be used as inputs for this Autokey cipher.
#'
#' @param message A character vector composed of a-z alphabetic characters and 0-9 numbers
#' @param key An encryption key composed of a-z alphabetic characters and 0-9 numbers
#' @param encrypt (Default: `TRUE`) TRUE will encrypt the message, while FALSE will decrypt the message.
#'
#' @return A character vector of either plaintext that has been encrypted or ciphertext that has been decrypted.
#' @export
#'
#' @examples autokey("VerySecretMessage", "Hack", encrypt = TRUE)

autokey <- function(message, key, encrypt=TRUE)
{
  # stop if message is not a character vector
  if (!is.character(message) || !is.vector(message) || any(grepl("[^a-zA-Z0-9]", message))) {
    stop("message must be a character vector with only letters and numbers")
  }

  # stop if the key is not a character vector
  if (!is.character(key) || !is.vector(key) || any(grepl("[^a-zA-Z0-9]", key))) {
    stop("key must be a character vector with only letters and numbers")
  }

  # stop if encrypt is not boolean
  if (!is.logical(encrypt)) {
    stop("encrypt must be TRUE or FALSE")
  }

  # calling encrypt method
  if (encrypt == TRUE)
  {
    # creating the encryption key by binding the key to the message and making it the length of the message
    full.key <- paste(c(strsplit(key, "")[[1]], strsplit(message, "")[[1]])[1:nchar(message)], collapse = "")

    # applying the Vigenere method to the message with the newly formed encryption key
    message <- DescTools::Vigenere(message, full.key)
  }

  # calling decrypt method
  if (encrypt == FALSE) {

    # calculating necessary number of iterations of decryption
    iterations <- ceiling(nchar(message)/nchar(key))

    # creating text blocks object to decrypt by length of key
    text.blocks <- vector("character", iterations)

    # if only 1 iteration then there will be a single text block of the message
    if (iterations == 1) {
      text.blocks <- message
    }

    if (iterations > 1) {
      for (i in 1:iterations-1) {
      text.blocks[i] <- paste(strsplit(message, "")[[1]][(1+(i-1)*nchar(key)):(i*nchar(key))], collapse = "")
      }
      text.blocks[iterations] <- paste(strsplit(message, "")[[1]][(1+(iterations-1)*nchar(key)):nchar(message)], collapse = "")
    }

    # creating decrypted blocks object to fill with decryption iterations
    decrypted.blocks <- vector("character", iterations)

    # decrypting first block of cipher text and putting into first block
    decrypted.blocks[1] <- DescTools::Vigenere(text.blocks[1], key, decrypt = TRUE)

    if (iterations > 1) {
      for (i in 2:iterations) {
      decrypted.blocks[i] <- DescTools::Vigenere(text.blocks[i], decrypted.blocks[i-1], decrypt = TRUE)
      }
    }
    message <- paste(decrypted.blocks, collapse = "")
  }
  return(message)
}

