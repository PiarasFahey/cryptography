#' Autokey Cipher
#'
#' @description This can be used to encrypt or decrypt an Autokey cipher. The Autokey Cipher is derived from the Vigenere Cipher,
#'  in which the key and plaintext are bound to generate a new encryption key for the Vigenere method.
#'  This Vigenere method uses only letters and number, as such any other characters used as inputs are not used in the cipher.
#'
#' @param message A character vector of plaintext to be encrypted or ciphertext to be decrypted
#' @param key A character vector to be used as the encryption key
#' @param encrypt (Default: `TRUE`) TRUE will encrypt the message, while FALSE will decrypt the message.
#'
#' @return A character vector of either plaintext that has been encrypted or ciphertext that has been decrypted.
#' @export
#'
#' @examples
#' autokey("VerySecretMessage", "Hack", encrypt = TRUE)
#' autokey("c4JYn8JfwNoLMbmAM", "Hack", encrypt = FALSE)
#' autokey("Very $%^&SecretMes(*sag£$%e", "Hack", encrypt = TRUE)
#'
autokey <- function(message, key, encrypt=TRUE)
{
  # stop if message is not a character vector
  if (!is.character(message) || !is.vector(message) || length(message) != 1) {
    stop("message must be a character vector")
  }

  # stop if the key is not a character vector
  if (!is.character(key) || !is.vector(key) || length(key) != 1) {
    stop("key must be a character vector")
  }

  # stop if encrypt is not boolean
  if (!is.logical(encrypt)) {
    stop("encrypt must be TRUE or FALSE")
  }

  # changing message to only valid characters (all letters and numbers)
  message <- gsub("[^A-Za-z0-9]", "", message)

  # changing key to only valid characters (all letters and numbers)
  key <- gsub("[^A-Za-z0-9]", "", key)

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

