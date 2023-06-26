#' Autokey Cipher
#'
#' @description The Autokey Cipher is derived from the Vigenere Ciphere where the key and plaintext are bound to generate a new
#' encryption key as an input for the Vigenere method.
#'
#' @param message A character vector composed of a-z alphabetic characters
#' @param key An encryption key composed of a-z alphabetic characters
#' @param encrypt (Default: `TRUE`) TRUE will encrypt the message, while FALSE will decrypt the message.
#'
#' @return A character vector of either plaintext that has been encrypted or ciphertext that has been decrypted.
#' @export
#'
#' @examples autokey("VerySecretMessage", "Hack", encrypt = TRUE)
autokey <- function(message, key, encrypt=TRUE)
{
  if (encrypt == TRUE)
  {
    message <- autokey_encrypt(message, key)
  }
  if (encrypt == FALSE)
  {
    message <- autokey_decrypt(message, key)
  }
  return(message)
}


autokey_encrypt <- function(message, key) {

  # creating the encryption key by binding the key to the message and making it the length of the message
  full.key <- paste(c(strsplit(key, "")[[1]], strsplit(message, "")[[1]])[1:nchar(message)], collapse = "")

  # applying the Vigenere method to the message with the newly formed encryption key
  encrypted.text <- DescTools::Vigenere(message, full.key)
}


autokey_decrypt <- function(x, key) {

}
