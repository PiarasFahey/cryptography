
#' Playfair Cipher
#'
#' @description This can used to create (encrypt) or solve (decrypt) a Playfair cipher.
#'
#'
#' @param message a character vector to be encrypted or decrypted
#' @param key a character vector to be used as the encryption key
#' @param encrypt (Default: `TRUE`) TRUE will encrypt the message, while FALSE will decrypt the message.
#'
#' @return A character vector of either plaintext that has been encrypted or ciphertext that has been decrypted.
#' @export
#'
#' @examples playfair("super secret message", "safety", encrypt = TRUE)
playfair <- function(message, key, encrypt = TRUE) {

  # generating the encryption matrix from the key input
  encryption.matrix <- KeyMatrix(key)

  # converting the message input to digraphs under necessary playfair conditions
  digraphs <- playfair_digraph(message)

  # storing the dimensions of the digraphs elements' position in the encryption matrix
  digraphs.row <- vector("list", length(digraphs))
  for (i in 1:length(digraphs)) {
    digraphs.row[[i]] <- c(which(encryption.matrix == digraphs[[i]][1], arr.ind=TRUE)[1],
                           which(encryption.matrix == digraphs[[i]][2], arr.ind=TRUE)[1])
  }

  digraphs.col <- vector("list", length(digraphs))
  for (i in 1:length(digraphs)) {
    digraphs.col[[i]] <- c(which(encryption.matrix == digraphs[[i]][1], arr.ind=TRUE)[2],
                           which(encryption.matrix == digraphs[[i]][2], arr.ind=TRUE)[2])
  }

  # calling encryption method
  if (encrypt == TRUE) {

    # creating object to store ciphertext
    ciphertext <- vector("character", length(digraphs)*2)

    for (i in 1:length(digraphs)) {

      # algorithm for when rows and columns are both unequal
      if (digraphs.row[[i]][1] != digraphs.row[[i]][2] &
          digraphs.col[[i]][1] != digraphs.col[[i]][2]) {
        ciphertext[(i*2)-1] <- encryption.matrix[digraphs.row[[i]][1], digraphs.col[[i]][2]]
        ciphertext[i*2] <- encryption.matrix[digraphs.row[[i]][2], digraphs.col[[i]][1]]
      }

      # algorithm for when rows are equal and columns are unequal
      if (digraphs.row[[i]][1] == digraphs.row[[i]][2] &
          digraphs.col[[i]][1] != digraphs.col[[i]][2]) {
        ciphertext[(i*2)-1] <- encryption.matrix[digraphs.row[[i]][1], (digraphs.col[[i]][1]%%5) + 1]
        ciphertext[i*2] <- encryption.matrix[digraphs.row[[i]][2], (digraphs.col[[i]][2]%%5)+1]
      }

      # algorithm for when rows are unequal and columns are equal
      if (digraphs.row[[i]][1] != digraphs.row[[i]][2] &
          digraphs.col[[i]][1] == digraphs.col[[i]][2]) {
        ciphertext[(i*2)-1] <- encryption.matrix[(digraphs.row[[i]][1]%%5)+1, digraphs.col[[i]][1]]
        ciphertext[i*2] <- encryption.matrix[(digraphs.row[[i]][2]%%5)+1, digraphs.col[[i]][2]]
      }
    }
    output <- paste(ciphertext, collapse = "")
  }

  # calling decryption method
  if (encrypt == FALSE) {

    # creating object to store plaintext
    plaintext <- vector("character", length(digraphs)*2)

    for (i in 1:length(digraphs)) {

      # algorithm for when rows and columns are both unequal
      if (digraphs.row[[i]][1] != digraphs.row[[i]][2] &
          digraphs.col[[i]][1] != digraphs.col[[i]][2]) {
        plaintext[(i*2)-1] <- encryption.matrix[digraphs.row[[i]][1], digraphs.col[[i]][2]]
        plaintext[i*2] <- encryption.matrix[digraphs.row[[i]][2], digraphs.col[[i]][1]]
      }

      # algorithm for when rows are equal and columns are unequal
      if (digraphs.row[[i]][1] == digraphs.row[[i]][2] &
          digraphs.col[[i]][1] != digraphs.col[[i]][2]) {
        plaintext[(i*2)-1] <- encryption.matrix[digraphs.row[[i]][1], ((digraphs.col[[i]][1]-2)%%5+1)]
        plaintext[i*2] <- encryption.matrix[digraphs.row[[i]][2], ((digraphs.col[[i]][2]-2)%%5+1)]
      }

      # algorithm for when rows are unequal and columns are equal
      if (digraphs.row[[i]][1] != digraphs.row[[i]][2] &
          digraphs.col[[i]][1] == digraphs.col[[i]][2]) {
        plaintext[(i*2)-1] <- encryption.matrix[((digraphs.row[[i]][1]-2)%%5+1), digraphs.col[[i]][1]]
        plaintext[i*2] <- encryption.matrix[((digraphs.row[[i]][2]-2)%%5+1), digraphs.col[[i]][2]]
      }
    }
    output <- paste(plaintext, collapse = "")
  }
  return(output)
}

# Function to generate the key matrix from the given key
KeyMatrix <- function(key) {
  # taking only A-Za-z characters and changing them to uppercase
  key <- toupper(gsub("[^A-Za-z]", "", key))

  # creating string with all letters in order of input to key matrix
  key <- c(strsplit(key, "")[[1]], toupper(letters))

  # replacing J with I as necessary for 5x5 encryption matrix
  key[key == "J"] <- "I"

  # removing repeated letters
  key <- key[!duplicated(key)]

  # inputting letters into key matrix
  keyMatrix <- matrix(key, nrow=5, ncol=5, byrow = TRUE)

  return(keyMatrix)
}

# function to convert input text to digraphs from playfair conditions
playfair_digraph <- function(message) {
  message <- toupper(gsub("[^A-Za-z]", "", message))

  message.fixed <- strsplit(message,"")[[1]]

  message.fixed[message.fixed == "J"] <- "I"

  repeat {
  for (i in 2:length(message.fixed))
  {
    if (message.fixed[i-1]==message.fixed[i] & i%%2 == 0)
    {
      message.fixed <- c(message.fixed,(""))
      for (j in length(message.fixed):i)
      {
        message.fixed[j] <- message.fixed[j-1]
      }
      if (message.fixed[i-1] != "X") {
        message.fixed[i] <- "X"
      }
      if (message.fixed[i-1] == "X") {
        message.fixed[i] <- "Z"
      }
    }
  }
  if (message.fixed[length(message.fixed)]=="X" & length(message.fixed)%%2!=0) {
    message.fixed <- c(message.fixed,("Z"))
  }
  if (length(message.fixed)%%2!=0) {
    message.fixed <- c(message.fixed,("X"))
  }
  if (message.fixed[i-1] != message.fixed[i] & (i)%%2 == 0) {
    break
  }

  }

  # putting the resulting characters into pairs
  digraphs <- vector("list", length(message.fixed)/2)
  for (i in 1:(length(message.fixed)/2)) {
      digraphs[[i]] <- c(message.fixed[i*2-1],message.fixed[i*2])
  }

  return(digraphs)
}
