#' Columnar Transposition Cipher
#'
#' @param message A character vector
#' @param key A character vector used as the encryption key
#' @param encrypt (Default: `TRUE`) TRUE will encrypt the message, while FALSE will decrypt the message.
#'
#' @return A character vector of either plaintext that has been encrypted or ciphertext that has been decrypted.
#' @export
#'
#' @examples columnar_transposition("Hidden message", "hack", encrypt = TRUE)

columnar_transposition <- function(message, key, encrypt=TRUE) {

  if (encrypt == TRUE) {
    message <- columnar_transposition_encrypt(message, key)
  }
  if (encrypt == FALSE) {
    message <- columnar_transposition_decrypt(message, key)
  }
  return(message)
}


  columnar_transposition_encrypt <- function(message, key)
  {
    x <- strsplit(message, "")[[1]]
    y <- nchar(key)
    z <- strsplit(tolower(key), "")[[1]]
    if (length(x) %% y != 0)
    {
      for (i in 1:(y-(length(x) %% y)))
      {
        x <- append(x, "")
      }
    }
    x.mat <- data.frame(matrix(x, ncol = y, byrow = TRUE))
    colnames(x.mat) <- z
    x.mat <- as.matrix(x.mat[,order(names(x.mat))])
    encrypted.message <- paste(t(x.mat), collapse = "")
  }

  columnar_transposition_decrypt <- function(message, key)
  {
    x <- strsplit(message, "")[[1]]
    y <- nchar(key)
    z <- strsplit(tolower(key), "")[[1]]
    if (length(x) %% y != 0)
    {
      j <- c()
      for (i in (((length(x)%%y)+1):y))
      {
        j[y-i+1] <- (which(order(z)==i))
      }
      for (i in (1:length(j)))
      {
        x <- append(x, "", after = (floor(length(x)/y)*y+j[i]-1) )
      }
    }
    x.mat <- t(matrix(x, ncol = y, byrow = TRUE))
    row.names(x.mat) <- order(z)
    x.mat <- x.mat[order(row.names(x.mat)), ]
    encrypted.message <- paste((x.mat), collapse = "")
  }

