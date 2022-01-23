# Hello, I hope you do not find it difficult to qualify my work.
# My English is not good enough to give me the confidence to write notes in that language.
# If you don't know Spanish, I would recommend using the translator.



# Nos pidieron calcular la matriz inversa de una matriz
#Como mencionaba la asigancion, por lo complejo que puede ser calcular esto usa muchos recursos y lo mejor evitar que eso pase

# La funcion tendra como objetivo crear una matriz en la memoria que nos ayude a obtener la inversa
# La funcion MatrizMemoria es muy parecida a la funcion de ejemplo que nos dan en el curso
makeCacheMatrix <- function(x = matrix()) {
  inversa <- NULL
  poner <- function(y) {
    x <<- y
    inversa <<- NULL
  }
  obtener <- function() x
  ponerInversa <- function(inversa1) inversa <<- inversa1
  obtenerInversa <- function() inversa
  list(poner = poner,
       obtener = obtener,
       ponerInversa = ponerInversa,
       obtenerInversa = obtenerInversa)
}


# Es es la funcion que se encargara de realizar el calculo de la inversa
# If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inversa <- x$obtenerInversa()
  if (!is.null(inversa)) {
    return(inversa)
  }
  mates <- x$get()
  inversa <- solve(mates, ...)
  x$ponerInversa(inversa)
  inversa
}









