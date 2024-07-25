#' Compute (x^y)^z Using Two Recursive Calls
#'
#' This function calculates (x^y)^z using recursion.
#'
#' @param x Base value.
#' @param y Exponent for the base.
#' @param z Exponent for the result of (x^y).
#' @return The result of (x^y)^z.
#' @examples
#' power_of_power_recursive(x = 2, y = 3, z = 2)
#' power_of_power_recursive(x = 5, y = 2, z = 3)
power_of_power_recursive <- function(x, y, z) {
  # Función recursiva para calcular x^y
  power_recursive <- function(base, exponent) {
    if (exponent == 0) {
      return(1)
    } else if (exponent > 0) {
      return(base * power_recursive(base, exponent - 1))
    } else {
      stop("Exponent should be a non-negative integer.")
    }
  }

  # Caso base para (x^y)^z
  if (z == 0) {
    return(1)  # Cualquier número elevado a la potencia 0 es 1
  } else if (z > 0) {
    intermediate_result <- power_recursive(x, y)  # Calcula x^y
    return(power_recursive(intermediate_result, z))  # Luego eleva el resultado a z
  } else {
    stop("Exponent z should be a non-negative integer.")
  }
}

