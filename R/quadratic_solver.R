#' Quadratic Equation Solver
#'
#' Finds roots of quadratic equations of the form ax^2 + bx + c = 0
#'
#' @author Greg Pilgrim \email{gpilgrim2670@@gmail.com}
#'
#' @param a the quadratic coefficient
#' @param b the linear coefficent
#' @param c a constant
#' @return the area of a trapezoid defined by \code{a}, \code{b} and \code{h}
#'
#' @examples
#' quadratic_solver(1, -3, -10)
#` quadratic_solver(1, -18, 45)


quadratic_solver <- function(a,b,c){
  if(b^2 - (4 * a *c ) > 0){ # real discriminant, b^2 - 4*a*c
    root_1 <- (-b + sqrt(b^2 - (4*a*c)))/(2*a)
    root_2 <- (-b - sqrt(b^2 - (4*a*c)))/(2*a)
    roots <- c(root_1, root_2)
    pos_root <- roots[sign(roots) == 1]
    return(pos_root)
  }
  else if((b^2 - (4 * a *c )) == 0){ # discriminant = 0
    root <- -b/(2*a)
    return(root)
  }
  else {"No real roots"} # discriminant < 0
}
