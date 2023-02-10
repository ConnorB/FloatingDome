#' @title Returns Schmidt number for a specific gas at a given temperature
#'
#' @param tempC Numeric vector of temperature in degrees celsius
#' @param gas Character string for gas code. Supoported gases are Ar, O2, CO2, SF6
#' @param method Character string to choose coeffecients from Raymond 2012 or Wanninkof 2014
#'
#' @return Schmidt number (unitless)
#' @export
#'
#' @note Temperature range is only valid from 4-35 deg Celsius
#' @references
#'Raymond, Peter A., Christopher J. Zappa, David Butman, Thomas L. Bott, Jody Potter, Patrick Mulholland,
#'Andrew E. Laursen, William H. McDowell, and Denis Newbold. \emph{Scaling the gas transfer velocity and hydraulic
#'geometry in streams and small rivers}. Limnology & Oceanography: Fluids & Environments 2 (2012): 41-53.
#' @references
#' Wanninkhof, Rik. \emph{Relationship between wind speed and gas exchange over the ocean revisited}. Limnology & Oceanography: Methods 12 (2014): 351-362.
#' @author Connor L. Brown
#' @examples
#' schmidtNum(tempC = 20, gas = "O2", method = "Ray2012)
schmidtNum <- function(tempC, gas, method) {
  t <- tempC
  #coefficients from Wanninkhof 2014
  wan2014 <- data.frame(
    "Ar" = c(1888.4,-134.55, 5.2003,-0.10946, 0.00093975),
    "O2" = c(1745.1, -124.34, 4.8055, -0.10115, 0.00086842),
    "CO2" = c(1923.6, -125.06, 4.3773, -0.085681, 0.00070284),
    "SF6" = c(3035,-196.35, 6.851,-0.13387, 0.0010972)
  )
  ray2012 <- data.frame(
    "Ar" = c(1799, -106.96, 2.797, -0.0289),
    "O2" = c(1568, -86.04, 2.142, -0.0216),
    "CO2" = c(1742, -91.24, 2.208, -0.0219),
    "SF6" = c(3255, -217.13, 6.837, -0.0861)
  )
  if(method == "Wan2014"){
    A <- unlist(wan2014[gas])[1]
    B <- unlist(wan2014[gas])[2]
    C <- unlist(wan2014[gas])[3]
    D <- unlist(wan2014[gas])[4]
    E <- unlist(wan2014[gas])[5]
    Sc <- as.numeric(A+(B*t)+(C*t^2)+(D*t^3)+(E*t^4))
    return(Sc)
  } else if(method == "Ray2012"){
    A <- unlist(ray2012[gas])[1]
    B <- unlist(ray2012[gas])[2]
    C <- unlist(ray2012[gas])[3]
    D <- unlist(ray2012[gas])[4]
    Sc <- as.numeric(A+(B*t)+(C*t^2)+(D*t^3))
  } else{warning("Unknown method, please select Ray2012 or Wan2014")}
  return(Sc)
}
