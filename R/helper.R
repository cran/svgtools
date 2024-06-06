# courtesy of miceadds 3.17-44 (Alexander Robitzsch, Simon Grund, Thorsten Henke)
roundAwayFromZero <- function (vec, digits = 0) 
{
  vec0 <- vec
  eps <- 1e-10
  vec <- abs(vec)
  vec <- vec * 10^digits
  vec2 <- vec - floor(vec)
  vec <- floor(vec) + ifelse((vec2 - 0.5) < -eps, 0, 1)
  vec.round <- sign(vec0) * vec/10^digits
  return(vec.round)
}
