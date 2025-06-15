
p.Diff <- function(x){
  ret <- x - lag(x)
  return(ret)
}
