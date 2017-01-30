# ' Half-Student-t Distribution
# '
# ' @param x vector of quantiles
# ' @param mean vector of means
# ' @param sd vector of standard deviations
# ' @param df vector of degrees of freedom
# ' @param log if TRUE, probabilities are given as log(p)
# ' @examples
# ' curve(dhalft(x,1,.5,2), -1,3, n=501)
# ' @export
# dhalft <- function(x,
#                    mean = 0,
#                    sd = 1,
#                    df = 1,
#                    log = FALSE){
#   p.lower <- pt(-mean/sd, df, lower.tail = TRUE)
#   dx <- sel <- x >= 0
#   if(sum(dx) > 0){
#     dx[sel] <- dt( (x[sel]-mean)/sd, df, log=log)
#     if(log){
#       dx[!sel] <- -Inf
#       dx[sel] <- dx[sel] - log(1 - p.lower)
#     }else{
#       dx[sel] <- dx[sel] / (1 - p.lower)
#     }
#   }
#   return(dx)
# }
