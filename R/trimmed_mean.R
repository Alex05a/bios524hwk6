#' Calculate the trimmed mean of a numeric vector
#'
#' This function computes the average of a numeric vector, where it omits the
#' 's' smallest values and the 'l' largest values from consideration,resulting
#' what is known as a 'trimmed mean'
#' @param y Numeric vector
#' @param s Number of smallest value to ignore
#' @param l Number of largest value to ignore
#'
#'@return Numeric.Trimmed mean of the vector
#'
#'@examples
#'trimmed_mean(10,9,y)
#' @export

# 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

y<-rnorm(50)
y
trimmed_mean<-function(s, l, y){
  if(length(y)<s+l+1){stop('trimmed_mean does not exist')}
  else{
    x= sort(y)
    upper=length(x)-l
    tm=mean(x[(s+1):upper])
    tm}
}
trimmed_mean(10,9,y)
