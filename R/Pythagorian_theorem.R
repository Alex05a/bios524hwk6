#' Calculate the length of the third side of a right triangle
#'
#' This function computes the length of the third side in a right triangle
#'  by applying the Pythagorean theorem
#' @param a Numeric. Length of one side
#' @param b Numeric. Length of the other side
#'
#'@return Numeric. Length of the third side
#'
#'@examples
#'Pythagorian_theorem(3,4,NA)
#'Pythagorian_theorem(a=4,b=3,c=NA)

#' @export
Pythagorian_theorem <-function(a,b,c){
  if(!is.na(a)&&!is.na(b)&&is.na(c)&&
     !is.character(a)&&!is.character(b)&&!is.character(c)){
    c<-sqrt(a^2+b^2)
    paste("c=" ,c)} else if
  (!is.na(c)&&!is.na(b)&&is.na(a)&&
   !is.character(a)&&!is.character(b)&&!is.character(c)){
    a<-sqrt(c^2-b^2)
    paste("a=" ,a)}else if
  (!is.na(c)&&!is.na(a)&&is.na(b)&&
   !is.character(a)&&!is.character(b)&&!is.character(c)){
    b<-sqrt(c^2-a^2)
    paste("a=" ,b)}else if
  ( is.na(a)&&is.na(b)|
    is.na(c)&&is.na(a)|
    is.na(b)&&is.na(c)){
    stop("missing numerals")}else if
  ( !is.na(a)&& !is.na(b)&&!is.na(c)&&
    is.numeric(a)&& is.numeric(b)&& is.numeric(c)){
    stop("three sides provided")}else if
  ( is.logical(a)| is.logical(b)|is.logical(c)|
    is.character(a)| is.character(b)|is.character(c))
    {stop("non-numeric values")}
}


