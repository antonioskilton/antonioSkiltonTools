returnMatchedTerms <- function(terms, x){
  index <- c()
  sapply(x, function(y){
    for(term in terms){index[which(terms %in% term)] <- grepl(term, y)}
    return(terms[index])})
}
