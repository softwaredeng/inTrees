presentRules <-
function(rules,colN,digits=NULL){
  for(i in 1:nrow(rules[,"condition",drop=FALSE])){
    if(is.numeric(digits)){
      digits <- as.integer(abs(digits))
      rules[,"freq"] <- round(as.numeric( rules[,"freq"]),digits=digits)
      rules[,"err"] <- round(as.numeric( rules[,"err"]),digits=digits)
    }
    A <- regexpr("X\\[,1\\]==X\\[,1\\]", rules[i,"condition"])
    thisPos <- as.numeric(A[[1]])
    thisLen <- attr(A, "match.length")
    if(thisPos > 0){
      origStr <- substr(rules[i,"condition"], thisPos, thisPos+thisLen-1)
      rules[i,"condition"] <- gsub(origStr, "Else", rules[i,"condition"], fixed=TRUE)
    }
    while(TRUE){
      A <- regexpr("X\\[,[0-9]+\\]", rules[i,"condition"])
      thisPos <- as.numeric(A[[1]])
      thisLen <- attr(A, "match.length")
      if(thisPos <= 0) break
      origStr <- substr(rules[i,"condition"], thisPos, thisPos+thisLen-1)
      ix <- as.numeric(gsub("\\D", "", origStr))
      colStr <- colN[ix]
      rules[i,"condition"] <- gsub(origStr, colStr, rules[i,"condition"], fixed=TRUE)
    }
  }
  return(rules)
}
