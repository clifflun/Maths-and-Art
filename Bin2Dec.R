## binary to decimal converter

Bin2Dec <- function(n){
  bin = strsplit(toString(n), "")[[1]]
  deci = 0
  for (i in 1:length(bin)){
    if (strtoi(bin[i]) == 1){
      deci = deci * 2 + 1
    } else {
      deci = deci * 2
    }
  }
  return(deci)
}