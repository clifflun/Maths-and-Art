Cellular_automata
================
2022-07-12

## displaying the grid

``` r
display <- function(mat){
  par(pty="s")
  image(t(mat), col=c("grey90", "grey40"), yaxt="n", xaxt="n")
  grid(nx = ncol(mat), ny = nrow(mat),col="grey70", lty=1)  
}
```

## Initialize a matrix

``` r
init <- function(n){
  n=41
  mat = matrix(0,nrow =1, ncol=n)
  mat[20]=1
  return (mat)
}
```

## Writing loop for Game of Life

``` r
rule30 <- function(mat, iter){
  display(mat)
  for (k in 1:iter) {
    temp_mat = matrix(0, nrow(mat), ncol(mat))
    for (i in 1:ncol(mat)){
      L = i-1
      R = i+1
      if (L == 0){L = length(mat)}
      if (R == length(mat) +1 ){R = 1}
      if (isTRUE(xor(mat[L], mat[i] || mat[R]))) {temp_mat[i] = 1}
    }
    mat = temp_mat
    display(mat)
  }
}
```

``` r
mat=init(40)
rule30(mat,40)
```

![](Cellular_automata_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-4-4.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-4-5.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-4-6.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-4-7.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-4-8.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-4-9.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-4-10.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-4-11.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-4-12.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-4-13.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-4-14.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-4-15.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-4-16.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-4-17.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-4-18.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-4-19.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-4-20.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-4-21.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-4-22.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-4-23.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-4-24.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-4-25.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-4-26.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-4-27.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-4-28.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-4-29.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-4-30.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-4-31.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-4-32.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-4-33.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-4-34.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-4-35.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-4-36.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-4-37.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-4-38.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-4-39.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-4-40.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-4-41.png)<!-- -->
