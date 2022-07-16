Turing\_Pattern
================

# Reaction-diffusion system

## Gray-Scott model

## Laplacian fucntions (convolution)

``` r
lapA <- function(m,i,j, cw = -1, an = 0.2, dn = 0.05){
  m = m[,,1]
  N = i - 1
  S = i + 1
  W = j - 1
  E = j + 1
  ## For edges
  if (N == 0) {N = 1}
  if (S == dim(m)[1] + 1) {S = dim(m)[1]}
  if (W == 0) {W = 1} 
  if (E == dim(m)[2] + 1) {E = dim(m)[2]}
  sum = m[i,j]*cw + an*(m[N,j]+m[S,j]+m[i,W]+m[i,E]) + dn*(m[N,W]+m[N,E]+m[S,W]+m[S,E])
  return(sum)
}

lapB <- function(m,i,j, cw = -1, an = 0.2, dn = 0.05){
  m = m[,,2]
  N = i - 1
  S = i + 1
  W = j - 1
  E = j + 1
  ## For edges
  if (N == 0) {N = 1}
  if (S == dim(m)[1] + 1) {S = dim(m)[1]}
  if (W == 0) {W = 1} 
  if (E == dim(m)[2] + 1) {E = dim(m)[2]}
  sum = m[i,j]*cw + an*(m[N,j]+m[S,j]+m[i,W]+m[i,E]) + dn*(m[N,W]+m[N,E]+m[S,W]+m[S,E])
  return(sum)
}
```

``` r
## GSM model

GSM <- function(m,iter, path, dA = 1.1, dB = 0.3, r = 0.95, f = .055, k = 0.062){
  for (a in 1:iter){
  tmp_m = array(0, dim = c(dim(m)[1], dim(m)[2], dim(m)[3]))
    for (i in 1:dim(m)[1]){
      for (j in 1:dim(m)[2]){
        A = m[i,j,1]
        B = m[i,j,2]
        A = A + (dA*lapA(m,i,j) - r*A*B*B + f*(1-A))
        B = B + (dB*lapB(m,i,j) + r*A*B*B - (k+f)*B)
        tmp_m[i,j,1] = A
        tmp_m[i,j,2] = B
      }
    }
  m = tmp_m
  save(m[,,1]-m[,,2],path,a)
  print(a)
  }
  # display(m[,,1]-m[,,2])
}
```

\`\`\`
