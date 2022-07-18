Turing\_Pattern
================

# Reaction-diffusion system

## Gray-Scott model

The Gray-Scott model states that the two elements follow the following
equations, where dA is the diffusion rate of A, dB is the diffusion rate
of B, r is the rate of reaction, f is the feed rate, and k is the kill
rate. The laplacian functions represent the diffusion function, which is
represented by convolution in this context.

``` r
## GSM model

GSM <- function(m,iter, path, dA = 1.1, dB = 0.3, r = 0.95, f = .055, k = 0.062){
  save(m[,,1]-m[,,2],path,0)
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
  if (a%%20 == 0){ ## not saving every iteration, the change is too slow
    save(m[,,1]-m[,,2],path,a)
    print(a)
    }
  }
}
```

### Laplacian fucntions (convolution)

Below, we created two 3x3 convolution kernels, one for each element.
This is not the simplest way to do it, but this will suffice for now.

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

This is the results with the initial states of dA = 1.1, dB = 0.3, r =
0.95, f = .055, k = 0.062 after 5000 iterations.

![default](Turing_Pattern_files/gifs/default.gif)

What if we tweak the parameters f and
k?

|                                                                      |                                                           |                                                                        |
| :------------------------------------------------------------------: | :-------------------------------------------------------: | :--------------------------------------------------------------------: |
| ![default](Turing_Pattern_files/gifs/low_f_high_k.gif) low f, high k |  ![default](Turing_Pattern_files/gifs/high_k.gif) high k  | ![default](Turing_Pattern_files/gifs/high_f_high_k.gif) high f, high k |
|        ![default](Turing_Pattern_files/gifs/low_f.gif) low f         | ![default](Turing_Pattern_files/gifs/default.gif) default |        ![default](Turing_Pattern_files/gifs/high_f.gif) high f         |
|  ![default](Turing_Pattern_files/gifs/low_f_low_k.gif) low f, low k  |   ![default](Turing_Pattern_files/gifs/low_k.gif) low k   |  ![default](Turing_Pattern_files/gifs/high_f_low_k.gif) high f, low k  |
