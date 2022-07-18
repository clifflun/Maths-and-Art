Turing Pattern
================

# Reaction-diffusion system

|                                                                                                    |                                                                                                                                                          |
| :------------------------------------------------------------------------------------------------: | :------------------------------------------------------------------------------------------------------------------------------------------------------: |
| ![pattern1](https://www.theinside.com/blog/wp-content/uploads/2021/05/01561732-e1586269333634.jpg) | ![pattern2](https://render.fineartamerica.com/images/images-profile-flow/400/images-medium-large-5/map-pufferfish-eye-scubazooscience-photo-library.jpg) |

Have you ever wondered how the patterns on animals are formed? how do
they have such perfect continuity and symmetry? Yet, sometimes, the
patterns seem random but there is a certain order to the chaos. The
answer lies in the interaction between chemicals in the organism. The
pattern formation can be modelled using the reaction-diffusion system.
This system basically looks at the interaction between elements like any
other reactions, but it adds the diffusion aspect to the equation.

The ***Gray-Scott model*** is the most well-known model and it may give
us some insight on how these patterns are formed.

## Gray-Scott model

The Gray-Scott model states that the two elements follow the following
equations:

![A' = A + (dA \* lap(A)) - rAB^2 +
f(1-A)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;A%27%20%3D%20A%20%2B%20%28dA%20%2A%20lap%28A%29%29%20-%20rAB%5E2%20%2B%20f%281-A%29
"A' = A + (dA * lap(A)) - rAB^2 + f(1-A)")

![B' = B + (dB \* lap(B)) + rAB^2 -
(k+f)B](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;B%27%20%3D%20B%20%2B%20%28dB%20%2A%20lap%28B%29%29%20%2B%20rAB%5E2%20-%20%28k%2Bf%29B
"B' = B + (dB * lap(B)) + rAB^2 - (k+f)B")

where
![dA](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;dA
"dA") is the diffusion rate of
![A](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;A
"A"),
![dB](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;dB
"dB") is the diffusion rate of
![B](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;B
"B"),
![r](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;r
"r") is the rate of reaction,
![f](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;f
"f") is the feed rate, and
![k](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;k
"k") is the kill rate. The laplacian functions represent the diffusion
function, which is represented by convolution in this context.

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
  if (a%%25 == 0){ ## not saving every iteration, the change is too slow
    save(m[,,1]-m[,,2],path,a)
    print(a)
    }
  }
}
```

### Laplacian fucntions (convolution)

Below, we created two 3x3 convolution kernels, one for each element.
This is not the simplest way to do it, but this will suffice for now.

The center cell has a weight of -1, adjacent neighbours have a weight of
0.2, and diagonal neighbours have a weight of 0.05.

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

This is the results with the initial states of
![dA](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;dA
"dA") = 1.1,
![dB](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;dB
"dB") = 0.3,
![r](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;r
"r") = 0.95,
![f](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;f
"f") = .055,
![k](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;k
"k") = 0.062 after 5000 iterations, where all
![A](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;A
"A") = 1 and all
![B](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;B
"B") = 0, except for a small initial starting cluster of
![B](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;B
"B").

Note that the concentration of
![A](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;A
"A") and
![B](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;B
"B") are represented on a gradient color scale, where
![A](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;A
"A") is yellow and
![B](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;B
"B") is purple/blue.

![default](Turing_Pattern_files/gifs/default.gif)

What if we tweak the parameters f and
k?

|                                                                      |                                                           |                                                                        |
| :------------------------------------------------------------------: | :-------------------------------------------------------: | :--------------------------------------------------------------------: |
| ![default](Turing_Pattern_files/gifs/low_f_high_k.gif) low f, high k |  ![default](Turing_Pattern_files/gifs/high_k.gif) high k  | ![default](Turing_Pattern_files/gifs/high_f_high_k.gif) high f, high k |
|        ![default](Turing_Pattern_files/gifs/low_f.gif) low f         | ![default](Turing_Pattern_files/gifs/default.gif) default |        ![default](Turing_Pattern_files/gifs/high_f.gif) high f         |
|  ![default](Turing_Pattern_files/gifs/low_f_low_k.gif) low f, low k  |   ![default](Turing_Pattern_files/gifs/low_k.gif) low k   |  ![default](Turing_Pattern_files/gifs/high_f_low_k.gif) high f, low k  |

It seems that by changing the parameters slightly will induce a drastic
change in the resulting pattern.

What if we have more than one starting cluster? Here are some
exmaples.

|                                                   |                                                  |
| :-----------------------------------------------: | :----------------------------------------------: |
| ![default](Turing_Pattern_files/gifs/spots_3.gif) | ![default](Turing_Pattern_files/gifs/stripe.gif) |

What if we start with random number of B cells at random locations?
Notice that the symmetry is gone because the starting pattern is not
symmetrical. The number of pattern possibilities is
endless.

|                                                    |                                                    |                                                    |
| :------------------------------------------------: | :------------------------------------------------: | :------------------------------------------------: |
| ![default](Turing_Pattern_files/gifs/random_1.gif) | ![default](Turing_Pattern_files/gifs/random_2.gif) | ![default](Turing_Pattern_files/gifs/random_3.gif) |
| ![default](Turing_Pattern_files/gifs/random_4.gif) | ![default](Turing_Pattern_files/gifs/random_5.gif) | ![default](Turing_Pattern_files/gifs/random_6.gif) |

As a last note, these parameters are fine tuned to obtain nice-looking
images. In nature, the most common case will probably be when one
element completely dominate the other, hence there is only one skin
color.

An interesting question to ask will be how will this look like in 3D?
But that is another topic for another time.
