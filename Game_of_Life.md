Cellular\_automata\_2D(Game of Life)
================
2022-07-13

# Objective

As we have briefly investigated 1d elementary cellular automata (CA), we
will now move to the 2D part and hopefully create some cool patterns\!
Game of life here we come\!

## Game of Life

Invented by John Conway in 1970s, this two-state, 2D CA has the
following rules:

  - Any live cell with fewer than two live neighbours dies, as if caused
    by underpopulation.
  - Any live cell with two or three live neighbours lives on to the next
    generation.
  - Any live cell with more than three live neighbours dies, as if by
    overpopulation.
  - Any dead cell with exactly three live neighbours becomes a live
    cell, as if by reproduction.

Here, a neighbour is defined as the all 8 cells surrounding the center
cell. This is called Moore neighborhood.

``` r
GoL <- function(mat, path, iter){
  save(mat, path, 0)
  for (k in 1:iter) {
    temp_mat = matrix(0,nrow(mat), ncol(mat))
    for (i in 1:ncol(mat)){
      for (j in 1:nrow(mat)){
        N = j-1
        S = j+1
        E = i-1
        W = i+1
        # for wrap around effect
        if (N == 0) {N = nrow(mat)}
        if (S == nrow(mat) + 1) {S = 1}
        if (E == 0) {E = 1}
        if (W == ncol(mat) + 1) {W = ncol(mat)}
        # check number of adjacent live cells
        count_lives = mat[N,i] + mat[S,i] + mat[j,E] + mat[j,W] + mat[N,W] + mat[N,E] + mat[S,W] + mat[S,E]
        if (mat[j,i] == 1 && count_lives < 2)                     {temp_mat[j,i] = 0}
        if (mat[j,i] == 1 && (count_lives ==2|| count_lives == 3)){temp_mat[j,i] = 1}
        if (mat[j,i] == 1 && count_lives > 3)                     {temp_mat[j,i] = 0}
        if (mat[j,i] == 0 && count_lives == 3)                    {temp_mat[j,i] = 1}
      }
    }
    mat = temp_mat
    save(mat, path, k)
  }
}
```

## Oscillators

These are some patterns that will repeat itself after a certain number
of iterations.

They come from different initial states

``` r
mat_size = 17
mat=init(mat_size)
# # Blinker
# mat[2:4,3] = 1

# # Toad
# mat[3,3:5] = 1 
# mat[4,2:4] = 1

# # Beacon 
# mat[2:3,2:3] = 1
# mat[4:5,4:5] = 1

# Pulsar
mat[c(3,8), 5:7] = 1
mat[5:7, c(3,8)] = 1
tmp = t(apply(mat, 2, rev)) # rotating pattern for 90 degrees
tmp2 = t(apply(tmp, 2, rev))
tmp3 = t(apply(tmp2, 2, rev))
mat = mat+tmp+tmp2+tmp3

# # Pentadecathlon
# mat[6:15,11] = 1
```

![Blinker](https://media0.giphy.com/media/yRS6RJr9N8JiuzVmna/200w.webp)
![Toad](https://media1.giphy.com/media/NfvVwnUyU6Zg5jWlfA/200w.webp)
![Beacon](https://media4.giphy.com/media/8PcqPIPXbRK50qUef1/200w.webp)
![Pulsar](https://media1.giphy.com/media/jHPa8vcy3zMmnWVhkN/200w.webp)

![pentadecathlon](https://media2.giphy.com/media/nBslXi3zOBM44x6KlJ/giphy.gif?cid=790b7611a378b629621a62794dfacca17024d0212aea497f&rid=giphy.gif&ct=g)
