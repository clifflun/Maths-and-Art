Elementary_cellular_automata
================
2022-07-12

# Objective

The purpose of this exercise is to understand the behaviour of
elementary celluarl automata (1D) before moving into modeling 2D
cellular automata (CA). We will first pick one rule and move on into
generating all 256 rules.

## Rule 30

This rule is special because it matches the condition “Left cell XOR
(Center cell OR Right cell)”.

This makes rule 30 a good place to start for coding.

Below, a matrix of selected size is initialized and Rule 30 is applied
for desired number of iterations.

``` r
rule30 <- function(mat, iter){
  for (k in 1:iter) {
    for (i in 1:ncol(mat)){
      L = i-1
      R = i+1
      # for wrap around effect
      if (L == 0){L = ncol(mat)}
      if (R == ncol(mat) +1 ){R = 1}
      # rule 30 states: left XOR (mid OR right)
      # populating table downwards
      if (isTRUE(xor(mat[k,L], mat[k,i] || mat[k,R]))) {
        mat[k+1,i] = 1}
    }

  }
  display(mat,30)
}
```

## Display an example of 100 iterations of Rule30

``` r
mat=init(101)
rule30(mat,100)
```

![](Cellular_automata_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Here we will display each iteration of rule 30

<img src="https://media.giphy.com/media/p5o2uVKdTmf1FnVFAs/giphy.gif"
style="height:150.0%" />

# The elementary CA numbering system

In all of Wolfram’s elementary cellular automata, an infinite
one-dimensional array of cellular automaton cells with only two states
is considered, with each cell in some initial state. At discrete time
intervals, every cell spontaneously changes state based on its current
state and the state of its two neighbors.

There are only 8 possible current patterns and Stephen Wolfram
standardized the order of such configuration.

111, 110, 101, 100, 011, 010, 001, 000

The binary equivalent of the rule number will be the new state of the
corresponding current pattern.

For instance, rule 30 has the binary equivalent of 00011110:

| current pattern | 111 | 110 | 101 | 100 | 001 | 010 | 001 | 000 |
|:---------------:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|
|    new state    |  0  |  0  |  0  |  1  |  1  |  1  |  1  |  0  |

By this logic, we can drive the new state for all 256 rules.

All we need to do is convert all rule numbers: 0-255 into binary format
and map each current patter to its corresponding new state.

## Decimal to binary converter

Quick decimal to binary calculator, using strings to make a named list
in the next step.

``` r
Dec2Bin <- function(n){
  bsum = 0
  bexp = 1
  while (n >0){
    mod = n %% 2
    n = floor(n/2)
    bsum = bsum + mod * bexp
    bexp = bexp *10
  }
  bsum = sprintf("%08s",toString(format(bsum, scientific = F))) # format to avoid sci notations, e+05 etc
  
  return (bsum)
}
```

## Setting up state_rules with named list

``` r
state_rules <- function(rule_num){
  binstr = strsplit(Dec2Bin(rule_num),"")[[1]]
  patterns = c("111", "110", "101", "100", "011", "010", "001", "000")
  new_state = as.list(binstr)
  names(new_state) = patterns
  return(new_state)
}
```

## Writing loop for Game of Life for all rules

``` r
rule_any <- function(mat, iter, rule_num = 30){
  # get state rules
  state_rule = state_rules(rule_num)
  
  # loop through array
  for (k in 1:iter) {
    for (i in 1:ncol(mat)){
      L = i-1
      R = i+1
      # for wrap around effect
      if (L == 0){L = ncol(mat)}
      if (R == ncol(mat) +1 ){R = 1}
      cur_pattern = paste(c(mat[k,L], mat[k,i],mat[k,R]), collapse = "") #only two lines of change
      mat[k+1,i]= strtoi(state_rule[cur_pattern][[1]])
    }

  }
  display(mat, rule_num)
}
```

## Generate all 256 rules (0-255)

As you will see, not all rules have special patterns. But this is
another topic for another time.

``` r
mat_size = 51 #pick odd number for better visualization()
mat=init(mat_size)
for (i in 0:255){
  rule_any(mat,mat_size-1,i)
}
```

![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-3.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-4.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-5.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-6.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-7.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-8.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-9.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-10.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-11.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-12.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-13.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-14.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-15.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-16.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-17.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-18.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-19.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-20.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-21.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-22.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-23.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-24.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-25.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-26.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-27.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-28.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-29.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-30.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-31.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-32.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-33.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-34.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-35.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-36.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-37.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-38.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-39.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-40.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-41.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-42.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-43.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-44.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-45.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-46.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-47.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-48.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-49.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-50.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-51.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-52.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-53.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-54.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-55.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-56.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-57.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-58.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-59.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-60.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-61.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-62.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-63.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-64.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-65.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-66.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-67.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-68.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-69.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-70.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-71.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-72.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-73.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-74.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-75.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-76.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-77.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-78.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-79.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-80.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-81.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-82.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-83.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-84.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-85.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-86.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-87.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-88.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-89.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-90.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-91.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-92.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-93.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-94.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-95.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-96.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-97.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-98.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-99.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-100.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-101.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-102.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-103.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-104.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-105.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-106.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-107.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-108.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-109.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-110.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-111.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-112.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-113.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-114.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-115.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-116.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-117.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-118.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-119.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-120.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-121.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-122.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-123.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-124.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-125.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-126.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-127.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-128.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-129.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-130.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-131.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-132.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-133.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-134.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-135.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-136.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-137.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-138.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-139.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-140.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-141.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-142.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-143.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-144.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-145.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-146.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-147.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-148.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-149.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-150.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-151.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-152.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-153.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-154.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-155.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-156.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-157.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-158.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-159.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-160.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-161.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-162.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-163.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-164.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-165.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-166.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-167.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-168.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-169.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-170.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-171.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-172.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-173.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-174.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-175.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-176.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-177.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-178.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-179.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-180.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-181.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-182.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-183.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-184.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-185.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-186.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-187.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-188.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-189.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-190.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-191.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-192.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-193.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-194.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-195.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-196.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-197.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-198.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-199.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-200.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-201.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-202.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-203.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-204.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-205.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-206.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-207.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-208.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-209.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-210.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-211.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-212.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-213.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-214.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-215.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-216.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-217.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-218.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-219.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-220.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-221.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-222.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-223.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-224.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-225.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-226.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-227.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-228.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-229.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-230.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-231.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-232.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-233.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-234.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-235.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-236.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-237.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-238.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-239.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-240.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-241.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-242.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-243.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-244.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-245.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-246.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-247.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-248.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-249.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-250.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-251.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-252.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-253.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-254.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-255.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-11-256.png)<!-- -->
