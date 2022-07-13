Cellular_automata
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

![](Cellular_automata_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

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

![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-3.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-4.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-5.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-6.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-7.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-8.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-9.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-10.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-11.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-12.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-13.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-14.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-15.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-16.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-17.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-18.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-19.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-20.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-21.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-22.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-23.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-24.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-25.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-26.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-27.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-28.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-29.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-30.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-31.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-32.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-33.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-34.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-35.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-36.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-37.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-38.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-39.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-40.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-41.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-42.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-43.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-44.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-45.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-46.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-47.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-48.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-49.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-50.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-51.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-52.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-53.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-54.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-55.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-56.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-57.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-58.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-59.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-60.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-61.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-62.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-63.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-64.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-65.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-66.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-67.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-68.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-69.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-70.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-71.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-72.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-73.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-74.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-75.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-76.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-77.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-78.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-79.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-80.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-81.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-82.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-83.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-84.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-85.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-86.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-87.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-88.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-89.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-90.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-91.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-92.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-93.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-94.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-95.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-96.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-97.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-98.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-99.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-100.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-101.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-102.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-103.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-104.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-105.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-106.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-107.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-108.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-109.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-110.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-111.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-112.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-113.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-114.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-115.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-116.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-117.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-118.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-119.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-120.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-121.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-122.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-123.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-124.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-125.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-126.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-127.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-128.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-129.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-130.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-131.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-132.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-133.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-134.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-135.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-136.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-137.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-138.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-139.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-140.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-141.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-142.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-143.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-144.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-145.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-146.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-147.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-148.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-149.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-150.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-151.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-152.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-153.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-154.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-155.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-156.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-157.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-158.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-159.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-160.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-161.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-162.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-163.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-164.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-165.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-166.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-167.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-168.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-169.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-170.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-171.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-172.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-173.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-174.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-175.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-176.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-177.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-178.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-179.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-180.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-181.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-182.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-183.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-184.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-185.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-186.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-187.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-188.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-189.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-190.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-191.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-192.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-193.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-194.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-195.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-196.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-197.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-198.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-199.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-200.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-201.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-202.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-203.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-204.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-205.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-206.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-207.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-208.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-209.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-210.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-211.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-212.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-213.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-214.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-215.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-216.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-217.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-218.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-219.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-220.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-221.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-222.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-223.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-224.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-225.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-226.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-227.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-228.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-229.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-230.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-231.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-232.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-233.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-234.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-235.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-236.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-237.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-238.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-239.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-240.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-241.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-242.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-243.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-244.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-245.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-246.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-247.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-248.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-249.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-250.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-251.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-252.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-253.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-254.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-255.png)<!-- -->![](Cellular_automata_files/figure-gfm/unnamed-chunk-8-256.png)<!-- -->
