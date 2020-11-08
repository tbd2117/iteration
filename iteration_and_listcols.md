Iteration and listcols
================
11/08/2020

## Lists

You ca put anything in a list

``` r
l = list(
  vec_numeric = 5:8,
  vec_logical = c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE),
  mat = matrix(1:8, nrow = 2, ncol = 4),
  summary = summary(rnorm(100))
)
```

``` r
l
```

    ## $vec_numeric
    ## [1] 5 6 7 8
    ## 
    ## $vec_logical
    ## [1]  TRUE  TRUE FALSE  TRUE FALSE FALSE
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8
    ## 
    ## $summary
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -2.50534 -0.63162  0.04038  0.05302  0.88374  2.45333

``` r
l$vec_numeric
```

    ## [1] 5 6 7 8

``` r
l[[1]]
```

    ## [1] 5 6 7 8

``` r
mean(l[["vec_numeric"]])
```

    ## [1] 6.5

## for loop

Create a new list

``` r
list_norm = 
  list(
    a = rnorm(20, mean = 3, sd = 1),
    b = rnorm(30, mean = 0, sd = 5),
    c = rnorm(40, mean = 10, sd = .2),
    d = rnorm(20, mean = -3, sd = 1)
  )
```

``` r
list_norm
```

    ## $a
    ##  [1] 3.957486 2.641722 2.769298 5.490882 2.376653 3.646755 3.034428 2.187756
    ##  [9] 3.483910 2.428279 2.689427 2.320807 2.734752 4.085913 1.057277 5.073815
    ## [17] 3.299891 1.579434 3.589649 2.001175
    ## 
    ## $b
    ##  [1]  2.1459807 -0.0917155 -2.2797829  7.4646628  2.4479612 -3.0787401
    ##  [7] -2.0371191  3.9220973  2.4748697  3.3733409 -1.8503909 -5.5829254
    ## [13]  3.7693160  0.3998216 -0.6355642 -3.0517157  6.1916362 -0.6012906
    ## [19] -2.5378059 -1.1496710 -7.5346556  6.6221384 -7.2485545  9.7165907
    ## [25] -1.4679803  6.5769003  1.2378350  1.1827802  2.5461298  0.2146142
    ## 
    ## $c
    ##  [1] 10.343711 10.069674  9.934083  9.913245 10.211987  9.733812  9.962221
    ##  [8]  9.836349  9.808309  9.797491 10.340741  9.796658  9.876432 10.274214
    ## [15] 10.145749 10.218157  9.725530 10.091930  9.661563  9.861777  9.716234
    ## [22] 10.309352  9.863959 10.100052  9.836342  9.736627 10.594156 10.224778
    ## [29] 10.253053 10.145894 10.260039  9.926965  9.725395 10.120076 10.218579
    ## [36] 10.042888  9.944729  9.669755 10.030884  9.918873
    ## 
    ## $d
    ##  [1] -4.027065 -3.094025 -3.798407 -2.788465 -2.623301 -3.175256 -3.914251
    ##  [8] -1.682131 -1.995493 -3.965338 -2.625101 -4.939726 -4.548947 -3.420493
    ## [15] -2.841303 -2.176446 -3.210956 -1.849458 -2.500245 -1.989252

Pause and get my old function.

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Input must be numeric")
  }
  
  if (length(x) < 3) {
    stop("Input must have at least 3 numbers")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  tibble(
    mean = mean_x,
    sd = sd_x
  )
  
}
```

I can apply that function to each list element

``` r
mean_and_sd(list_norm[[1]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.02  1.09

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.705  4.20

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.227

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.06 0.923

Let’s use a for loop

``` r
output = vector("list", length = 4)

for (i in 1:4) {
output[[i]] = mean_and_sd(list_norm[[i]])
}
```

Let’s try map\!

``` r
output = map(list_norm, mean_and_sd)
```

What if you want a different function?

``` r
output = map(list_norm, median)
```
