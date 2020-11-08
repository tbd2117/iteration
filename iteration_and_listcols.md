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
    ## -3.18293 -0.77470 -0.08464 -0.12742  0.41140  2.74506

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
    ##  [1] 2.1375116 2.6000232 3.8162876 0.8665632 2.6888924 3.0654008 1.4588810
    ##  [8] 2.4291330 2.7503250 2.2758206 2.1093757 4.2992260 1.5657311 3.0685471
    ## [15] 1.3079636 1.9350963 3.5715687 4.1371035 1.0211444 2.6683342
    ## 
    ## $b
    ##  [1]   3.5905458 -11.0226172  -0.2511951  -3.7512947  -1.6411211  -8.4245879
    ##  [7]   2.0132713  -4.3160813   3.5819977  -1.5654490 -10.3300506  -1.2001862
    ## [13]  -0.4075581  -1.4474724  -3.3221199   2.6135669   1.2791140   0.8942248
    ## [19]  -3.4871417   0.0343864  -3.6945522  -2.0915739  -5.4897936   1.3864253
    ## [25]   7.5631525  -1.9854403   4.2856029   4.5847865   6.4804264   0.9458857
    ## 
    ## $c
    ##  [1] 10.005671 10.001796 10.288908 10.025393  9.889363  9.846892 10.037979
    ##  [8] 10.098206 10.253275  9.941225  9.929910 10.370693 10.121909 10.162367
    ## [15]  9.836901 10.140331  9.918199 10.141271  9.944172 10.073119  9.465483
    ## [22] 10.357740  9.710789 10.089742 10.036689 10.128130  9.859571 10.016180
    ## [29]  9.677683  9.896910 10.032331  9.760535  9.846380  9.756652 10.233028
    ## [36]  9.915727  9.691757 10.207330  9.835772 10.193569
    ## 
    ## $d
    ##  [1] -5.265415 -1.205961 -2.487180 -2.185926 -2.820634 -2.361602 -2.558847
    ##  [8] -3.443599 -3.265351 -2.883128 -3.132802 -2.980997 -3.674355 -4.148450
    ## [15] -2.436268 -5.606908 -3.177394 -2.197370 -1.502081 -2.096788

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
    ## 1  2.49 0.986

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.839  4.45

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.99 0.198

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.97  1.10

Let’s use a for loop

``` r
output = vector("list", length = 4)

for (i in 1:4) {
output[[i]] = mean_and_sd(list_norm[[i]])
}
```
