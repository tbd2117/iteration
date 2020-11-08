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
    ## -3.03454 -0.63160  0.08851  0.04650  0.77689  2.39785

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
    ##  [1] 4.747059 5.273179 3.713858 3.022233 2.989230 4.030744 1.601181 2.326475
    ##  [9] 2.617023 4.683984 4.211161 2.203415 3.430292 1.457607 2.113429 2.388678
    ## [17] 4.932916 1.508408 3.134480 2.042144
    ## 
    ## $b
    ##  [1]   6.0910031  -6.5184243  -2.1130605  -1.9567152   0.2600749  -7.2462569
    ##  [7]  -1.7024051  -6.2842843   3.9768523  -0.6216738   6.3088766   1.6372781
    ## [13]   5.2207509   5.2906077  -7.1438633   2.2874358  -5.8428402  -2.5631943
    ## [19]  -8.6541028   2.1209571  -0.7482488  -2.9519484   1.0193318  -0.6888987
    ## [25]   2.8409814  -2.8045740  -0.8459959  -7.0915198 -10.8249453   2.3312410
    ## 
    ## $c
    ##  [1] 10.003969  9.675991 10.060741  9.991693 10.077404 10.198281 10.141797
    ##  [8]  9.468744  9.922402  9.819208  9.869765  9.885632  9.990301 10.053627
    ## [15]  9.959842 10.147700  9.745104  9.805986 10.004139  9.995224  9.917106
    ## [22] 10.068897 10.013640  9.977433  9.993493  9.939567  9.748900 10.061169
    ## [29] 10.399139 10.528353  9.813831 10.056501  9.745282  9.969615  9.779473
    ## [36]  9.769894 10.162273  9.969139  9.931021 10.157903
    ## 
    ## $d
    ##  [1] -3.686946 -4.175897 -4.079200 -3.629124 -3.493339 -3.286693 -3.698173
    ##  [8] -0.766339 -3.200783 -2.903061 -2.447389 -3.158832 -4.028755 -3.423986
    ## [15] -2.740267 -1.968590 -3.553800 -3.524458 -5.082292 -2.602487

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
    ## 1  3.12  1.21

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.24  4.65

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.97 0.190

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.27 0.905

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

``` r
output = map_dbl(list_norm, median)
```

``` r
output = map_df(list_norm, mean_and_sd, .id = "input")
```

## List columns\!

``` r
listcol_df = 
  tibble(
    name = c("a", "b", "c", "d"),
    samp = list_norm
  )
```

``` r
listcol_df %>% pull(name)
```

    ## [1] "a" "b" "c" "d"

``` r
listcol_df %>% pull(samp)
```

    ## $a
    ##  [1] 4.747059 5.273179 3.713858 3.022233 2.989230 4.030744 1.601181 2.326475
    ##  [9] 2.617023 4.683984 4.211161 2.203415 3.430292 1.457607 2.113429 2.388678
    ## [17] 4.932916 1.508408 3.134480 2.042144
    ## 
    ## $b
    ##  [1]   6.0910031  -6.5184243  -2.1130605  -1.9567152   0.2600749  -7.2462569
    ##  [7]  -1.7024051  -6.2842843   3.9768523  -0.6216738   6.3088766   1.6372781
    ## [13]   5.2207509   5.2906077  -7.1438633   2.2874358  -5.8428402  -2.5631943
    ## [19]  -8.6541028   2.1209571  -0.7482488  -2.9519484   1.0193318  -0.6888987
    ## [25]   2.8409814  -2.8045740  -0.8459959  -7.0915198 -10.8249453   2.3312410
    ## 
    ## $c
    ##  [1] 10.003969  9.675991 10.060741  9.991693 10.077404 10.198281 10.141797
    ##  [8]  9.468744  9.922402  9.819208  9.869765  9.885632  9.990301 10.053627
    ## [15]  9.959842 10.147700  9.745104  9.805986 10.004139  9.995224  9.917106
    ## [22] 10.068897 10.013640  9.977433  9.993493  9.939567  9.748900 10.061169
    ## [29] 10.399139 10.528353  9.813831 10.056501  9.745282  9.969615  9.779473
    ## [36]  9.769894 10.162273  9.969139  9.931021 10.157903
    ## 
    ## $d
    ##  [1] -3.686946 -4.175897 -4.079200 -3.629124 -3.493339 -3.286693 -3.698173
    ##  [8] -0.766339 -3.200783 -2.903061 -2.447389 -3.158832 -4.028755 -3.423986
    ## [15] -2.740267 -1.968590 -3.553800 -3.524458 -5.082292 -2.602487

``` r
listcol_df %>% 
  filter(name == "a")
```

    ## # A tibble: 1 x 2
    ##   name  samp        
    ##   <chr> <named list>
    ## 1 a     <dbl [20]>

Let’s try some operations…

``` r
mean_and_sd(listcol_df$samp[[1]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.12  1.21

``` r
mean_and_sd(listcol_df$samp[[2]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.24  4.65

Can I just map?

``` r
map(listcol_df$samp, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.12  1.21
    ## 
    ## $b
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.24  4.65
    ## 
    ## $c
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.97 0.190
    ## 
    ## $d
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.27 0.905

So… can I add a list column?

``` r
listcol_df = 
  listcol_df %>% 
    mutate(
      summary = map(samp, mean_and_sd),
      medians = map_dbl(samp, median)
    )
```
