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
    ## -1.76119 -0.64434 -0.05463  0.11020  0.83822  2.97682

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
    ##  [1] 3.521943 3.123706 3.337548 2.979222 3.397595 3.024223 3.453002 4.134701
    ##  [9] 2.152160 3.339150 2.198502 3.525028 3.392941 2.332192 2.824929 3.901712
    ## [17] 3.243866 4.231524 3.367733 2.627788
    ## 
    ## $b
    ##  [1]  5.6751067  2.0815914 -1.4522067  0.7942920 -5.0783617 -7.2516673
    ##  [7]  4.3188725 -6.8712867  4.1512465  2.2132130 -4.3758982 -6.9622077
    ## [13] -3.2237808 -6.7801068  1.5416341 -4.4720175  2.0681675 -2.7712614
    ## [19]  5.4450475  8.9364636  5.0573154 -3.2360944  0.6057112  4.7997643
    ## [25] -1.2887067 -2.0088841 -1.7862094  0.5448373 -4.4419385 -6.2994595
    ## 
    ## $c
    ##  [1]  9.844818 10.026894 10.028930 10.009901 10.193206 10.169152  9.924386
    ##  [8]  9.709605 10.046710 10.199540  9.992598  9.998652 10.025350  9.917827
    ## [15] 10.311692  9.736523  9.842539  9.927330  9.867079  9.894475 10.041172
    ## [22]  9.925166  9.941562  9.928005 10.105022 10.094207 10.162371  9.712985
    ## [29]  9.945331  9.927952  9.882976  9.768702  9.879583  9.975724  9.938640
    ## [36]  9.856462  9.923172  9.988109 10.039019 10.103095
    ## 
    ## $d
    ##  [1] -0.9385650 -3.5773892 -3.0806906 -2.5391094 -1.8610852 -4.0737971
    ##  [7] -3.4968551 -3.8029599 -4.0531835 -3.3649913 -3.4205051 -2.5700796
    ## [13] -0.1404323 -3.7683688 -2.7032031 -1.7974005 -1.6812546 -4.1836064
    ## [19] -1.2197370 -4.0734759

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
    ## 1  3.21 0.574

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.669  4.50

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.97 0.134

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.82  1.19

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
    ##  [1] 3.521943 3.123706 3.337548 2.979222 3.397595 3.024223 3.453002 4.134701
    ##  [9] 2.152160 3.339150 2.198502 3.525028 3.392941 2.332192 2.824929 3.901712
    ## [17] 3.243866 4.231524 3.367733 2.627788
    ## 
    ## $b
    ##  [1]  5.6751067  2.0815914 -1.4522067  0.7942920 -5.0783617 -7.2516673
    ##  [7]  4.3188725 -6.8712867  4.1512465  2.2132130 -4.3758982 -6.9622077
    ## [13] -3.2237808 -6.7801068  1.5416341 -4.4720175  2.0681675 -2.7712614
    ## [19]  5.4450475  8.9364636  5.0573154 -3.2360944  0.6057112  4.7997643
    ## [25] -1.2887067 -2.0088841 -1.7862094  0.5448373 -4.4419385 -6.2994595
    ## 
    ## $c
    ##  [1]  9.844818 10.026894 10.028930 10.009901 10.193206 10.169152  9.924386
    ##  [8]  9.709605 10.046710 10.199540  9.992598  9.998652 10.025350  9.917827
    ## [15] 10.311692  9.736523  9.842539  9.927330  9.867079  9.894475 10.041172
    ## [22]  9.925166  9.941562  9.928005 10.105022 10.094207 10.162371  9.712985
    ## [29]  9.945331  9.927952  9.882976  9.768702  9.879583  9.975724  9.938640
    ## [36]  9.856462  9.923172  9.988109 10.039019 10.103095
    ## 
    ## $d
    ##  [1] -0.9385650 -3.5773892 -3.0806906 -2.5391094 -1.8610852 -4.0737971
    ##  [7] -3.4968551 -3.8029599 -4.0531835 -3.3649913 -3.4205051 -2.5700796
    ## [13] -0.1404323 -3.7683688 -2.7032031 -1.7974005 -1.6812546 -4.1836064
    ## [19] -1.2197370 -4.0734759

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
    ## 1  3.21 0.574

``` r
mean_and_sd(listcol_df$samp[[2]])
```

    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.669  4.50

Can I just map?

``` r
map(listcol_df$samp, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.21 0.574
    ## 
    ## $b
    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.669  4.50
    ## 
    ## $c
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.97 0.134
    ## 
    ## $d
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.82  1.19

So… can I add a list column?

``` r
  listcol_df = 
  listcol_df %>% 
    mutate(
      summary = map(samp, mean_and_sd),
      medians = map_dbl(samp, median)
    )
```

## Weather data

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USC00519397", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2017-01-01",
    date_max = "2017-12-31") %>%
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USC00519397 = "Waikiki_HA",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: /Users/thiagoaraujo/Library/Caches/R/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2020-10-02 07:31:47 (7.52)

    ## file min/max dates: 1869-01-01 / 2020-09-30

    ## using cached file: /Users/thiagoaraujo/Library/Caches/R/noaa_ghcnd/USC00519397.dly

    ## date created (size, mb): 2020-10-02 07:31:54 (1.699)

    ## file min/max dates: 1965-01-01 / 2020-03-31

    ## using cached file: /Users/thiagoaraujo/Library/Caches/R/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2020-10-02 07:31:58 (0.877)

    ## file min/max dates: 1999-09-01 / 2020-09-30

Get our list columns…

``` r
weather_nest = 
  weather_df %>% 
  nest(data = date:tmin)
```

``` r
weather_nest %>% pull(name)
```

    ## [1] "CentralPark_NY" "Waikiki_HA"     "Waterhole_WA"

``` r
weather_nest %>% pull(data)
```

    ## [[1]]
    ## # A tibble: 365 x 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0   8.9   4.4
    ##  2 2017-01-02    53   5     2.8
    ##  3 2017-01-03   147   6.1   3.9
    ##  4 2017-01-04     0  11.1   1.1
    ##  5 2017-01-05     0   1.1  -2.7
    ##  6 2017-01-06    13   0.6  -3.8
    ##  7 2017-01-07    81  -3.2  -6.6
    ##  8 2017-01-08     0  -3.8  -8.8
    ##  9 2017-01-09     0  -4.9  -9.9
    ## 10 2017-01-10     0   7.8  -6  
    ## # … with 355 more rows
    ## 
    ## [[2]]
    ## # A tibble: 365 x 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0  26.7  16.7
    ##  2 2017-01-02     0  27.2  16.7
    ##  3 2017-01-03     0  27.8  17.2
    ##  4 2017-01-04     0  27.2  16.7
    ##  5 2017-01-05     0  27.8  16.7
    ##  6 2017-01-06     0  27.2  16.7
    ##  7 2017-01-07     0  27.2  16.7
    ##  8 2017-01-08     0  25.6  15  
    ##  9 2017-01-09     0  27.2  15.6
    ## 10 2017-01-10     0  28.3  17.2
    ## # … with 355 more rows
    ## 
    ## [[3]]
    ## # A tibble: 365 x 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01   432  -6.8 -10.7
    ##  2 2017-01-02    25 -10.5 -12.4
    ##  3 2017-01-03     0  -8.9 -15.9
    ##  4 2017-01-04     0  -9.9 -15.5
    ##  5 2017-01-05     0  -5.9 -14.2
    ##  6 2017-01-06     0  -4.4 -11.3
    ##  7 2017-01-07    51   0.6 -11.5
    ##  8 2017-01-08    76   2.3  -1.2
    ##  9 2017-01-09    51  -1.2  -7  
    ## 10 2017-01-10     0  -5   -14.2
    ## # … with 355 more rows

Suppose I want to regress `tmax` on `tmin` for each station.

This works…

``` r
lm(tmax ~ tmin, data = weather_nest$data[[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest$data[[1]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039

Let’s write a function to do this regression

``` r
weather_lm = function(df) {
  
  lm(tmax ~ tmin, data = df)
  
}

output = vector("list", 3)

for (i in 1:3) {
  
 output[[i]] = weather_lm(weather_nest$data[[i]])

}
```

map…

``` r
map(weather_nest$data, weather_lm)
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     20.0966       0.4509  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.499        1.221

What about a map in a list column?

``` r
weather_nest = 
  weather_nest %>% 
    mutate(
      models = map(data, weather_lm)
    )

weather_nest$models
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     20.0966       0.4509  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.499        1.221
