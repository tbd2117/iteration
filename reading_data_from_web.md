Iteration
================
11/08/2020

## Do something simple

``` r
x_vec = rnorm(30, mean = 5, sd = 3)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -2.059662015 -0.262620962 -1.684912339 -0.974648767 -0.260663511
    ##  [6]  0.891075055 -0.005555139 -0.128438397  0.792463180  1.257720069
    ## [11]  0.478022292  1.202636373 -1.467694432 -0.103326381 -0.743752201
    ## [16]  0.765567072  1.434854351  2.227659594 -1.005008399 -0.755308593
    ## [21]  1.177670441 -0.918179276  0.119129234 -0.127450275  0.227395176
    ## [26]  0.767024003  0.152510171 -0.303256972  0.351567718 -1.044817067

I want a function to compute z-scores

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("Input must be numeric")
  }
  
  if (length(x) < 3) {
    stop("Input must have at least 3 numbers")
  }
  z = (x - mean(x))/sd(x)
  
  return(z)
  
}

z_scores(x_vec)
```

    ##  [1] -2.059662015 -0.262620962 -1.684912339 -0.974648767 -0.260663511
    ##  [6]  0.891075055 -0.005555139 -0.128438397  0.792463180  1.257720069
    ## [11]  0.478022292  1.202636373 -1.467694432 -0.103326381 -0.743752201
    ## [16]  0.765567072  1.434854351  2.227659594 -1.005008399 -0.755308593
    ## [21]  1.177670441 -0.918179276  0.119129234 -0.127450275  0.227395176
    ## [26]  0.767024003  0.152510171 -0.303256972  0.351567718 -1.044817067

Try my function in some other things. these should give errors.

``` r
z_scores(3)
```

    ## Error in z_scores(3): Input must have at least 3 numbers

``` r
z_scores("my name if thiago")
```

    ## Error in z_scores("my name if thiago"): Input must be numeric

``` r
z_scores(mtcars)
```

    ## Error in z_scores(mtcars): Input must be numeric

``` r
z_scores(c(TRUE, TRUE, FALSE, TRUE))
```

    ## Error in z_scores(c(TRUE, TRUE, FALSE, TRUE)): Input must be numeric
