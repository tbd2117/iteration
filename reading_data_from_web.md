Iteration
================
11/08/2020

## Do something simple

``` r
x_vec = rnorm(30, mean = 5, sd = 3)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  0.49209270  1.34991396 -1.37684264  0.01598769  0.95627023 -0.09096968
    ##  [7] -0.41917131 -0.53922366 -0.36141639 -0.53237438 -0.11361737 -1.30622551
    ## [13]  1.87231747 -0.84018002  0.81632211 -0.91229621  0.35407001 -1.11803649
    ## [19] -0.01179400  0.36409609  0.30895568  0.96063244  0.83084508  0.57320976
    ## [25] -1.50982292 -2.07488300 -0.34789632  0.48990794  2.30190824 -0.13177951

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

    ##  [1]  0.49209270  1.34991396 -1.37684264  0.01598769  0.95627023 -0.09096968
    ##  [7] -0.41917131 -0.53922366 -0.36141639 -0.53237438 -0.11361737 -1.30622551
    ## [13]  1.87231747 -0.84018002  0.81632211 -0.91229621  0.35407001 -1.11803649
    ## [19] -0.01179400  0.36409609  0.30895568  0.96063244  0.83084508  0.57320976
    ## [25] -1.50982292 -2.07488300 -0.34789632  0.48990794  2.30190824 -0.13177951

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

## Multiple outputs

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

Check that the function works

``` r
x_vec = rnorm(100, mean = 3, sd = 4)
mean_and_sd(x_vec)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.32  4.39

## Multiple inputs

I would like to this with a function

``` r
sim_data = 
  tibble(
    x = rnorm(100, mean = 4, sd = 3)
  )

sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.50  3.25

``` r
sim_mean_sd = function(samp_size, mu = 3, sigma = 4){
  
  sim_data = 
      tibble(
        x = rnorm(n = samp_size, mean = mu, sd = sigma)
  )

  sim_data %>% 
    summarize(
      mean = mean(x),
      sd = sd(x)
  )

}

sim_mean_sd(100, 6, 3)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.17  3.01

``` r
sim_mean_sd(100)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.40  4.22

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_nodes(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text() %>%
  str_extract("^\\d") %>%
  as.numeric()

review_text = 
  dynamite_html %>%
  html_nodes(".review-text-content span") %>%
  html_text() %>% 
  str_replace_all("\n", "") %>% 
  str_trim()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

What about the next page of reviews?

Let’s turn taht code into a function

``` r
read_page_reviews = function(url){
  
  html = read_html(url)

  review_titles = 
   html %>%
    html_nodes(".a-text-bold span") %>%
    html_text()

  review_stars = 
    html %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("^\\d") %>%
    as.numeric()

  review_text = 
    html %>%
    html_nodes(".review-text-content span") %>%
    html_text() %>% 
    str_replace_all("\n", "") %>% 
    str_trim()

  reviews = 
    tibble(
      title = review_titles,
      stars = review_stars,
      text = review_text
)
  
  reviews
  
}
```

Lets try my fucntion

``` r
dynamite_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=2"

read_page_reviews(dynamite_url)
```

    ## # A tibble: 10 x 3
    ##    title                               stars text                               
    ##    <chr>                               <dbl> <chr>                              
    ##  1 Movie is still silly fun....amazon…     1 We are getting really frustrated w…
    ##  2 Brilliant and awkwardly funny.          5 I've watched this movie repeatedly…
    ##  3 Great purchase price for great mov…     5 Great movie and real good digital …
    ##  4 Movie for memories                      5 I've been looking for this movie t…
    ##  5 Love!                                   5 Love this movie. Great quality     
    ##  6 Hilarious!                              5 Such a funny movie, definitely bro…
    ##  7 napoleon dynamite                       5 cool movie                         
    ##  8 Top 5                                   5 Best MOVIE ever! Funny one liners …
    ##  9 👍                                      5 Exactly as described and came on t…
    ## 10 A top favorite movie !!                 5 Love this movie, needed to add it …

Lets read a few pages of review

``` r
dynamite_url_base = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

dynamite_urls = str_c(dynamite_url_base, 1:50)

all_reviews = 
  bind_rows(
    read_page_reviews(dynamite_urls[1]),
    read_page_reviews(dynamite_urls[2]),
    read_page_reviews(dynamite_urls[3]),
    read_page_reviews(dynamite_urls[4]),
    read_page_reviews(dynamite_urls[5])
    )
```

## Mean scopeing example

``` r
f = function(x) {
  z = x + y
  z
}

x = 1
y = 2

f(x = y)
```

    ## [1] 4

## Functions as arguments

``` r
my_summary = function(x, summ_func) {
  
  summ_func(x)
  
}

x_vec = rnorm(100, 3, 7)
mean(x_vec)
```

    ## [1] 3.478362

``` r
median(x_vec)
```

    ## [1] 3.580191

``` r
my_summary(x_vec, IQR)
```

    ## [1] 10.0047
