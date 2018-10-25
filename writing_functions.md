
-----

title: “Writing functions”

output: github\_document

-----

## First function

``` r
x = rnorm(25, mean = 5, sd = 3)

(x - mean(x)) / sd(x)
##  [1] -0.78836148  0.46556567  0.11253929 -2.19459988 -0.56480422
##  [6] -0.90387043  0.06233340  0.36708413  0.14662717  2.03111318
## [11]  0.17575916 -0.09968021  0.01453766 -0.11938316 -0.85018771
## [16] -2.43936206  1.75633800 -0.36931755  1.25569840  0.28950136
## [21] -0.20436401  0.85826359  0.26275920  0.67024724  0.06556326

#a function that takes the sample as an argument, computes the vector of Z scores in the body, and returns the result.

z_scores = function(x) {
  
  z = (x - mean(x)) / sd(x)
  z
  
}

z_scores(x)
##  [1] -0.78836148  0.46556567  0.11253929 -2.19459988 -0.56480422
##  [6] -0.90387043  0.06233340  0.36708413  0.14662717  2.03111318
## [11]  0.17575916 -0.09968021  0.01453766 -0.11938316 -0.85018771
## [16] -2.43936206  1.75633800 -0.36931755  1.25569840  0.28950136
## [21] -0.20436401  0.85826359  0.26275920  0.67024724  0.06556326
```

check other examples

``` r
z_scores(3)
## [1] NA
z_scores("my name is jeff")
## Warning in mean.default(x): argument is not numeric or logical: returning
## NA
## Error in x - mean(x): non-numeric argument to binary operator
z_scores(iris)
## Warning in mean.default(x): argument is not numeric or logical: returning
## NA
## Warning in Ops.factor(left, right): '-' not meaningful for factors
## Error in is.data.frame(x): (list) object cannot be coerced to type 'double'
z_scores(sample(c(TRUE, FALSE), 25, replace = TRUE))
##  [1]  0.6721344 -1.4282857  0.6721344  0.6721344  0.6721344  0.6721344
##  [7]  0.6721344 -1.4282857 -1.4282857  0.6721344 -1.4282857  0.6721344
## [13]  0.6721344  0.6721344 -1.4282857  0.6721344  0.6721344 -1.4282857
## [19]  0.6721344 -1.4282857  0.6721344 -1.4282857  0.6721344  0.6721344
## [25]  0.6721344
```

These all did something I didn’t want, but only two returned errors.
we’ll add some checks on the argument values using conditional
statements.

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Z scores cannot be computed for length 1 vectors")
  }
  
  z = mean(x) / sd(x)
  
  z
}
```

## Multiple outputs

``` r
#creating a vector that contains both mean and sd

mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Cannot be computed for length 1 vectors")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)

  list(mean = mean_x, 
       sd = sd_x)
}
```

``` r
#create a data frame that contains both mean and sd. this is preferred

mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Cannot be computed for length 1 vectors")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)

  tibble(
    mean = mean_x, 
    sd = sd_x
  )
}
```

## Multiple inputs

simple linear regression

``` r
#simulate data
sim_data = tibble(
  x = rnorm(30, mean = 1, sd = 1),
  y = 2 + 3 * x + rnorm(30, 0, 1)
)

ls_fit = lm(y ~ x, data = sim_data)
  
beta0_hat = coef(ls_fit)[1]
beta1_hat = coef(ls_fit)[2]
```

Once you’re satisfied, it’s time to wrap things up in a function. I’d
like to be able to change the sample size and regression parameters, so
those will be my arguments; the code that simulates data and fits the
regression goes in the body; and the return statement should include the
intercept and slope. A function that does all this, using default values
for the intercept and slope, is below.

``` r
#n is sample size. beta0 is intercept, beta1 is slope
sim_regression = function(n, beta0 = 2, beta1 = 3) {
  
  sim_data = tibble(
    x = rnorm(n, mean = 1, sd = 1),
    y = beta0 + beta1 * x + rnorm(n, 0, 1)
  )
  
  ls_fit = lm(y ~ x, data = sim_data)
  
  tibble(
    beta0_hat = coef(ls_fit)[1],
    beta1_hat = coef(ls_fit)[2]
  )
}

#then you can put different n's as your input
sim_regression(n = 3000)
## # A tibble: 1 x 2
##   beta0_hat beta1_hat
##       <dbl>     <dbl>
## 1      1.99      3.01
```

## Some old examples

Amazon example

``` r
#bad code

url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-title") %>%
  html_text()

review_stars = dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text()

review_text = dynamite_html %>%
    html_nodes(".review-data:nth-child(4)") %>%
    html_text()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

write a function instead

``` r
read_page_reviews <- function(url) {
  
  h = read_html(url)
  
  review_titles = h %>%
    html_nodes("#cm_cr-review_list .review-title") %>%
    html_text()
  
  review_stars = h %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("\\d") %>%
    as.numeric()
  
  review_text = h %>%
    html_nodes(".review-data:nth-child(4)") %>%
    html_text()
  
  tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
  )
}
```

read in reviews from a few pages and combine the
results.

``` r
url_base = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="
urls = str_c(url_base, 1:5)

dynamite_reviews = bind_rows(
  read_page_reviews(urls[1]),
  read_page_reviews(urls[2]),
  read_page_reviews(urls[3]),
  read_page_reviews(urls[4]),
  read_page_reviews(urls[5])
)

dynamite_reviews
## # A tibble: 50 x 3
##    title                     stars text                                   
##    <chr>                     <dbl> <chr>                                  
##  1 "Great \"Odd Ball\" movi~     5 The dance scene was worth the time spe~
##  2 Nostalgic Stupidity           4 This movie is dumb. I won't lie and sa~
##  3 Happy                         5 Don't know why I lov this movie but ido
##  4 Go watch THE ROCK or dum~     2 This movie is horrible. How do so many~
##  5 My mom loves it               5 Got this for my mom for mother's day, ~
##  6 Nothing Quite Like It.        5 So much fun watching these listless pe~
##  7 Has pretty sweet bow ski~     5 Well, things are getting pretty seriou~
##  8 Great                         5 Great                                  
##  9 Fast delivery                 5 Bought as gift                         
## 10 Lol                           5 Funny                                  
## # ... with 40 more rows
```

## Functions as arguments

``` r
x = rnorm(25, 0, 1)

my_summary = function(x, summ_func) {
  summ_func(x)
}

my_summary(x, sd)
## [1] 1.003901
## [1] 0.8988712
my_summary(x, IQR)
## [1] 1.433719
## [1] 1.271572
my_summary(x, var)
## [1] 1.007817
## [1] 0.8079694
```

## Scoping and names

``` r
#r goes into global environment to find y. it computes x = 2 (because x = y) and y = 2.
f = function(x) {
  z = x + y
  z
}

x = 1
y = 2

f(x = y)
## [1] 4
```
