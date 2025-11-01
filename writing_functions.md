Writing Functions
================
2025-10-23

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.2     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.1.0     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(readxl)
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

## Start Small

everyone loves z scores… note `(x_vec - mean(x_vec)) / sd(x_vec)` this
is z transformation

``` r
x_vec = rnorm(20, mean = 10, sd = 3.5)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  0.51977187 -0.35182811 -0.03587646  0.10436726 -1.13117345  1.69773787
    ##  [7] -0.87824053  1.05801472 -0.35801508  0.10308089 -0.63951971 -0.51852278
    ## [13] -1.64674338 -1.83686990  0.45034082  1.64249477  1.24729658 -0.55796392
    ## [19]  0.22610641  0.90554214

write aka define a function to compute z scores note: `!is.numeric(x)`
means “not numeric”

``` r
z_scores = function(x) {
  
if(!is.numeric(x)) {
  stop("the input x shoud be numeric")
}
  
if(length(x) < 5) {
  stop("only compute z scores when the input has 5 or more numbers")
}
  
z = (x - mean(x)) / sd(x) 
  
z 

}
```

let’s try our function

``` r
z_scores(x = x_vec)
```

    ##  [1]  0.51977187 -0.35182811 -0.03587646  0.10436726 -1.13117345  1.69773787
    ##  [7] -0.87824053  1.05801472 -0.35801508  0.10308089 -0.63951971 -0.51852278
    ## [13] -1.64674338 -1.83686990  0.45034082  1.64249477  1.24729658 -0.55796392
    ## [19]  0.22610641  0.90554214

``` r
num_vec = rnorm(123, mean = 14, sd = 0.4)

z_scores(x = num_vec)
```

    ##   [1] -0.11791483  0.23989632 -0.75812840 -0.11570139 -0.47611200  1.25157041
    ##   [7]  0.56786536 -0.30140865 -0.99063108 -0.14461203  0.27279433  0.83145373
    ##  [13]  0.29456089  2.20813302 -1.32221887 -0.96190896 -0.64902470  0.31909766
    ##  [19] -0.04765725 -2.12208137 -1.11801215 -0.99889903 -0.59285108  0.18823215
    ##  [25]  1.44486307  1.38857191  0.45064434  0.97033227  0.33144467  0.40327479
    ##  [31] -0.81353908  1.14468707 -0.25775597 -0.97526844  1.25407091 -0.54547053
    ##  [37]  1.16339945  0.44431385 -0.50607536 -0.26343574  0.62336926  0.26878982
    ##  [43] -2.47966971  0.80493865  0.19986639 -1.25131308 -1.72481923 -1.23931462
    ##  [49]  1.58662663 -0.79532625  0.71381379 -0.99916339  2.03289427  1.04070442
    ##  [55] -0.30673625 -2.37603054  1.39623208 -2.22058292  0.94955800 -0.55592324
    ##  [61] -0.16175913  0.88258653  0.34388768  1.26237617 -1.53344987  0.36860550
    ##  [67] -1.44549146  0.61676199 -0.11148649  0.27490339  0.88078112  2.85120665
    ##  [73]  0.61072980 -1.47300876 -0.05499889 -0.61292590  1.61166596  1.50251609
    ##  [79]  0.46137210 -0.53982951  0.04224268 -0.15920597  0.02520990 -1.01900260
    ##  [85] -0.51568747  0.30904683  0.01611261 -1.22026237 -1.75802175 -0.24996457
    ##  [91] -0.78162976  1.59202963 -0.42982479 -0.50506728  0.49466770  0.47630089
    ##  [97] -0.02272754 -0.07058320  2.10159123 -0.03053257 -0.41133740  0.15462591
    ## [103] -0.70751781 -0.17797161 -1.69705507  1.84546763  0.07668239  0.73804060
    ## [109]  0.64367840 -0.32188832  0.23883705  0.17718764 -0.71484417 -0.54643056
    ## [115] -0.18671940  0.07959972  0.78156548 -0.84037906  0.40681612  1.03726380
    ## [121] -1.33694303  0.26591252 -0.29213877

let’s break our function

``` r
z_scores(3)
```

    ## Error in z_scores(3): only compute z scores when the input has 5 or more numbers

``` r
z_scores("my name is jeff")
```

    ## Error in z_scores("my name is jeff"): the input x shoud be numeric

## Let’s comput some stuff

Let’s compute and return the mean and sd of a numeric vector

``` r
mean_and_sd = function(x) {
  
 if(!is.numeric(x)) {
  stop("the input x shoud be numeric")
}
  
if(length(x) < 5) {
  stop("only compute mean and sd when the input has 5 or more numbers") 
}

mean_x = mean(x, na.rm = TRUE)
sd_x = sd(x, na.rm = TRUE)

tibble(
  mean = mean_x,
  sd = sd_x
)

}

mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.9  2.64

## make up data…

Let’s *simulate* some data

``` r
sim_df =
  tibble(
    x = rnorm(n = 30, mean = 3, sd = 2)
  )

sim_df %>% 
  summarize(
    mu_hat = mean(x),
    sigma_hat = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   2.95      2.18

write a function to do simulations

the inputs are *`n_subj` is number of subjects *`mu` is the true mean
\*`signma` is the true sd

function simulates data from a normal and computes sample mean sand sd.
(we wrote this in a code check last time…now I put the function in a R
script and placed it in a source folder)

``` r
source("source/sim_mean_sd.R")
```

Let’s run this function

``` r
sim_mean_sd()
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   2.96      1.53

``` r
sim_mean_sd(50)
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   2.75      1.97

``` r
sim_mean_sd(mu = 48, 50)
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   47.6      2.02

import LotR dataset

``` r
fellowship_ring = 
  read_excel("data/LotR_Words.xlsx", range = "B3:D6") %>% 
  mutate(movie = "Fellowship of the Ring")

two_towers = 
  read_excel("data/LotR_Words.xlsx", range = "F3:H6") %>% 
  mutate(movie = "Two Towers")

return_of_king = 
  read_excel("data/LotR_Words.xlsx", range = "j3:L6") %>% 
  mutate(movie = "Return of the King")

lotr_df = 
  bind_rows(fellowship_ring, two_towers, return_of_king)
```

import LotR dataset…with a function note: in your function parenthesis,
is what you are changing, then in the next lines you write what you want
to do originally substituting places with what is within your function
parenthesis

``` r
lotr_import= function(cell_range, movie_title) {
  
  df = 
    read_excel("data/LotR_Words.xlsx", range = cell_range) %>% 
    mutate(movie = movie_title)
  
  df
  
}

fellowship = lotr_import(cell_range = "B3:D6", movie_title = "Fellowship")
two_towers = lotr_import(cell_range = "F3:H6", movie_title = "Two Towers")
return_of_king = lotr_import(cell_range = "j3:L6", movie_title = "Return")

bind_rows(fellowship, two_towers, return_of_king)
```

    ## # A tibble: 9 × 4
    ##   Race   Female  Male movie     
    ##   <chr>   <dbl> <dbl> <chr>     
    ## 1 Elf      1229   971 Fellowship
    ## 2 Hobbit     14  3644 Fellowship
    ## 3 Man         0  1995 Fellowship
    ## 4 Elf       331   513 Two Towers
    ## 5 Hobbit      0  2463 Two Towers
    ## 6 Man       401  3589 Two Towers
    ## 7 Elf       183   510 Return    
    ## 8 Hobbit      2  2673 Return    
    ## 9 Man       268  2459 Return
