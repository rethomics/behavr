
`behavr` [![Travis-CI Build Status](https://travis-ci.org/rethomics/behavr.svg?branch=master)](https://travis-ci.org/rethomics/behavr)
======================================================================================================================================

<!-- [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/tidyverse/hms?branch=master&svg=true)](https://ci.appveyor.com/project/tidyverse/hms)  -->
<!-- [![Coverage Status](https://img.shields.io/codecov/c/github/tidyverse/hms/master.svg)](https://codecov.io/github/tidyverse/hms?branch=master) [![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/hms)](https://cran.r-project.org/package=hms) -->
`behavr` is part of the [rethomics framework](todo.html). This README is a short explanation of the basics of `behavr`. A [comprehensive documentation](todo.html) of rethomics is also available.

Data structure for high-throughput biological experiments
---------------------------------------------------------

As behavioural biologists, we are often interested in recording behaviour of multiple animals. In our context, the *data* is a collection of recordings (i.e. time series) with often several variables (e.g. time, position, activity, ...). In addition to these recorded data, each animal may have different -- sometimes many -- experimental condition (e.g. age, sex, genotype, treatment, ...). These are also known as *metadata*.

During analysis of behaviour, it is very convenient to be able to **use both data and metadata together** in order to subset the data, alter or create new variables, and compute summary statistics. A natural approach would be to store data as a `data.frame` with one row per measurement and one column per variable, repeat the metadata as many time as there are reads, and put all the animals in the same data structure. Even though this is very convenient, in practice, it takes a lot of memory (because of the redundant metadata)!

Alternatively, one could keep data and metadata separated, and perform joins manually every time. However, this quickly becomes error prone, as metadata and data are not in the same structure. In addition, the cognitive burden is quite large too.

This package tries to solve this issue by offering a new data structure derived from `data.table`.

Installation
------------

``` r
library(devtools)
install_github("rethomics/behavr")
```

First steps
-----------

Let us create some toy data. Five animals, 100 reads (`t`, `x` ,`y` ,`eating`) per animal. **Both metadata and data must have the same key** (here, the column `id`).

``` r
library(data.table)
library(behavr)
set.seed(1)
met <- data.table::data.table(id = 1:5,
                             condition=letters[1:5],
                             sex=c("M","M","M","F", "F"),
                             t0 = c(100,2,-50,300,21),
                             key="id")

data <- met[  ,
             list(t=1L:100L,
                 x=rnorm(100),
                 y=rnorm(100),
                 eating=runif(100) > .5 ),
             by="id"]

print(data)
```

    ##      id   t          x           y eating
    ##   1:  1   1 -0.6264538 -0.62036668   TRUE
    ##   2:  1   2  0.1836433  0.04211587  FALSE
    ##   3:  1   3 -0.8356286 -0.91092165   TRUE
    ##   4:  1   4  1.5952808  0.15802877   TRUE
    ##   5:  1   5  0.3295078 -0.65458464   TRUE
    ##  ---                                     
    ## 496:  5  96 -2.0908461 -0.30824994   TRUE
    ## 497:  5  97  1.6973939  0.01551524  FALSE
    ## 498:  5  98  1.0638812 -0.44231772   TRUE
    ## 499:  5  99 -0.7666166 -1.63800773  FALSE
    ## 500:  5 100  0.3820076 -0.64140116  FALSE

Puting data and metadata together.

``` r
dt <- behavr(data, met)
print(dt)
```

    ##      id   t          x           y eating
    ##   1:  1   1 -0.6264538 -0.62036668   TRUE
    ##   2:  1   2  0.1836433  0.04211587  FALSE
    ##   3:  1   3 -0.8356286 -0.91092165   TRUE
    ##   4:  1   4  1.5952808  0.15802877   TRUE
    ##   5:  1   5  0.3295078 -0.65458464   TRUE
    ##  ---                                     
    ## 496:  5  96 -2.0908461 -0.30824994   TRUE
    ## 497:  5  97  1.6973939  0.01551524  FALSE
    ## 498:  5  98  1.0638812 -0.44231772   TRUE
    ## 499:  5  99 -0.7666166 -1.63800773  FALSE
    ## 500:  5 100  0.3820076 -0.64140116  FALSE

``` r
summary(dt)
```

    ##        id          t                x                  y          
    ##  Min.   :1   Min.   :  1.00   Min.   :-2.59611   Min.   :-3.2132  
    ##  1st Qu.:2   1st Qu.: 25.75   1st Qu.:-0.61433   1st Qu.:-0.7217  
    ##  Median :3   Median : 50.50   Median : 0.05844   Median :-0.1508  
    ##  Mean   :3   Mean   : 50.50   Mean   : 0.03275   Mean   :-0.0808  
    ##  3rd Qu.:4   3rd Qu.: 75.25   3rd Qu.: 0.76319   3rd Qu.: 0.6376  
    ##  Max.   :5   Max.   :100.00   Max.   : 3.05574   Max.   : 2.6757  
    ##    eating       
    ##  Mode :logical  
    ##  FALSE:265      
    ##  TRUE :235      
    ##                 
    ##                 
    ## 

Examples of what we can do with `behavr`
----------------------------------------

### Adding new variables

This works just like in `data.table`:

``` r
dt[, z := x+y]
print(dt)
```

    ##      id   t          x           y eating          z
    ##   1:  1   1 -0.6264538 -0.62036668   TRUE -1.2468205
    ##   2:  1   2  0.1836433  0.04211587  FALSE  0.2257592
    ##   3:  1   3 -0.8356286 -0.91092165   TRUE -1.7465503
    ##   4:  1   4  1.5952808  0.15802877   TRUE  1.7533096
    ##   5:  1   5  0.3295078 -0.65458464   TRUE -0.3250769
    ##  ---                                                
    ## 496:  5  96 -2.0908461 -0.30824994   TRUE -2.3990960
    ## 497:  5  97  1.6973939  0.01551524  FALSE  1.7129091
    ## 498:  5  98  1.0638812 -0.44231772   TRUE  0.6215634
    ## 499:  5  99 -0.7666166 -1.63800773  FALSE -2.4046244
    ## 500:  5 100  0.3820076 -0.64140116  FALSE -0.2593936

### Filtering using variable

Again like in `data.table`:

``` r
print(dt[t < 50])
```

    ##      id  t          x           y eating          z
    ##   1:  1  1 -0.6264538 -0.62036668   TRUE -1.2468205
    ##   2:  1  2  0.1836433  0.04211587  FALSE  0.2257592
    ##   3:  1  3 -0.8356286 -0.91092165   TRUE -1.7465503
    ##   4:  1  4  1.5952808  0.15802877   TRUE  1.7533096
    ##   5:  1  5  0.3295078 -0.65458464   TRUE -0.3250769
    ##  ---                                               
    ## 241:  5 45  1.9197705  0.79517402   TRUE  2.7149445
    ## 242:  5 46  0.8812778 -0.48820251   TRUE  0.3930753
    ## 243:  5 47  0.7420818 -0.90399345  FALSE -0.1619117
    ## 244:  5 48  0.1475734 -0.39041842   TRUE -0.2428450
    ## 245:  5 49  0.4853886  0.81406342   TRUE  1.2994520

``` r
# if we wanted to reasign:
# dt <- dt[t<50]
```

### Using meta variable

Say you want to get the **data** for all males only. If `sex` was a column of `dt`, we could just do `dt[sex=="M"]` Instead, `sex` is in metadata, so we need to **expand** it, using `xmd()`

``` r
print(dt[xmd(sex) == "M"])
```

    ##      id   t          x           y eating          z
    ##   1:  1   1 -0.6264538 -0.62036668   TRUE -1.2468205
    ##   2:  1   2  0.1836433  0.04211587  FALSE  0.2257592
    ##   3:  1   3 -0.8356286 -0.91092165   TRUE -1.7465503
    ##   4:  1   4  1.5952808  0.15802877   TRUE  1.7533096
    ##   5:  1   5  0.3295078 -0.65458464   TRUE -0.3250769
    ##  ---                                                
    ## 296:  3  96  0.7682782  0.14510663   TRUE  0.9133848
    ## 297:  3  97 -0.8161606 -2.44231132   TRUE -3.2584719
    ## 298:  3  98 -0.4361069  0.58031869   TRUE  0.1442118
    ## 299:  3  99  0.9047050  0.65505200   TRUE  1.5597570
    ## 300:  3 100 -0.7630863 -0.30450884  FALSE -1.0675951

This also works if we need to compute a data variable according to metadata. This is often the case when we need to standardise the positions or time compared to a reference, which is in the metadata. In this example, we want to express `t` relatively `t0` (`t = t-t0`). So we expand `t0`, with `xmd()`:

``` r
dt[, t := t - xmd(t0)]
print(dt)
```

    ##      id   t          x           y eating          z
    ##   1:  1 -99 -0.6264538 -0.62036668   TRUE -1.2468205
    ##   2:  1 -98  0.1836433  0.04211587  FALSE  0.2257592
    ##   3:  1 -97 -0.8356286 -0.91092165   TRUE -1.7465503
    ##   4:  1 -96  1.5952808  0.15802877   TRUE  1.7533096
    ##   5:  1 -95  0.3295078 -0.65458464   TRUE -0.3250769
    ##  ---                                                
    ## 496:  5  75 -2.0908461 -0.30824994   TRUE -2.3990960
    ## 497:  5  76  1.6973939  0.01551524  FALSE  1.7129091
    ## 498:  5  77  1.0638812 -0.44231772   TRUE  0.6215634
    ## 499:  5  78 -0.7666166 -1.63800773  FALSE -2.4046244
    ## 500:  5  79  0.3820076 -0.64140116  FALSE -0.2593936

### Accessing the metadata

Metadata is another `data.table` "inside" `dt`. To perform operations on the metadata, one can use `meta=TRUE` within the `[]` operator of `dt`. So, to see the metadata:

``` r
dt[meta=T]
```

    ##    id condition sex  t0
    ## 1:  1         a   M 100
    ## 2:  2         b   M   2
    ## 3:  3         c   M -50
    ## 4:  4         d   F 300
    ## 5:  5         e   F  21

Then, we can also use the same principle to filter:

``` r
dt[id>2 ,meta=T]
```

    ##    id condition sex  t0
    ## 1:  3         c   M -50
    ## 2:  4         d   F 300
    ## 3:  5         e   F  21

Note that this does not alter the metadata, it only shows a filtered copy.

More importantly, we can alter the metadata inline, for instance compute new columns (i.e. meta-variables). Here, we combine `sex` and `condition` as a `treatment`:

``` r
dt[, treatment:=interaction(condition,sex), meta=T]
dt[meta=T]
```

    ##    id condition sex  t0 treatment
    ## 1:  1         a   M 100       a.M
    ## 2:  2         b   M   2       b.M
    ## 3:  3         c   M -50       c.M
    ## 4:  4         d   F 300       d.F
    ## 5:  5         e   F  21       e.F

### Summaries

In the pipeline of behavioural analysis, one often needs to compute summary statistics **per animal**, and link them to the metadata. For example, here, we compute the median `x` position, and the proportion of `eating`:

``` r
summary_dt <- dt[,
                 .(median_x = median(x),
                   prop_eating= mean(eating)),
                by=id]

print(summary_dt)
```

    ##    id    median_x prop_eating
    ## 1:  1  0.11390916        0.47
    ## 2:  2  0.13533478        0.46
    ## 3:  3 -0.05678109        0.44
    ## 4:  4  0.11016320        0.46
    ## 5:  5  0.03277037        0.52

Now, we can **rejoin** the metadata. That is, we can reunite the metadata to summary data:

``` r
summary_all <- rejoin(summary_dt)
print(summary_all)
```

    ##    id condition sex  t0 treatment    median_x prop_eating
    ## 1:  1         a   M 100       a.M  0.11390916        0.47
    ## 2:  2         b   M   2       b.M  0.13533478        0.46
    ## 3:  3         c   M -50       c.M -0.05678109        0.44
    ## 4:  4         d   F 300       d.F  0.11016320        0.46
    ## 5:  5         e   F  21       e.F  0.03277037        0.52

Toy data
--------

`behavr` comes with functions to simulate toy data. See `?toy_activity_data`.

This is useful if you want quick way to generate a lot of data, say for prototyping functions. For instance, imagine you want to build a package to compute circadian rythm variables. You could generate 40 animals (two conditions) DAM like data using `toy_dam_data()`:

``` r
query<- data.frame(experiment_id="toy_experiment",
                    region_id=1:40, # 40 animals
                    condition=c("A","B"), # conditions A,B,A,B ...
                    # drift is a coeficient to drift the time so that we make 
                    # to slightly different periods see below
                    drift=c(1.001,1.000)
                    )
dt <- toy_activity_data(query,duration=days(10))
print(dt)
```

One could, for instance, *artificially* drift the time to make it look like *the two conditions have different circadian periods*:

    dt[, t:=t*xmd(drift)]
    dt

Going further
-------------

<!-- * [behavr](https://github.com/rethomics/behavr) -- to manipulate the data (create new variable/meta-variables) -->
-   [damr](https://github.com/rethomics/damr) -- to load data from the DAM2 system
-   [scopr](https://github.com/rethomics/scopr) -- to load data from the [ethoscope](http://gilestrolab.github.io/ethoscope/) system
-   [ggetho](https://github.com/rethomics/ggetho) -- to plot visualise the data
-   [sleepr](https://github.com/rethomics/sleepr) -- to perform sleep and circadian rythm analysis
