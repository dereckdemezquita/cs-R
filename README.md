# cs-R <img src="./figures/project-logo.png" width="100" align="right">

Advanced features of R, things I've learned, notes, templates, and benchmarks. In this course I include things I learned during package development.

1. Better string interpolation: `stringr::str_interp`.
1. Better loops: conclusion `lapply` is better, if you have big data use `future.apply::future_lapply()`
    1. Consider the shape of your data, if you have a lot of columns calculations will take a long time, parallelisation might be useful.
    1. If you must use a `for` loop, initialise its vector output: `results <- vector(mode = "list", length = how_many_elements_do_you_expect)`
    1. Use this function for printing in parallel code: `messageParallel <- function(...) {system(sprintf('echo "%s"', paste0(..., collapse = "")))}`
1. Use `data.table`:
    1. `data.table` is an extension of `data.frame` they are compatible.
    1. Reshape your data (longer/wider) with `data.table`'s `melt()` and `dcast()` - a lot faster.
    1. Read/write `csv` files with `data.table::fread()`/`data.table::fwrite()`.
1. Matrices take up less space.
1. S4 class slot type check happens before class initialisation.

Here is some code and syntax I found myself using and re-using.

## Better string interpolation

Typically in `R` people build strings this way using `paste`.


```r
how_long <- "very long"
paste('This is a', how_long, 'string...', sep = " ")
```

```
## [1] "This is a very long string..."
```

A more natural and `JavaScript` way of building strings is using `stringr`:


```r
how_long <- "very long"
stringr::str_interp('This is a ${how_long} string...')
```

```
## [1] "This is a very long string..."
```

This allows for more natural syntax in the following way:


```r
for (i in seq_along(iris[,-ncol(iris)])) {
    print(stringr::str_interp('The mean for the column ${colnames(iris)[i]} is: ${round(mean(iris[,i]), 5)}'))
}
```

```
## [1] "The mean for the column Sepal.Length is: 5.84333"
## [1] "The mean for the column Sepal.Width is: 3.05733"
## [1] "The mean for the column Petal.Length is: 3.758"
## [1] "The mean for the column Petal.Width is: 1.19933"
```

## Better loops; `for` vs `apply`

If you come from other programming languages you might instinctively use a `for` loop in this way.


```r
separate_species <- vector(mode = "list", length = length(unique(iris$Species)))

for (i in seq_along(unique(iris$Species))) {
    separate_species[[i]] <- iris[as.character(iris$Species) == as.character(unique(iris$Species)[i]), -ncol(iris)]
    
    max_val <- max(separate_species[[i]])
    print(max_val)
}
```

```
## [1] 5.8
## [1] 7
## [1] 7.9
```

This has the side effect of polluting our global environment with variables.


```r
max_val
```

```
## [1] 7.9
```

```r
i
```

```
## [1] 3
```

In we can use something much better, `apply` type loops in conjunction with `list`s. This allows us to manipulate our data inside anonymous functions, and use `lapply`. The apply family of functions have a couple of advantages:

1. These are optimised in `R` and run on `C` code a lot faster than `for` loops.
1. These are embarrassingly parallel problems; these can easily be swapped out for `future` versions and run in parallel - `future.apply` package.



```r
species_names <- as.character(unique(iris$Species))
names(species_names) <- species_names

apply_sep <- lapply(species_names, function(species_names) {
    species <- iris[iris$Species == species_names, -ncol(iris)]
    print(max(species))
    return(species)
})
```

```
## [1] 5.8
## [1] 7
## [1] 7.9
```

### Benchmark: `for` vs `lapply`

Let's benchmark these two methods. Applies also work over `data.frames`, use all applies for going over columns, and `apply` with a margin of `1` for going over rows.

Let's start by simulating some data.


```r
generated_data <- (function(offset_min, offset_max, num_cols, num_samples) {
   generated_data <- list(
        subject_a = replicate(n = num_samples, expr = runif(n = num_cols, min = 10, max = 15), simplify = "data.frame"),
        subject_b = replicate(n = num_samples, expr = runif(n = num_cols, min = 10 + offset_min, max = 15 + offset_max), simplify = "data.frame")
    )
    
    generated_data <- mapply(function(data, group_name) { # this is a multivariate apply
        data <- as.data.frame(t(data))
        colnames(data) <- paste(rep("gene", ncol(data)), 1:ncol(data), sep = "_")
        data$subject <- group_name
        data$condition <- "control"
        return(data[,c("subject", "condition", setdiff(colnames(data), c("subject", "condition")))])
    }, generated_data, names(generated_data), SIMPLIFY = FALSE)
    
    bound_data <- do.call("rbind", unname(generated_data))
    
    bound_data$subject <- make.names(bound_data$subject, unique = TRUE)
    
    bound_data[((nrow(bound_data) / 2) + 1):nrow(bound_data),]$condition <- "test"
    return(bound_data)
})(100, 150, num_cols = 10000, num_samples = 40)

# let's save this for later
data.table::fwrite(generated_data, "./data/generated-data.csv")
```

We will generalise the above into a function:


```r
generateData <- function(offset_min, offset_max, num_cols, num_samples) {
   generated_data <- list(
        subject_a = replicate(n = num_samples, expr = runif(n = num_cols, min = 10, max = 15), simplify = "data.frame"),
        subject_b = replicate(n = num_samples, expr = runif(n = num_cols, min = 10 + offset_min, max = 15 + offset_max), simplify = "data.frame")
    )
    
    generated_data <- mapply(function(data, group_name) { # this is a multivariate apply
        data <- as.data.frame(t(data))
        colnames(data) <- paste(rep("gene", ncol(data)), 1:ncol(data), sep = "_")
        data$subject <- group_name
        data$condition <- "control"
        return(data[,c("subject", "condition", setdiff(colnames(data), c("subject", "condition")))])
    }, generated_data, names(generated_data), SIMPLIFY = FALSE)
    
    bound_data <- do.call("rbind", unname(generated_data))
    
    bound_data$subject <- make.names(bound_data$subject, unique = TRUE)
    
    bound_data[((nrow(bound_data) / 2) + 1):nrow(bound_data),]$condition <- "test"
    return(bound_data)
}
```

Let's do a simple t-test a relatively simple operation; here's a preview of our data:


```r
head(generated_data[, 1:5])
```

```
##       subject condition   gene_1   gene_2   gene_3
## 1   subject_a   control 13.59077 14.76026 11.60643
## 2 subject_a.1   control 12.98023 12.75922 11.05997
## 3 subject_a.2   control 12.48429 11.62539 13.36036
## 4 subject_a.3   control 10.34328 13.32511 10.81589
## 5 subject_a.4   control 11.94569 13.62541 13.40303
## 6 subject_a.5   control 11.58461 12.90442 10.59633
```

```r
tail(generated_data[, 1:5])
```

```
##         subject condition   gene_1   gene_2   gene_3
## 75 subject_b.34      test 152.5395 132.5556 157.4977
## 76 subject_b.35      test 128.8987 148.8747 131.3119
## 77 subject_b.36      test 128.8214 121.0501 152.2529
## 78 subject_b.37      test 132.7050 121.8610 156.1513
## 79 subject_b.38      test 151.6955 125.5584 148.8270
## 80 subject_b.39      test 112.0891 155.2052 149.5747
```

```r
dim(generated_data)
```

```
## [1]    80 10002
```

Let's start benchmarking with a dataset of size: columns 10 k, rows 40.

Now let's do our calculations over these data; we will use 4 different methods to loop the data.

1. Uninitialised for loop; this is a loop which grows a result vector. What happens here is that memory is allocated for every n size vector created at each iteration - **not efficient**.
1. Initialised for loop; here we predict how big our results vector will be and create it at that size from the begining.
1. Apply `lapply` loop. No need to create a vector this is done for us from the input data.
1. Future `future_lapply` the same as above but this code is run in parallel. As you might notice this might not be a lot faster than `lapply`; future parallel code will beat sequential code when the datasets become very large; the job is split up across each core of your processor.


```r
for_vs_lapply <- microbenchmark::microbenchmark(
    for_loop_uninitialised = ({
        results <- vector()
        for (i in 1:(ncol(generated_data) - 2)) {
            results[i] <- t.test(
                generated_data[, -c(1, 2)][generated_data$condition == "control", i],
                generated_data[, -c(1, 2)][generated_data$condition == "test", i],
                var.equal = TRUE
            )$p.value
        }
    }),
    for_loop_initialised = ({
        results <- vector(mode = "list", length = (ncol(generated_data) - 2))
        for (i in 1:(ncol(generated_data) - 2)) {
            results[i] <- t.test(
                generated_data[, -c(1, 2)][generated_data$condition == "control", i],
                generated_data[, -c(1, 2)][generated_data$condition == "test", i],
                var.equal = TRUE
            )$p.value
        }
    }),
    lapply_loop = ({
        lapply(generated_data[,-c(1, 2)], function(column) {
            # print(data)
            t.test(
                column[generated_data$condition == "control"],
                column[generated_data$condition == "test"],
                var.equal = TRUE
            )$p.value
        })
    }),
    future_lapply = ({
        future::plan(strategy = "multisession", workers = future::availableCores())
        future.apply::future_lapply(generated_data[,-c(1, 2)], function(column) {
            t.test(
                column[generated_data$condition == "control"],
                column[generated_data$condition == "test"],
                var.equal = TRUE
            )$p.value
        })
    }),
    times = 3
)
```

```r
ggplot2::autoplot(for_vs_lapply)
```

<div align="center"><img src="/figures/cs-bash_files/figure-html/microbench-plot-for-vs-lapply-10k-40-1.png" style="display: block; margin: auto;" /></div>

Consider the shape of your data, how many subjects how many observations? Now let's up the data size to 10k columns and 1.5k rows.


```r
generated_data <- generateData(100, 150, num_cols = 7500, num_samples = 1500)
```

Again the same calculations as above a t-test.


```r
for_vs_lapply <- microbenchmark::microbenchmark(
    for_loop_uninitialised = ({
        results <- vector()
        for (i in 1:(ncol(generated_data) - 2)) {
            results[i] <- t.test(
                generated_data[, -c(1, 2)][generated_data$condition == "control", i],
                generated_data[, -c(1, 2)][generated_data$condition == "test", i],
                var.equal = TRUE
            )$p.value
        }
    }),
    for_loop_initialised = ({
        results <- vector(mode = "list", length = (ncol(generated_data) - 2))
        for (i in 1:(ncol(generated_data) - 2)) {
            results[i] <- t.test(
                generated_data[, -c(1, 2)][generated_data$condition == "control", i],
                generated_data[, -c(1, 2)][generated_data$condition == "test", i],
                var.equal = TRUE
            )$p.value
        }
    }),
    lapply_loop = ({
        lapply(generated_data[,-c(1, 2)], function(column) {
            # print(data)
            t.test(
                column[generated_data$condition == "control"],
                column[generated_data$condition == "test"],
                var.equal = TRUE
            )$p.value
        })
    }),
    future_lapply = ({
        future::plan(strategy = "multisession", workers = future::availableCores())
        future.apply::future_lapply(generated_data[,-c(1, 2)], function(column) {
            t.test(
                column[generated_data$condition == "control"],
                column[generated_data$condition == "test"],
                var.equal = TRUE
            )$p.value
        })
    }),
    times = 3
)
```



```r
ggplot2::autoplot(for_vs_lapply)
```

<div align="center"><img src="/figures/cs-bash_files/figure-html/microbench-plot-for-vs-lapply-10k-1500-1.png" style="display: block; margin: auto;" /></div>

### Messaging and printing in future applies

Messaging back to the console is a challenge when using parallel code; use these functions to print to the console from parallel code:


```r
messageParallel <- function(...) {
    system(sprintf('echo "%s"', paste0(..., collapse = "")))
}
```


```r
generated_data <- generateData(100, 150, num_cols = 5, num_samples = 10)
```

Here we message back from a normal `apply` type function.


```r
invisible(mapply(function(column, column_name) {
    message(stringr::str_interp('We are on this column: ${column_name}'))
    t.test(
        column[generated_data$condition == "control"],
        column[generated_data$condition == "test"],
        var.equal = TRUE
    )$p.value
}, generated_data[,-c(1, 2)], names(generated_data[,-c(1, 2)]), SIMPLIFY = FALSE))
```

```
## We are on this column: gene_1
```

```
## We are on this column: gene_2
```

```
## We are on this column: gene_3
```

```
## We are on this column: gene_4
```

```
## We are on this column: gene_5
```

As you can see using the `message` function from paralle code doesn't work.


```r
future::plan(strategy = "multisession", workers = future::availableCores())

invisible(future.apply::future_mapply(function(column, column_name) {
    message(stringr::str_interp('We are on this column: ${column_name}'))
    t.test(
        column[generated_data$condition == "control"],
        column[generated_data$condition == "test"],
        var.equal = TRUE
    )$p.value
}, generated_data[,-c(1, 2)], names(generated_data[,-c(1, 2)]), SIMPLIFY = FALSE))
```

Here we use the `messageParallel` function we defined above:


```r
future::plan(strategy = "multisession", workers = future::availableCores())

invisible(future.apply::future_mapply(function(column, column_name) {
    messageParallel(stringr::str_interp('We are on this column: ${column_name}'))
    t.test(
        column[generated_data$condition == "control"],
        column[generated_data$condition == "test"],
        var.equal = TRUE
    )$p.value
}, generated_data[,-c(1, 2)], names(generated_data[,-c(1, 2)]), SIMPLIFY = FALSE))
```

```
## We are on this column: gene_1
```

```
## We are on this column: gene_2
```

```
## We are on this column: gene_3
```

```
## We are on this column: gene_4
```

```
## We are on this column: gene_5
```

# Memory and resource efficient programming

## Pivot and reshaping data

We should heavily use `data.table`. It has been heavily optimised and written mostly in multi-threaded `C` and `C++`. For instructions on installing a multi-threaded version of `data.table` you can check out my guide here: [Makevars](https://gist.github.com/dereckdemezquita/ed860601138a46cf591a1bdcc95db0a2).

Here I demonstrate melting data; pivot from wide to long dataset and benchmark the different methods.


```r
df_data <- as.data.frame(tidyr::relig_income)
tib_data <- tidyr::as_tibble(tidyr::relig_income)
DT_data <- data.table::as.data.table(tidyr::relig_income) # converts data which is a data.frame to data.table *by reference*
```

We will be using the following methods for benchmark:

1. `data.table::melt`
1. `tidyr::pivot_longer`
1. `reshape2::melt`
1. `reshape::melt`


```r
data.table::melt(DT_data, id.vars = "religion")
tidyr::pivot_longer(tib_data, -religion)
reshape2::melt(df_data, id.vars = "religion")
reshape::melt(df_data, id.vars = "religion")
```

We will also try these in combination with different data types. Some of these methods cast the data to other types or pass them to other methods; specifically `data.table` passes to `reshape2` if it receives a `data.frame` instead of a `data.table`.


```r
bench_pivoting <- microbenchmark::microbenchmark(
    dt_longer = data.table::melt(DT_data, id.vars = "religion"),
    dt_convert_longer = data.table::melt(data.table::as.data.table(df_data), id.vars = "religion"),
    dt_df_longer = data.table::melt(df_data, id.vars = "religion"),
    tidyr_longer = tidyr::pivot_longer(tib_data, -religion),
    reshape2 = reshape2::melt(df_data, id.vars = "religion"),
    reshape = reshape::melt(df_data, id.vars = "religion"),
    times = 100
)
```


```r
ggplot2::autoplot(bench_pivoting) +
    ggplot2::labs(title = "Comparing pivot long format data 1000 iterations", subtitle = "data.table is memory efficient - variables by reference not copies")
```

<div align="center"><img src="/figures/cs-bash_files/figure-html/microbench-plot-pivot-longer-methods-1.png" style="display: block; margin: auto;" /></div>

## Reading data and writing data

We previously saved some data; let's try and read it back in. A lot of different options: `read.csv`, `read_csv`, `vroom`, `fread`.


```r
reading_csv <- microbenchmark::microbenchmark(
    base_read_csv = ({data <- read.csv("./data/generated-data.csv")}),
    datatable_fread = ({data <- data.table::fread("./data/generated-data.csv")}),
    readr_read_csv = ({data <- readr::read_csv("./data/generated-data.csv")}),
    vroom_read_csv = ({data <- vroom::vroom("./data/generated-data.csv")}),
    times = 3
)
```


```r
ggplot2::autoplot(reading_csv)
```

<div align="center"><img src="/figures/cs-bash_files/figure-html/microbench-plot-read-data-1.png" style="display: block; margin: auto;" /></div>

Now let's write some data. As before `data.table` comes out on top again.


```r
read_in_data <- data.table::fread("./data/generated-data.csv")

writing_csv <- microbenchmark::microbenchmark(
    base_write_csv = ({write.csv(read_in_data, "./data/write-test-generated-data.csv")}),
    datatable_fwrite = ({data.table::fwrite(read_in_data, "./data/write-test-generated-data.csv")}),
    times = 3
)
```


```r
ggplot2::autoplot(writing_csv)
```

<div align="center"><img src="/figures/cs-bash_files/figure-html/microbench-plot-write-data-1.png" style="display: block; margin: auto;" /></div>

## Data structures and manipulation

### Object sizing

Matrices are better than data.frames. Consider the shape of your data, do you have a large number of columns?

Here in this example we have 200000 rows and 2 columns.


```r
m <- matrix(1:400000, 200000, 2)
d <- data.frame(m)

object.size(m)
```

```
## 1600216 bytes
```

```r
object.size(d)
```

```
## 1600848 bytes
```

```r
dim(m)
```

```
## [1] 200000      2
```

In this next example we have 200000 columns, and 2 rows.


```r
m <- matrix(1:400000, 2, 200000)
d <- data.frame(m)
object.size(m)
```

```
## 1600216 bytes
```

```r
object.size(d)
```

```
## 25600608 bytes
```

```r
dim(m)
```

```
## [1]      2 200000
```

In conclusion the more columns we have the larger the object is. This can be mitigated by using a `matrix` rather than `data.frame`.

# S4 and object oriented programming (OOP)

## Class validity check or initialisation first?

I had a question whether class initialisation happens before or after validity check. Specifically I wanted to know if I can pass a `list` type object and convert to a `data.table` in the `initialize` method.

Let's start with a data set; this is a `list` of `data.table`s.


```r
DT <- data.table::data.table(iris)

DT[, row_name := 1:nrow(DT)]

ls <- list(DT[, c("row_name", "Sepal.Length", "Sepal.Width")], DT[, c("row_name", "Petal.Length", "Petal.Width")])

# merge(ls[[1]], ls[[2]], by = "row_name")
```

Let's create the class.


```r
InitListDT <- setClass(
    Class = "InitListDT",
    slots = list(
        list_to_dt = "data.table"
    ),
    prototype = list(
        list_to_dt = data.table::data.table()
    )
)

setMethod("initialize", "InitListDT", function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)

    .Object@list_to_dt <- Reduce(function(...) {
        merge(..., by = "row_name")
    }, .Object@list_to_dt)

    return(.Object)
})

InitListDT(list_to_dt = ls)
```

```
## Error in validObject(.Object): invalid class "InitListDT" object: invalid object for slot "list_to_dt" in class "InitListDT": got class "list", should be or extend class "data.table"
```

In my example shown above you can see this is not possible. The class slots are set and checked before they are passed over to the initialisation method. We can solve this by allow for a `list` **or** `data.table` type in this slot.


```r
setClassUnion(
    "list_OR_data.table",
    members = c("list", "data.table")
)
```

```
## Warning: class "data.table" is defined (with package slot 'data.table') but no
## metadata object found to revise superClass information---not imported? Making a
## copy in package '.GlobalEnv'
```

```r
InitListDT <- setClass(
    Class = "InitListDT",
    slots = list(
        list_to_dt = "list_OR_data.table"
    ),
    prototype = list(
        list_to_dt = data.table::data.table()
    ),
    validity = function(object) {
        if(data.table::is.data.table(object@list_to_dt)) {
            if(colnames(object@list_to_dt)[1] != "row_name") {
                stop('First column name must be "row_name".')
            }
        }
    }
)

setMethod("initialize", "InitListDT", function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)

    .Object@list_to_dt <- Reduce(function(...) {
        merge(..., by = "row_name")
    }, .Object@list_to_dt)

    return(.Object)
})

object <- InitListDT(list_to_dt = ls)

head(object@list_to_dt)
```

```
##    row_name Sepal.Length Sepal.Width Petal.Length Petal.Width
## 1:        1          5.1         3.5          1.4         0.2
## 2:        2          4.9         3.0          1.4         0.2
## 3:        3          4.7         3.2          1.3         0.2
## 4:        4          4.6         3.1          1.5         0.2
## 5:        5          5.0         3.6          1.4         0.2
## 6:        6          5.4         3.9          1.7         0.4
```

A more simple example as follows:


```r
Test <- setClass(
    "Test",
    slots = list(
        yeet = "character"
    ),
    prototype = list(
        yeet = character()
    ),
    validity = function(object) {
        print('Hello from validity.')

        if(object@yeet[1] != "a") {
            stop('First element of slot yeet does not equal "a".')
        }
    }
)

setMethod("initialize", "Test", function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)

    print('Hello from initialisation.')

    .Object@yeet[1] <- "a"

    validObject(.Object)

    return(.Object)
})
```


```r
Test(yeet = c("b", "d", "e"))
```

```
## [1] "Hello from validity."
```

```
## Error in validityMethod(object): First element of slot yeet does not equal "a".
```

As we can see the print first comes from the validity check. In the vase of a valid object we can re-check validity after initialisation using the function `validObject`. 


```r
Test(yeet = c("a", "d", "e"))
```

```
## [1] "Hello from validity."
## [1] "Hello from initialisation."
## [1] "Hello from validity."
```

```
## An object of class "Test"
## Slot "yeet":
## [1] "a" "d" "e"
```

## Does class validation occur on all modifications?

I'm wondering if every time a slot gets modified the validation function gets called. We previously created an object of type `InitlistDT`. The first row must be named "row_name" if it's a `data.table`. Let's try modifying this and see if we get an error. We will do it through a direct modification first and then by using a `setter` method.


```r
test <- object@list_to_dt
colnames(test)[1] <- "yeet"

object@list_to_dt <- test
```

No error was thrown, let's try using a `setter` method now.


```r
object <- InitListDT(list_to_dt = ls)

setGeneric("accessSlot<-", function(obj, value) {
    standardGeneric("accessSlot<-")
})
```

```
## [1] "accessSlot<-"
```

```r
setMethod("accessSlot<-", "InitListDT", function(obj, value) {
    obj@list_to_dt <- value
    validObject(obj) # call validation function here
    return(obj)
})

accessSlot(object) <- test
```

```
## Error in validityMethod(object): First column name must be "row_name".
```

If we don't explicitly call the `validObject` function in out `setter` method then nothing is checked.

## Session info


```r
sessionInfo()
```

```
## R version 4.1.2 (2021-11-01)
## Platform: aarch64-apple-darwin20.6.0 (64-bit)
## Running under: macOS Big Sur 11.4
## 
## Matrix products: default
## BLAS:   /opt/homebrew/Cellar/openblas/0.3.18/lib/libopenblasp-r0.3.18.dylib
## LAPACK: /opt/homebrew/Cellar/r/4.1.2/lib/R/lib/libRlapack.dylib
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## loaded via a namespace (and not attached):
##  [1] tidyselect_1.1.1     xfun_0.29            bslib_0.3.1         
##  [4] purrr_0.3.4          reshape2_1.4.4       listenv_0.8.0       
##  [7] colorspace_2.0-2     vctrs_0.3.8          generics_0.1.1      
## [10] htmltools_0.5.2      yaml_2.2.1           utf8_1.2.2          
## [13] rlang_0.4.12         jquerylib_0.1.4      pillar_1.6.4        
## [16] glue_1.6.0           DBI_1.1.1            bit64_4.0.5         
## [19] lifecycle_1.0.1      plyr_1.8.6           stringr_1.4.0       
## [22] munsell_0.5.0        gtable_0.3.0         future_1.23.0       
## [25] codetools_0.2-18     evaluate_0.14        knitr_1.37          
## [28] tzdb_0.2.0           fastmap_1.1.0        parallel_4.1.2      
## [31] fansi_1.0.2          highr_0.9            Rcpp_1.0.8          
## [34] readr_2.1.1          scales_1.1.1         vroom_1.5.7         
## [37] jsonlite_1.7.2       bit_4.0.4            farver_2.1.0        
## [40] parallelly_1.30.0    microbenchmark_1.4.9 hms_1.1.1           
## [43] ggplot2_3.3.5        digest_0.6.29        stringi_1.7.6       
## [46] dplyr_1.0.7          grid_4.1.2           cli_3.1.0           
## [49] tools_4.1.2          magrittr_2.0.1       sass_0.4.0          
## [52] tibble_3.1.6         crayon_1.4.2         tidyr_1.1.4         
## [55] future.apply_1.8.1   pkgconfig_2.0.3      ellipsis_0.3.2      
## [58] data.table_1.14.2    rstudioapi_0.13      assertthat_0.2.1    
## [61] rmarkdown_2.11       reshape_0.8.8        R6_2.5.1            
## [64] globals_0.14.0       compiler_4.1.2
```
