<div id="main" class="col-md-9" role="main">

# Create a GTFParquet object

<div class="ref-description section level2">

Create a GTFParquet object

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
GTFParquet(path)
```

</div>

</div>

<div class="section level2">

## Arguments

-   path:

    Path to directory containing Parquet files from gtf\_to\_parquet.py

</div>

<div class="section level2">

## Value

A GTFParquet S4 object

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
gtf <- GTFParquet(system.file("gc49", package="lkparq"))
#> Error in normalizePath(path, mustWork = TRUE): path[1]="": No such file or directory
genes(gtf)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'genes': object 'gtf' not found
genes(gtf, filter = list(gene_type = "protein_coding"))
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'genes': object 'gtf' not found
```

</div>

</div>

</div>
