
# target-causalclaims

Code associated with Garrison & Colleagues (2021) Causal Claims Paper.

# Running analyses

## Step 1: Recreate Package Environment with `renv`

renv is a package manager for R that aids in reproducibility by facilitating project-specific dependencies. For more information on it, see the [Introduction to renv Vignette](https://rstudio.github.io/renv/articles/renv.html). To ensure the analysis run smoothly, you can easily recreate our package environment as follows:

```r
if (!require("renv")) install.packages("renv")
renv::restore()
```

This recreates all of the R packages used in the analysis on your computer. Because renv works on a project-specific level, this should not affect where your R packages are installed on your machine or impact other analysis work with R.

## Step 2: Run the target workflow

The [targets](https://github.com/ropensci/targets) package is a pipeline toolkit for conducting analyses in R. It helps you maintain a reproducible workflow with minimal overhead by learning how code fits together and only running out-of-date tasks. For more information on the targets package, check out the [user manual](https://books.ropensci.org/targets/).

In order to re-run the analysis from the paper, all you need to do is call the folllowing and the targets package will re-run any necessary tasks, saving the output into `_targets/objects` folder.

```r
targets::tar_make()
```

## Step 3: Review Results

That's it! To inspect the underling regression objects, run `targets::tar_load(contains("results"))` or view a prettified table version in the `output/` folder.

---------------

GitHub Repository structure and README adapted from Nicholas Tierney's [ttiq-simulation](https://github.com/njtierney/ttiq-simulation) repo.
