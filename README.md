# covid-impact
Repository for Chernozhukov, Kasahara, and Schrimpf "Causal Impact of Masks, Policies, Behavior on Early Covid-19 Pandemic in the U.S."

## Instructions

### Install Packages
We use many R packages. An easy way to install all the dependencies is with 

First install `renv`.
```r
if (!requireNamespace("remotes"))
  install.packages("remotes")

remotes::install_github("rstudio/renv")
```
You may also need to install pandoc. One way to do so is with the `installr` package. 

Clone this git repository. Make sure R's working directory is inside the cloned git repository and run the following:
```r
loadNamespace("renv") # library(renv) will overwrite some base functions and break things
rootdir <- system("git rev-parse --show-toplevel", intern=TRUE)[1]
setwd(rootdir) # this may not be needed, I don't completely know how renv works
renv::restore()
```

### Run the code
Code to generate the main regression results is in `rmd/regressionsWithPlots.Rmd`.

Code for the counterfactuals is in `rmd/counterfactuals.Rmd`. 

In either case, you can generate the latex tables, pdf figures, and html output by running

```r
library(rmarkdown)
rootdir <- system("git rev-parse --show-toplevel", intern=TRUE)[1]
rmarkdown::render(paste(rootdir,"cases_and_policies/rmd/regressionsWithPlots.Rmd", sep="/"))
rmarkdown::render(paste(rootdir,"cases_and_policies/rmd/counterfactuals.Rmd", sep="/"))
```

### Caveats

The usage of `renv` was added after the paper was published, so
versions of packages in the `renv` environment may not match those
used in the publication. In particular, it appears that `plm` has had
some changes in behavior (or another package has changes and
interferes with `plm`). I think I have updated the code to work, but
there might still be problems.

While updating the code to work with updated dependencies, a bug in
the code for the PI->B table was unknowingly corrected. The version of
the table in the published paper incorrectly has month included as an
integer. The corrected code includes month dummies. Other regressions
include month dummies in both the published version and the updated
code. Thanks to Philippe Lemoine for pointing out this discrepency. 
