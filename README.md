# Analysis and Projection of Municipal Solid Waste

R package **mrwaste**, version **0.4.4**

[![CRAN status](https://www.r-pkg.org/badges/version/mrwaste)](https://cran.r-project.org/package=mrwaste)  [![R build status](https://github.com/pik-piam/mrwaste/workflows/check/badge.svg)](https://github.com/pik-piam/mrwaste/actions) [![codecov](https://codecov.io/gh/pik-piam/mrwaste/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/mrwaste) [![r-universe](https://pik-piam.r-universe.dev/badges/mrwaste)](https://pik-piam.r-universe.dev/builds)

## Purpose and Functionality

Reads in waste data from What a Waste 2.0 and uses brms
    package to create future regressions based on GDP.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("mrwaste")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact David Meng-Chuen Chen <david.chen@pik-potsdam.de>.

## Citation

To cite package **mrwaste** in publications use:

Chen D (2024). _mrwaste: Analysis and Projection of Municipal Solid Waste_. R package version 0.4.4.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {mrwaste: Analysis and Projection of Municipal Solid Waste},
  author = {David Meng-Chuen Chen},
  year = {2024},
  note = {R package version 0.4.4},
}
```
