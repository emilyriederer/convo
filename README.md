# convo <img src='man/figures/logo.png' align="right" height="138" />

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of `convo` is to enable the creation of a a controlled vocabularly for naming columns in a relational dataset as described in my blog post [Column Names as Contracts](https://emilyriederer.netlify.app/post/column-name-contracts/). This controlled vocabularly can then be used to check a set of names for adherence, to automate documentation, and to generate data checks via the `pointblank` package.

## Installation

You can install the development version of convo from GitHub with:

``` r
devtools::install_github("emilyriederer/convo")
```

## Features

### Available

- Define controlled vocabularly (a `convo`) in R or YAML including valid name stubs at different levels of the ontology and optional descriptions or validation checks
- Parse stub lists (candidate `convo`s) from a set of variables
- Evaluate if a set of names adheres to a `convo` and identify violations
- Compare `convo` objects and/or stub lists with set-like operations (union, intersect, setdiff) to identify new candidates for inclusion
- Generate a `pointblank` validation agent or YAML file from a `convo` object for data validation
- Document a dataset with network diagrams or a table 

### Current Limitations / Future Enhancements

- Define overall metadata for controlled vocabularly metadata such as:
  + overall descriptor string
  + human-readable names describing each level
- Richer control over levels. Currently can only evaluate starting from the front, but in the future could:
  + allow some levels to be optional
  + work from both front and the back
- Current levels are independent of one another
  + could allow for truly hierarchical ontologies where allowed level 2 stubs vary by level 1 stub used
- Current assumption is that realizations of a controlled vocabularly are all delimited by the same separator
  + to work better with filepaths, might potentially want to enable multiple types of delimeters
- Current regex support slightly unreliable. Need to better document and expand

## Example

Main pieces of functionality are illustrated in the [Quick Start Guide](https://emilyriederer.github.io/convo/articles/quickstart-guide.html) on the package website.



