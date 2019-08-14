# plotFun

## What is `plotFun`?

**plotFun** is a R package with helper functions for plotting meteorological data. It has been developed in the framework of the Horizon2020 [HEAT-SHIELD project](https://www.heat-shield.eu).

[![DOI](https://zenodo.org/badge/165884416.svg)](https://zenodo.org/badge/latestdoi/165884416)

****

### Installation

The recommended procedure for installing the package is using the devtools package. 

```R
devtools::install_github("anacv/plotFun")
```

The following plotting functions are available:
* ```plotFun.stn```: Plot a map of stations.
* ```plotFun.grid```: Plot a map of a regular grid.
* ```plotFun.ts```: Plot time series with or without temporal aggregation.
* ```plotFun.kde2d.hist```: Plot a 2D Kernel density plot with the histograms of the marginal distributions.
* ```plotFun.heat```: Plot heat index as a function of the input variables.

See e.g. ```?plotFun.stn``` for examples.


### Reference and further information: 
[![DOI](https://zenodo.org/badge/165884416.svg)](https://zenodo.org/badge/latestdoi/165884416)

This package was used in the following publications:
* Casanueva et al. 2019. Climate projections of a multi-variate heat stress index: the role of downscaling and bias correction, *Geoscientific Model Development*, https://www.geosci-model-dev.net/12/3419/2019/
* Casanueva et al. 2019. Escalating environmental heat exposure – a future threat for the European workforce, *Regional Environmental Change*.
