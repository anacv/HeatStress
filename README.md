# HeatStress

## What is `HeatStress`?

**HeatStress** is a R package for the calculation of heat stress indices. It has been developed in the framework of the Horizon2020 [HEAT-SHIELD project](https://www.heat-shield.eu).

[![DOI](https://zenodo.org/badge/82677974.svg)](https://zenodo.org/badge/latestdoi/82677974)

****

### Installation

The recommended procedure for installing the package is using the devtools package. 

```R
devtools::install_github("anacv/HeatStress")
```

A list of all available indices and the atomic functions calculating them is printed on screen with:

```R
library(HeatStress)
indexShow()
```

### Reference and further information: 

This package was used in the development of climate change scenarios of heat stress, in the following publications:
* Casanueva et al. 2019. Climate projections of a multi-variate heat stress index: the role of downscaling and bias correction, *Geoscientific Model Development*, https://www.geosci-model-dev-discuss.net/gmd-2018-294/
* Casanueva et al. 2019. Escalating environmental heat exposure – a future threat for the European workforce, *Regional Environmental Change*.
