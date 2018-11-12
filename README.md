RCarb <img width=120px src="docs/img/Logo_RCarb.png" align="right" />
===================================================================

The **R** package 'RCarb' provides a collection of various R functions to model dose rates
in carbonate-rich samples. The package is a translation of the MATLAB program *Carb* by Roger P. Nathan.

[![CRAN](http://www.r-pkg.org/badges/version/RCarb)](http://cran.rstudio.com/package=RCarb)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/RCarb)](http://www.r-pkg.org/pkg/RCarb)
[![Build Status](https://travis-ci.org/R-Lum/RCarb.svg?branch=master)](https://travis-ci.org/R-Lum/RCarb)
[![Build status](https://ci.appveyor.com/api/projects/status/bjfy5lkqblrgvo15?svg=true)](https://ci.appveyor.com/project/RLumSK/rcarb)
[![codecov](https://codecov.io/gh/R-Lum/RCarb/branch/master/graph/badge.svg)](https://codecov.io/gh/R-Lum/RCarb)


## Installation of the developer version

To install the latest development builds directly from GitHub, run

```r
if(!require("devtools"))
  install.packages("devtools")
devtools::install_github("R-Lum/RCarb@master")
```

To install a developer build other than 'master', replace the term 'master' in the codeline by the name
of the wanted developer build (not available yet). 

Please further note that this version is a development version and may change day by day. 
For stable branches please visit the package on [CRAN](http://cran.rstudio.com/package=RCarb).

## License

This program is free software: you can redistribute it and/or modify it under the terms of the 
GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the [GNU General Public License](https://github.com/R-Lum/RCarb/blob/master/LICENSE) for more details.

## Related projects 

* [Luminescence](https://github.com/R-Lum/Luminescence)
* [RLumModel](https://github.com/R-Lum/RLumModel)
* [RLumShiny](https://github.com/R-Lum/RLumShiny)

## References

Nathan, R.P., Mauz, B., 2008. On the dose-rate estimate of carbonate-rich sediments for trapped charge dating. Radiation Measurements 43, 14–25. doi: https://dx.doi.org/10.1016/j.radmeas.2007.12.012

Mauz, B., Hoffmann, D., 2014. What to do when carbonate replaced water: Carb, the model for estimating the dose rate of carbonate-rich samples. Ancient TL 32, 24–32.

