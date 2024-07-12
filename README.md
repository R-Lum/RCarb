




<!-- README.md was auto-generated by README.Rmd. Please DO NOT edit by hand!-->

# RCarb <img width=120px src="man/figures/Logo_RCarb.svg" align="right" />

[![CRAN](https://www.r-pkg.org/badges/version/RCarb)](https://CRAN.R-project.org/package=RCarb)
[![CRAN
DOI](https://img.shields.io/badge/doi-10.32614/CRAN.package.RCarb-blue.svg)](https://doi.org/10.32614/CRAN.package.RCarb)
[![DOI](https://zenodo.org/badge/151577249.svg)](https://zenodo.org/badge/latestdoi/151577249)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/RCarb)](https://www.r-pkg.org/pkg/RCarb)
[![R-CMD-check](https://github.com/R-Lum/RCarb/workflows/GitHub%20Actions%20CI/badge.svg)](https://github.com/R-Lum/RCarb/actions)
[![codecov](https://app.codecov.io/gh/R-Lum/RCarb/branch/master/graph/badge.svg)](https://app.codecov.io/gh/R-Lum/RCarb)

The **R** package `'RCarb'` provides a collection of various R functions
to model dose rates in carbonate-rich samples. The package is a
translation of the `'MATLAB'` program *Carb* by Roger P. Nathan.

## Installation of the developer version

To install the latest development builds of `'RCarb'` directly from
GitHub, run

``` r
if(!require("devtools"))
  install.packages("devtools")
devtools::install_github("R-Lum/RCarb@master")
```

To install a developer build other than `'master'`, replace the term
`'master'` in the code line by the name of the wanted developer build
(not available yet).

Please further note that our ‘GitHub’ versions are developer versions
and subject to daily changes. For stable please versions download the
package from [CRAN](https://CRAN.R-project.org/package=RCarb).

## License

This program is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or any later
version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the [GNU
General Public
License](https://github.com/R-Lum/RCarb/blob/master/LICENSE) for more
details.

## Related projects

- [Luminescence](https://github.com/R-Lum/Luminescence)
- [RLumModel](https://github.com/R-Lum/RLumModel)
- [RLumCarlo](https://github.com/R-Lum/RLumCarlo)
- [RLumShiny](https://github.com/R-Lum/RLumShiny)

## References

Kreutzer, S., Mauz, B., Martin, L., Mercier, N., 2019. ‘RCarb’: Dose
Rate Modelling of Carbonate-Rich Samples - an Implementation of Carb in
R -. Ancient TL 37, 1–8.
<http://ancienttl.org/ATL_37-2_2019/ATL_37-2_Kreutzer_p1-8.pdf>

Nathan, R.P., Mauz, B., 2008. On the dose-rate estimate of
carbonate-rich sediments for trapped charge dating. Radiation
Measurements 43, 14–25. doi:
[10.1016/j.radmeas.2007.12.012](https://dx.doi.org/10.1016/j.radmeas.2007.12.012)

Mauz, B., Hoffmann, D., 2014. What to do when carbonate replaced water:
Carb, the model for estimating the dose rate of carbonate-rich samples.
Ancient TL 32, 24–32.
<http://ancienttl.org/ATL_32-2_2014/ATL_32-2_Mauz_p24-32.pdf>

**Further reading**

Nathan, R.P., 2010. Numerical modelling of environmental dose rate and
its application to trapped-charge dating. DPhil thesis, St Hugh’s
College, Oxford. <https://ora.ox.ac.uk/objects/ora:6421>

## <span class="glyphicon glyphicon-euro"></span> Funding

- Between 2018-2019, the work of Sebastian Kreutzer as maintainer of the
  package was supported by LabEx LaScArBxSK (ANR - n. ANR-10-LABX-52).

- Between 01/2020-04/2022, Sebastian Kreutzer received funding from the
  European Union’s Horizon 2020 research and innovation programme under
  the Marie Skłodowska-Curie grant agreement No 844457 (project:
  CREDit).
