# Reference data

Reference data and correction factors for beta and gamma radiation used
for internal calculations. These values are used instead of the
correction factors given in Aitken (1985) for the carbonate model.

## Format

`Reference_Data`: [list](https://rdrr.io/r/base/list.html)\

|  |  |  |  |
|----|----|----|----|
| **NAME** | **TYPE** | **DIM** | **DESCRIPTION** |
| `DATAek` | `matrix` | 4 x 4 | correction factors for electrons for water and carbonate to sediment mass ratio for K |
| `DATAet` | `matrix` | 4 x 4 | correction factors for electrons for water and carbonate to sediment mass ratio for Th |
| `DATAet230` | `matrix` | 4 x 4 | correction factors for electrons for water and carbonate to sediment mass ratio for Th-230 |
| `DATAeu` | `matrix` | 4 x 4 | correction factors for electrons for water and carbonate to sediment mass ratio for U |
| `DATAeu234` | `matrix` | 4 x 4 | correction factors for electrons for water and carbonate to sediment mass ratio for U-234 |
| `DATAeu238` | `matrix` | 4 x 4 | correction factors for electrons for water and carbonate to sediment mass ratio for U-238 |
| `DATApk` | `matrix` | 4 x 4 | correction factors for photons for water and carbonate to sediment mass ratio for K |
| `DATApt` | `matrix` | 4 x 4 | correction factors for photons for water and carbonate to sediment mass ratio for T |
| `DATApt230` | `matrix` | 4 x 4 | correction factors for photons for water and carbonate to sediment mass ratio for Th-230 |
| `DATApu` | `matrix` | 4 x 4 | correction factors for photons for water and carbonate to sediment mass ratio for U |
| `DATApu234` | `matrix` | 4 x 4 | correction factors for photons for water and carbonate to sediment mass ratio for U-234 |
| `DATApu238` | `matrix` | 4 x 4 | correction factors for photons for water and carbonate to sediment mass ratio for U-238 |
| `mejdahl` | `data.frame` | 36 x 4 | beta-dose attenuation values for quartz grains according to Mejdahl (1979) |
| `DR_conv_factors` | `data.frame` | 4 x 13 | beta and gamma dose rate conversion factors used internally (see details) |

## Details

The reference values are used internally to account for: (1) grain size
depend beta-attenuation factors (Mejdahl, 1979) and (2) to correct
nuclide dependent beta and gamma radiation for water/carbonate
proportions. The latter values are given as matrix and precise values
are interpolated during the modelling process.

Additionally 'RCarb' provides and own set of dose rate conversion
factors to convert concentrations of U, Th, and K to dose rate values.
Historically *Carb* (and thus 'RCarb') as its own dose rate conversion
factors, which differ slightly from other published values. To provide a
consistent calculation approach by default the 'old' *Carb* values are
used, but the user can further switch (see
[model_DoseRate](https://r-lum.github.io/RCarb/reference/model_DoseRate.md))
to values provided by Adamiec & Aitken (1998), Guérin et al. (2011),
Liritzis et al. (2013) or Cresswell et al. (2018).

Different values quoted for U-238 and U-234 accounts for different
activity ratios. For further details on the origin of these data we
refer to Nathan & Mauz (2008) and Nathan (2010).\

**Nuclear data origin according to Nathan & Mauz (2008)**

The gamma primary energy spectra of uranium, thorium and potassium are
drawn from Evaluated Nuclear Structure Data File (ENSDF) database at
<https://www.nndc.bnl.gov> (2002-01-16) and the beta primary energy
spectra was derived from ENSDF end-point energies using a Fermi beta
decay model (Evans, 1955) modified by Behrens & Szybisz (1976). For the
simulations of the collisional mass stopping powers for quartz the
software ESTAR (Berger et al., 2000) was used. The mass
energy-absorption coefficients for quartz were tabulated by Hubbell &
Seltzer (2004).

*For further details and references please read Nathan & Mauz (2008)*

## Version

0.2.0

## References

Adamiec, G., Aitken, M.J., 1998. Dose-rate conversion factors: update.
Ancient TL 16, 37–50.
[doi:10.26034/la.atl.1998.292](https://doi.org/10.26034/la.atl.1998.292)

Guérin, G., Mercier, N., Adamiec, G., 2011. Dose-rate conversion
factors: update. Ancient TL 29, 5–9.
[doi:10.26034/la.atl.2011.443](https://doi.org/10.26034/la.atl.2011.443)

Cresswell, A.J., Carter, J., Sanderson, D.C.W., 2018. Dose rate
conversion parameters: Assessment of nuclear data. Radiation
Measurements 120, 195–201.
[doi:10.1016/j.radmeas.2018.02.007](https://doi.org/10.1016/j.radmeas.2018.02.007)

Liritzis, I., Stamoulis, K., Papachristodoulou, C., Ioannides, K., 2013.
A Re-Evaluation of Radiation Dose-Rate Conversion Factors. Mediterranean
Archaeology and Archaeometry 12, 1–15.
<https://www.maajournal.com/index.php/maa/article/view/1012/921>

Mejdahl, V., 1979. Thermoluminescence dating: beta-dose attenuation in
quartz grains. Archaeometry 21, 61-72.
[doi:10.1111/j.1475-4754.1979.tb00241.x](https://doi.org/10.1111/j.1475-4754.1979.tb00241.x)

Nathan, R.P., Mauz, B., 2008. On the dose-rate estimate of
carbonate-rich sediments for trapped charge dating. Radiation
Measurements 43, 14-25.
[doi:10.1016/j.radmeas.2007.12.012](https://doi.org/10.1016/j.radmeas.2007.12.012)

Nathan, R.P., 2010. Numerical modelling of environmental dose rate and
its application to trapped-charge dating. DPhil thesis, St Hugh's
College, Oxford. <https://ora.ox.ac.uk/objects/ora:6421>\

**Further reading**

Aitken, M.J., 1985. Thermoluminescence dating. Academic Press.

Berger, M.J., Coursey, J.S., Zucker, M.A., 2000. ESTAR, PSTAR, and
ASTAR: Computer Programs for Calculating Stopping-Power and Range Tables
for Electrons, Protons, and Helium Ions (version 1.2.2).
https://physics.nist.gov/Star (2005-08-09). National Institute of
Standards and Technology, Gaithersburg, MD.

Behrens, H., Szybisz, L., 1976. Shapes of beta spectra. Physics Data
6-1, Zentralstelle fuer Atomkernenergie-Dokumentation (ZAED), Germany.

Evans, R.D., 1955. The Atomic Nucleus. McGraw-Hill, NY.

Hubbell, J.H., Seltzer, S.M., 2004. Tables of X-Ray Mass Attenuation
Coefficients and Mass Energy-Absorption Coefficients (version 1.4).
https://physics.nist.gov/xaamdi (2005-08-09). National Institute of
Standards and Technology, Gaithersburg, MD.

## Examples

``` r

data(Reference_Data, envir = environment())
str(Reference_Data)
#> List of 14
#>  $ DATAek         : num [1:4, 1:4] 1 1.195 1.197 1.189 0.995 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : chr [1:4] "1e-06" "0.04" "0.2" "1"
#>   .. ..$ : chr [1:4] "1e-06" "0.04" "0.2" "1"
#>  $ DATAet         : num [1:4, 1:4] 1 1.195 1.201 1.192 0.993 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : chr [1:4] "1e-06" "0.04" "0.2" "1"
#>   .. ..$ : chr [1:4] "1e-06" "0.04" "0.2" "1"
#>  $ DATAet230      : num [1:4, 1:4] 1 1.236 1.25 1.244 0.997 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : chr [1:4] "1e-06" "0.04" "0.2" "1"
#>   .. ..$ : chr [1:4] "1e-06" "0.04" "0.2" "1"
#>  $ DATAeu         : num [1:4, 1:4] 1 1.193 1.199 1.187 0.992 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : chr [1:4] "1e-06" "0.04" "0.2" "1"
#>   .. ..$ : chr [1:4] "1e-06" "0.04" "0.2" "1"
#>  $ DATAeu234      : num [1:4, 1:4] 1 1.248 1.256 1.249 0.997 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : chr [1:4] "1e-06" "0.04" "0.2" "1"
#>   .. ..$ : chr [1:4] "1e-06" "0.04" "0.2" "1"
#>  $ DATAeu238      : num [1:4, 1:4] 1 1.187 1.189 1.177 0.993 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : chr [1:4] "1e-06" "0.04" "0.2" "1"
#>   .. ..$ : chr [1:4] "1e-06" "0.04" "0.2" "1"
#>  $ DATApk         : num [1:4, 1:4] 1 1.06 1.06 1.04 1.02 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : chr [1:4] "1e-06" "0.04" "0.2" "1"
#>   .. ..$ : chr [1:4] "1e-06" "0.04" "0.2" "1"
#>  $ DATApt         : num [1:4, 1:4] 1 1.023 1.015 0.987 1.039 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : chr [1:4] "1e-06" "0.04" "0.2" "1"
#>   .. ..$ : chr [1:4] "1e-06" "0.04" "0.2" "1"
#>  $ DATApt230      : num [1:4, 1:4] 1 0.26 0.252 0.236 1.494 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : chr [1:4] "1e-06" "0.04" "0.2" "1"
#>   .. ..$ : chr [1:4] "1e-06" "0.04" "0.2" "1"
#>  $ DATApu         : num [1:4, 1:4] 1 1.015 1.007 0.976 1.044 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : chr [1:4] "1e-06" "0.04" "0.2" "1"
#>   .. ..$ : chr [1:4] "1e-06" "0.04" "0.2" "1"
#>  $ DATApu234      : num [1:4, 1:4] 1 0.208 0.206 0.202 1.527 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : chr [1:4] "1e-06" "0.04" "0.2" "1"
#>   .. ..$ : chr [1:4] "1e-06" "0.04" "0.2" "1"
#>  $ DATApu238      : num [1:4, 1:4] 1 0.814 0.792 0.716 1.16 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : chr [1:4] "1e-06" "0.04" "0.2" "1"
#>   .. ..$ : chr [1:4] "1e-06" "0.04" "0.2" "1"
#>  $ mejdahl        :'data.frame': 36 obs. of  4 variables:
#>   ..$ D : num [1:36] 0.005 0.01 0.015 0.02 0.03 0.04 0.05 0.06 0.07 0.08 ...
#>   ..$ K : num [1:36] 0.0018 0.0035 0.0053 0.0071 0.0106 0.0141 0.0177 0.0212 0.0248 0.0283 ...
#>   ..$ Th: num [1:36] 0.0225 0.0366 0.0484 0.0582 0.0743 ...
#>   ..$ U : num [1:36] 0.012 0.0214 0.0296 0.0366 0.0475 0.0564 0.0642 0.0713 0.0779 0.084 ...
#>  $ DR_conv_factors:'data.frame': 5 obs. of  13 variables:
#>   ..$ REFERENCE: chr [1:5] "Carb2007" "Adamiec_Aitken_1998" "Guerin_et_al_2011" "Liritzis_et_al_2013" ...
#>   ..$ UB       : num [1:5] 0.146 0.146 0.146 0.146 0.142
#>   ..$ UB_X     : num [1:5] 1e-03 NA NA 4e-04 2e-03
#>   ..$ TB       : num [1:5] 0.027 0.0273 0.0277 0.0275 0.028
#>   ..$ TB_X     : num [1:5] 1e-03 NA NA 9e-04 1e-03
#>   ..$ KB       : num [1:5] 0.786 0.782 0.798 0.801 0.854
#>   ..$ KB_X     : num [1:5] 0.008 NA NA 0.0073 0.008
#>   ..$ UG       : num [1:5] 0.113 0.113 0.112 0.112 0.112
#>   ..$ UG_X     : num [1:5] 2e-03 NA NA 2e-04 1e-03
#>   ..$ TG       : num [1:5] 0.048 0.0476 0.0479 0.0481 0.0489
#>   ..$ TG_X     : num [1:5] 2e-03 NA NA 2e-04 3e-04
#>   ..$ KG       : num [1:5] 0.245 0.243 0.249 0.25 0.248
#>   ..$ KG_X     : num [1:5] 0.005 NA NA 0.0048 0.003
Reference_Data$DATAek
#>        1e-06   0.04    0.2      1
#> 1e-06 1.0000 0.9946 0.9936 0.9911
#> 0.04  1.1948 1.0944 1.0266 0.9985
#> 0.2   1.1973 1.1631 1.0922 1.0230
#> 1     1.1886 1.1808 1.1546 1.0850
```
