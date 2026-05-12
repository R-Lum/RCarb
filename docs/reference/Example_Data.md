# Example data

Example data as shipped with *Carb* by Mauz & Hoffmann (2014). In
contrast to the original data, `NA` values have been replaced by 0 and
columns and rows have been transposed. Samples are now organised in rows
and parameters in columns.

The data can be used to test `'RCarb'` and play with the secondary
carbonatisation process. Sample HD107 was renamed to LV107 for the sake
of consistency with Fig. 4 in Mauz & Hoffmann (2014).

## Format

`Example_Data`: [data.frame](https://rdrr.io/r/base/data.frame.html) (28
x 29)

Each column has two attributes:

- `UNIT`: the unit, so far applicable, e.g. "ppm"

- `DESCRIPTION`: the column description

## Version

0.1.0

## References

Mauz, B., Hoffmann, D., 2014. What to do when carbonate replaced water:
Carb, the model for estimating the dose rate of carbonate-rich samples.
Ancient TL 32, 24-32.
[doi:10.26034/la.atl.2014.481](https://doi.org/10.26034/la.atl.2014.481)

## Author

Mauz & Hoffmann (2014), with minor modifications by Sebastian Kreutzer,
Institute of Geography, Heidelberb University (Germany)

## Examples

``` r

## show first elements of the example data
data(Example_Data, envir = environment())
head(Example_Data)
#>   SAMP_NAME     K   K_X    T  T_X    U  U_X U238 U238_X U234_U238 U234_U238_X
#> 1     BN107 0.080 0.010 1.64 0.08 1.90 0.08    0      0         0           0
#> 2     BN102 0.170 0.009 2.59 0.03 3.02 0.07    0      0         0           0
#> 3     BN106 0.560 0.030 1.80 0.11 0.83 0.03    0      0         0           0
#> 4      LV61 0.131 0.005 0.85 0.03 0.86 0.11    0      0         0           0
#> 5      LV99 0.047 0.003 0.59 0.03 1.94 0.11    0      0         0           0
#> 6      D101 0.105 0.004 0.65 0.02 1.25 0.08    0      0         0           0
#>   WCI WCI_X WCF WCF_X CC CC_X DIAM DIAM_X COSMIC COSMIC_X INTERNAL INTERNAL_X
#> 1  20     7   7     7 62    1  180     10  0.180   0.0100        0          0
#> 2  20    10  10    10 68    1  180     22  0.180   0.0100        0          0
#> 3  20     6   6     6 49    1  145     15  0.180   0.0100        0          0
#> 4  12     5   2     2 17    1  210     30  0.069   0.0035        0          0
#> 5   8     3   5     5 61    3  210     30  0.182   0.0090        0          0
#> 6   8     3   2     2 59    2  210     20  0.180   0.0100        0          0
#>   ONSET ONSET_X FINISH FINISH_X  DE DE_X
#> 1   100      10     40       10  98    9
#> 2   100      10     40       10 130   10
#> 3   100      10     40       10 120   10
#> 4   120      10     40       10  52    5
#> 5    60      10     40       10  50    4
#> 6   180      10    130       10  81    5

##show only column U230
Example_Data$U238
#>  [1] 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00
#> [16] 0.31 0.00 0.00 0.32 0.35 0.35 0.30 0.15 0.19 0.14 0.60 0.21 0.68
#> attr(,"UNIT")
#> [1] "ppm"
#> attr(,"DESCRIPTION")
#> [1] "U-238 concentration"
```
