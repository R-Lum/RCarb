# Write table input template

This function creates a template table that can be used as input for the
function
[model_DoseRate](https://r-lum.github.io/RCarb/reference/model_DoseRate.md)

## Usage

``` r
write_InputTemplate(file = NULL, nrows = NULL, ...)
```

## Arguments

- file:

  [character](https://rdrr.io/r/base/character.html) (optional): output
  path, if `NULL` nothing is written, but a template
  [data.frame](https://rdrr.io/r/base/data.frame.html) is returned.

- nrows:

  [integer](https://rdrr.io/r/base/integer.html) (optionl): set the
  number of rows in the template, the default `NULL` is one.

- ...:

  additional arguments that can be passed to function
  [write.table](https://rdrr.io/r/utils/write.table.html) if
  `file != NULL`. Supported arguments are: `sep`, `dec`, `fileEncoding`

## Function version

0.1.0

## See also

[Example_Data](https://r-lum.github.io/RCarb/reference/Example_Data.md),
[write.table](https://rdrr.io/r/utils/write.table.html)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany)

## How to cite

Kreutzer, S., 2026. write_InputTemplate(): Write table input template.
Function version 0.1.0. In: Kreutzer, S., Nathan, R.P., Mauz, B., 2026.
RCarb: Dose Rate Modelling of Carbonate-Rich Samples . R package version
0.1.8. https://r-lum.github.io/RCarb/

## Examples

``` r

##create template without file creation
write_InputTemplate()
#>   SAMP_NAME    K  K_X    T  T_X   U  U_X U238 U238_X U234_U238 U234_U238_X WCI
#> 1   EXAMPLE 0.08 0.01 1.64 0.08 1.9 0.08    0      0         0           0  20
#>   WCI_X WCF WCF_X CC CC_X DIAM DIAM_X COSMIC COSMIC_X INTERNAL INTERNAL_X ONSET
#> 1     7   7     7 62    1  180     10   0.18     0.01        0          0   100
#>   ONSET_X FINISH FINISH_X DE DE_X
#> 1      10     40       10 98    9

if (FALSE) { # \dontrun{
##Example with file output

## set temporary filename
## (replace by own path if needed)
temp_file <- tempfile(pattern = "template", fileext = ".csv")
write_InputTemplate(file = temp_file)

} # }
```
