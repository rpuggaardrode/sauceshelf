
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sauceshelf

<!-- badges: start -->
<!-- badges: end -->

The `sauceshelf` library provides a framework for estimating and
comparing voice quality measures from various different sources. It can
serve as a wrapper for (a recently overhauled version of) the
[PraatSauce](https://kirbyj.github.io/praatsauce) suite of
[Praat](https://www.fon.hum.uva.nl/praat/) scripts for handily
estimating pitch, formants, harmonic amplitudes and spectral slope,
harmonics-to-noise ratio, cepstral peak prominence, root-mean-squared
intensity, and strength of harmonic excitation. Methods are also
provided for estimating these measures in R, relying on signal
processing functions in the
[emuverse](https://ips-lmu.github.io/EMU.html), and for mixing and
matching different estimation procedures. Finally, methods are provided
for loading in measures from
[VoiceSauce](https://www.phonetics.ucla.edu/voicesauce/) as a
well-formatted data frame, and for converting measures to SSFF files
(again, useful for users working within the “emuverse”).

`sauceshelf` is mainly aimed at a workflow where measures are estimated
for entire sound files prior to further analysis. PraatSauce is somewhat
more fully-featured than the other methods in `sauceshelf` in allowing
users to estimate voice quality in chunks of sound files based on
`.TextGrid` labels.

`sauceshelf` is under development. If you run into issues, you’re very
welcome to report them, either here on GitHub or via email to r.puggaard
at phonetik.uni-muenchen.de

## Installation

You can install the development version of sauceshelf from
[GitHub](https://github.com/) with:

``` r
# install.packages('devtools')
devtools::install_github('rpuggaardrode/sauceshelf')
```

## Calling PraatSauce

PraatSauce can be called with the `praatsauce()` function, which is
fully documented (see `?praatsauce`). If this doesn’t work at first,
arguments are available for setting your operating system `os`, which
will try to resolve the location of Praat on your machine, or for
explicitly setting this with the `praatLocation` argument. The purpose
of other arguments are described in detail
[here](https://kirbyj.github.io/praatsauce).

A simple call to `praatsauce()` looks like this:

``` r
library(sauceshelf)

datapath <- system.file('extdata/audio', package='sauceshelf')
sauce <- praatsauce(datapath)
head(sauce)
#>    file     t f0       F1       F2       F3 H1c H2c H4c       A1c       A2c
#> 1 1.wav 0.022 NA       NA       NA       NA  NA  NA  NA        NA        NA
#> 2 1.wav 0.027 NA       NA       NA       NA  NA  NA  NA        NA        NA
#> 3 1.wav 0.032 NA  964.766 1805.244 2703.854  NA  NA  NA -34.32804 -26.49718
#> 4 1.wav 0.037 NA 1182.248 1977.560 2660.562  NA  NA  NA -36.76248 -29.18773
#> 5 1.wav 0.042 NA 1146.152 1862.903 2484.158  NA  NA  NA        NA        NA
#> 6 1.wav 0.047 NA 1117.279 1799.946 2752.109  NA  NA  NA        NA        NA
#>         A3c H2Ku H5Ku H1u H2u H4u A1u A2u A3u        B1        B2        B3
#> 1        NA   NA   NA  NA  NA  NA  NA  NA  NA 103.32970 103.32970 103.32970
#> 2        NA   NA   NA  NA  NA  NA  NA  NA  NA 103.32970 103.32970 103.32970
#> 3 -13.95285   NA   NA  NA  NA  NA  NA  NA  NA  26.02938  34.54392  66.07613
#> 4 -22.33137   NA   NA  NA  NA  NA  NA  NA  NA  26.83238  38.70385  64.05331
#> 5        NA   NA   NA  NA  NA  NA  NA  NA  NA        NA        NA        NA
#> 6        NA   NA   NA  NA  NA  NA  NA  NA  NA        NA        NA        NA
#>   H1H2c H2H4c    H1A1c    H1A2c    H1A3c H2KH5Ku H1H2u H2H4u H1A1u H1A2u H1A3u
#> 1    NA    NA       NA       NA       NA      NA    NA    NA    NA    NA    NA
#> 2    NA    NA       NA       NA       NA      NA    NA    NA    NA    NA    NA
#> 3    NA    NA 34.32804 26.49718 13.95285      NA    NA    NA    NA    NA    NA
#> 4    NA    NA 36.76248 29.18773 22.33137      NA    NA    NA    NA    NA    NA
#> 5    NA    NA       NA       NA       NA      NA    NA    NA    NA    NA    NA
#> 6    NA    NA       NA       NA       NA      NA    NA    NA    NA    NA    NA
#>   CPP      HNR05     HNR15     HNR25     HNR35 intensity          soe
#> 1  NA -0.3315118 -2.002094 -3.515394 -4.586019        NA           NA
#> 2  NA -3.6469897 -3.701297 -5.962864 -5.478643        NA           NA
#> 3  NA -3.1088543 -3.441339 -4.572564 -5.805927        NA 3.549139e-05
#> 4  NA -1.7583520 -3.584764 -4.180622 -4.962196        NA 3.549139e-05
#> 5  NA -1.3698286 -2.533062 -5.254765 -4.978958        NA           NA
#> 6  NA -2.7257692 -4.227339 -5.420713 -7.188130        NA 5.579450e-05
```

The `mixedsauce()` function will combine signal processing in Praat and
R. By default, pitch, formants, CPP, HNR, and intensity are estimated in
Praat, while harmonic amplitudes, spectral slope, formant bandwidths,
and SoE are estimated in R:

``` r
sauce <- mixedsauce(datapath)
head(sauce)
#>    file     t f0       F1       F2       F3 CPP      HNR05     HNR15     HNR25
#> 1 1.wav 0.022 NA       NA       NA       NA  NA -0.3315118 -2.002094 -3.515394
#> 2 1.wav 0.027 NA       NA       NA       NA  NA -3.6469897 -3.701297 -5.962864
#> 3 1.wav 0.032 NA  964.766 1805.244 2703.854  NA -3.1088543 -3.441339 -4.572564
#> 4 1.wav 0.037 NA 1182.248 1977.560 2660.562  NA -1.7583520 -3.584764 -4.180622
#> 5 1.wav 0.042 NA 1146.152 1862.903 2484.158  NA -1.3698286 -2.533062 -5.254765
#> 6 1.wav 0.047 NA 1117.279 1799.946 2752.109  NA -2.7257692 -4.227339 -5.420713
#>       HNR35 intensity B1 B2 B3 H1c H2c H4c A1c A2c A3c H2Ku H5Ku H1u H2u H4u
#> 1 -4.586019        NA NA NA NA  NA  NA  NA  NA  NA  NA   NA   NA  NA  NA  NA
#> 2 -5.478643        NA NA NA NA  NA  NA  NA  NA  NA  NA   NA   NA  NA  NA  NA
#> 3 -5.805927        NA NA NA NA  NA  NA  NA  NA  NA  NA   NA   NA  NA  NA  NA
#> 4 -4.962196        NA NA NA NA  NA  NA  NA  NA  NA  NA   NA   NA  NA  NA  NA
#> 5 -4.978958        NA NA NA NA  NA  NA  NA  NA  NA  NA   NA   NA  NA  NA  NA
#> 6 -7.188130        NA NA NA NA  NA  NA  NA  NA  NA  NA   NA   NA  NA  NA  NA
#>   A1u A2u A3u H1H2c H2H4c H1A1c H1A2c H1A3c H2KH5Ku H1H2u H2H4u H1A1u H1A2u
#> 1  NA  NA  NA    NA    NA    NA    NA    NA      NA    NA    NA    NA    NA
#> 2  NA  NA  NA    NA    NA    NA    NA    NA      NA    NA    NA    NA    NA
#> 3  NA  NA  NA    NA    NA    NA    NA    NA      NA    NA    NA    NA    NA
#> 4  NA  NA  NA    NA    NA    NA    NA    NA      NA    NA    NA    NA    NA
#> 5  NA  NA  NA    NA    NA    NA    NA    NA      NA    NA    NA    NA    NA
#> 6  NA  NA  NA    NA    NA    NA    NA    NA      NA    NA    NA    NA    NA
#>   H1A3u          soe
#> 1    NA           NA
#> 2    NA           NA
#> 3    NA           NA
#> 4    NA           NA
#> 5    NA 9.944158e-05
#> 6    NA 8.228169e-05
```

`mixedsauce()` makes it easy to re-estimate spectral slope and harmonic
amplitudes if you’ve used other software to correct pitch and formants,
e.g. programmatically in R, or using tools like
[PitchMendR](https://github.com/tsostarics/PitchMendR).
