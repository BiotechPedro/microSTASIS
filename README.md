# µSTASIS

R package to be released in [Bioconductor v3.16](https://bioconductor.org/about/release-announcements/) for the assessment of microbiota stability across longitudinal samples.

`microSTASIS` was developed for the stability analysis of microbiota in a temporal framework by leveraging on iterative clustering. Concretely, Hartigan-Wong k-means algorithm is used as many times as possible for stressing out paired samples from the same individuals to test if they remain together for multiple numbers of clusters over a whole data set of individuals.

The metric **mS score**, is easy to interpret and provides a contextualized and intuitive metric to estimate temporal microbiota stability. Also, the package incorporates cross-validation routines (leave-one-out and k-fold), that compute the mean absolute error, and multiple functions to visualize the result.

The tool tries to fill the gap in microbiome research about temporal stability since the point of view of compositional data analysis.

<!---
## Intstallation instructions

Get the latest stable `R` release from [CRAN](http://cran.r-project.org/). Then install `microSTASIS` from [Bioconductor](http://bioconductor.org/) using the following code:

``` r
if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
}

BiocManager::install("microSTASIS")
```

And the development version from
[GitHub](https://github.com/BiotechPedro/microSTASIS) with:

``` r
BiocManager::install("BiotechPedro/microSTASIS")
```
-->

## Citation

We encourage the user to read the vignette and to cite the following paper: 

Pedro Sánchez-Sánchez, Francisco J Santonja, Alfonso Benítez-Páez, Assessment of human microbiota stability across longitudinal samples using iteratively growing-partitioned clustering, Briefings in Bioinformatics, Volume 23, Issue 2, March 2022, bbac055, https://doi.org/10.1093/bib/bbac055
