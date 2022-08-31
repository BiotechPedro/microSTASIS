# microSTASIS

R package to be released in Bioconductor v3.16 for the assessment of microbiota stability across longitudinal samples.

**μSTASIS** was developed for the stability analysis of microbiota in a temporal framework by leveraging on iterative clustering. Concretely, Hartigan-Wong k-means algorithm is used as many times as possible for stressing out paired samples from the same individuals to test if they remain together for multiple numbers of clusters over a whole data set of individuals.

The metric **mS score**, is easy to interpret and provides a contextualized and intuitive metric to estimate temporal microbiota stability. Also, the package incorporates cross-validation routines (leave-one-out and k-fold), that compute the mean absolute error, and multiple functions to visualize the result.

The tool tries to fill the gap in microbiome research about temporal stability since the point of view of compositional data analysis.

We encourage the user to read the vignette and to cite the following paper: 

Pedro Sánchez-Sánchez, Francisco J Santonja, Alfonso Benítez-Páez, Assessment of human microbiota stability across longitudinal samples using iteratively growing-partitioned clustering, Briefings in Bioinformatics, Volume 23, Issue 2, March 2022, bbac055, https://doi.org/10.1093/bib/bbac055
