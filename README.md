# Dependence Explorer eXSD

![eXSD](./eXSD/inst/www/dexplorer-banner.jpg)

eXSD is an R software package that provides a platform for exploring spectral dependence in time series through different algorithms. eXSD provides an extensible unified visual exploration toolbox that can be easily integrated into R scripts through a simple programming interface. This package was written in the R programming language, and the R-Shiny framework, in order to ensure cross-platform capabilities and extensibility.

## Features

* Eight types of dependence methods integrated:
  * Correlation
  * Coherence
  * Partial coherence
  * Partial directed coherence
  * Lagged dual-frequency autocoherence [LDFA]
  * Principal components analysis
  * Spectral component analysis
  * Generalized dynamic principal component analysis.
* Partial Directed Coherence.
* Generalized Partial Directed Coherence.
* Recall that there are several methods to obtain spectral estimators. The following techniques were implemented for coherence and partial directed coherence:
  * Autoregressive (parametric) estimators.
  * Nonparametric spectral estimators (FFT)
* In addition, for coherence, partial coherence, and spectral principal component analysis:
  * Vector autoregressive estimators.
  * Generalized shrinkage estimator (PURE method [PURE]) as a semi-automatic trade-off between parametric and non-parametric methods.

## How to install

Make sure the dependences are installed:

```R
install.packages(c('vars', 'latex2exp', 'cowplot', 'extrafont', 'seewave', 'shinydashboard', 'ggbiplot', 'glmnet', 'BigVAR', 'shinyWidgets', 'gdpc', 'plotly'))
remotes::install_github("vqv/ggbiplot")
```

Then, install our package. Temporary, we suggest to use the tar.gz distributed in this repository:
```R
install.packages('eXSD_<latest-version>.tar.gz', repos = NULL, type='source')
```


## References

[PURE] Fiecas, M., & Ombao, H. (2011). The generalized shrinkage estimator for the analysis of functional connectivity of brain signals. The Annals of Applied Statistics, 5(2A), 1102-1125.
[LDFA] Gorrostieta, C., Ombao, H., Prado, R., Patel, S., & Eskandar, E. (2012). Exploring dependence between brain signals in a monkey during learning. Journal of Time Series Analysis, 33(5), 771-778