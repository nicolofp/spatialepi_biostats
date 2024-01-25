Spatial Modelling using STAN
================

## Introduction

This brief tutorial provides an overview of utilizing STAN in R for
calibrating spatial regression models. It demonstrates how STAN, when
combined with standard R, serves as a Bayesian inferential tool. The
tutorial uses data from [Open Data
Lombardia](https://www.dati.lombardia.it/). The dataset provide sets of
polygons to define the health district of Regione Lombardia and Regione
Lombardia as the map projection.

## Autoregressive spatial model

Before engaging in any Bayesian analysis, a relatively simple
autoregressive error model can be applied to the data using a maximum
likelihood approach. The data model in this context is represented as:
$$\mathbf{Y} = \alpha + \beta \mathbf{x} + \varepsilon$$ Here, $Y$ is
the outcome variable, $x$ is the variable of interest (both column
vectors),$\alpha$ and $\beta$ are regression coefficients, and
$\varepsilon$ is the random error term. However, instead of assuming
that $\varepsilon$ consists of independent errors, we posit that:
$$\varepsilon = \lambda \mathbf{W} \varepsilon + \zeta$$ where
$$\zeta_i \sim \textrm{N}(0,\sigma^2) \ \forall \  i$$ Here, $W$ is
linked to the contiguity matrix of health districts, where $W_{ij}$ is
zero if counties $i$ and $j$ are not contiguous, and $1/d_i$ if they
are, with $d_i$ being the number of counties contiguous to county $i$,
Thus, $W$ is essentially functions as a smoothing matrix that replaces
the value of a certain quantity at county $i$ with the mean of its
neighbors. In this case, the smoothing is applied to the error terms,
and an additional error term ($\zeta$) is introduced. The parameter
$\lambda$ governs the extent of spatial correlation; if $\lambda = 0$,
the process is equivalent to the ordinary least squares model.
