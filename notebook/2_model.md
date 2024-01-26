Spatial Modelling using STAN
================

## Introduction

When areal data exhibits a spatial structure where observations from
neighboring regions display a higher correlation than those from distant
regions, this correlation can be effectively addressed using a class of
spatial models known as **CAR** models (Conditional Auto-Regressive),
initially introduced by Besag (Besag 1974). Within the **CAR** model
framework, *Intrinsic Conditional Auto-Regressive* (**ICAR**) models
emerge as a specific subclass. The Besag York Mollié (BYM) model, on the
other hand, is a lognormal Poisson model that incorporates both an
**ICAR** component for spatial smoothing and a random-effects component
for non-spatial heterogeneity. This case study delves into the efficient
coding of these models in Stan. This brief tutorial provides an overview
of utilizing STAN in R for calibrating spatial regression models. It
demonstrates how STAN, when combined with standard R, serves as a
Bayesian inferential tool. The tutorial uses data from [Open Data
Lombardia](https://www.dati.lombardia.it/). The dataset provide sets of
polygons to define the health district of Regione Lombardia and Regione
Lombardia as the map projection.

## Autoregressive spatial model

**CAR** and **ICAR** models are applied when areal data comprises a
singular aggregated measure per areal unit, be it a binary, count, or
continuous value. Areal units essentially represent volumes, precisely
dividing a multi-dimensional volume $D$ into a finite number of
sub-volumes with distinct boundaries. Areal data, in contrast to point
data, is characterized by measurements taken at specific geo-spatial
points. Point data involves a continuous, real-valued distance measure
between points that can be automatically computed for any two points on
the map, facilitating the addition of new points. However, when dealing
with a set of areal units, there is no automated procedure for adding a
new areal unit. Consequently, models for areal data lack generative
capabilities concerning the areal regions.

For a set of $N$ areal units, the association between areal units is
elucidated through an $N \times N$ adjacency matrix, which is usually
written $A$ for adjacency, or $W$ for weights. In the context of binary
neighbor relationships, denoted as $W_{ij}$ is zero if counties $i$ and
$j$ are not contiguous, and $1$ if they are. In the case of **CAR**
models, the neighbor relationship is symmetric but not reflexive,
i.e. $W_{ij} = 0$ if counties $i = j$.

Given a set of observations taken at $N$ different areal units of a
region, spatial interactions between a pair of units $i$ and $j$ can be
modelled conditionally as a spatial random variable $\varphi$ which is
an $n$-length vector
$\varphi = (\varphi_1,\ldots,\varphi_n)^{\intercal}$. In the full
conditional distribution, each $\varphi_i$ is conditional on the sum of
the weighted values of its neighbors $(w_{ij}\varphi_j)$ and has unknown
variance:
$$\varphi_i \mid \varphi_j, j \neq i \sim N(\alpha \sum_{j = 1}^n w_{ij} \varphi_j, \tau_i^{-1})$$
where $\tau_i$ is a spatially varying precision parameter, and
$w_{ii} = 0$. Specification of the global, or joint distribution via the
local specification of the conditional distributions of the individual
random variables defines a *Gaussian Markov random field* (GMRF). Besag
(1974) proved that the corresponding joint specification of $\varphi$ is
a multivariate normal random variable centered at $0$. By *Brook’s
Lemma*, the joint distribution of $\varphi$ is then:
$$\varphi \sim N(0, [D_\tau (\mathbb{I}  - \alpha B)]^{-1})$$ Where we
are assuming the following:

- $D_\tau = \tau D$
- $D=diag(m_i)$: an $n \times n$ diagonal matrix with $m_i$ = the number
  of neighbors for location $i$
- $\mathbb{I}$: an $n \times n$ identity matrix
- $\alpha$: a parameter that controls spatial dependence ($\alpha = 0$
  implies spatial independence, and $\alpha = 1$ collapses to an
  *intrinsic conditional autoregressive* specification)
- $B=D^{−1}W$: the scaled adjacency matrix
- $W$: the adjacency matrix ($w_{ii}=0$, $w_{ij} = 1$ if $i$ is a
  neighbor of $j$, and $w_{ij}=0$ otherwise)

Then the model simplified is:

$$\varphi \sim N(0, [\tau (D - \alpha W)]^{-1})$$ The $\alpha$ parameter
ensures propriety of the joint distribution of $\varphi$ as long as
$|\alpha| < 1$ (Gelfand & Vounatsou 2003). However, $\alpha$ is often
taken as $1$, leading to the *IAR* specification which creates a
singular precision matrix and an improper prior distribution.

## Applications

Suppose we have aggregated count data $y_1,\ldots,y_n$ at $n$ locations,
and we expect that neighboring locations will have similar counts. With
a Poisson likelihood:

$$y_i \sim \text{Poisson}(\text{exp}(X_{i} \beta + \varphi_i + \log(\text{offset}_i)))$$
where $X_i$ is a design vector (the $i^{th}$ row from a design matrix),
$\beta$ is a vector of coefficients, $\varphi_i$ is a spatial
adjustment, and $\log(\text{offset}_i)$ accounts for differences in
expected values or exposures at the spatial units (popular choices
include area for physical processes, or population size for disease
applications). If we specify a proper CAR prior for $\varphi$, then we
have that $\varphi \sim \text{N}(0, [\tau (D - \alpha W)]^{-1})$ where
$\tau (D - \alpha W)$ is the precision matrix $\Sigma^{-1}$.

## References

+ [SVR via STAN, Chris Brunsdon](https://rpubs.com/chrisbrunsdon/503833)
+ [Conditional autoregressive (CAR) models, Connor Donegan](https://connordonegan.github.io/geostan/reference/stan_car.html)
+ [Geostatistical modelling with R and Stan, Sam Watson](https://aheblog.com/2016/12/07/geostatistical-modelling-with-r-and-stan/)
+ [STAN-IAR, Connor Donegan](https://github.com/ConnorDonegan/Stan-IAR)
+ [Spatial Models in Stan: Intrinsic Auto-Regressive Models for Areal Data, Mitzi Morris](https://mc-stan.org/users/documentation/case-studies/icar_stan.html#bym2-improving-the-parameterization-of-the-besag-york-and-mollie-model)
+ [Exact sparse CAR models in Stan, Max Joseph](https://mc-stan.org/users/documentation/case-studies/mbjoseph-CARStan.html)




