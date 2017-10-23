# INLAOutputs

R package to process selected outputs form the `INLA` package.  

The `INLAOutputs` package has functions for the following tasks:

* Alternative return of fixed effects (exponentiated by default, highest probability density, quantiles).

* Alternative return of random effects (exponentiated by default).

* Calculate random effects in excess.

* Calculate the proportion of variance explained by each component of variance.

* Calculate measures of godness-of-fit and predictive performance.


* The functions to process random effects accept more than one model and process all random effects of each model.

* The functions to calculate measure of godness-of-fit and predictive preformance also accept more than one model and return models in order given by the performance.

To install `INLAOutputs`, use `install_github('oswaldosantos/INLAOutputs')` (make sure that `devtools` package is loaded).

For details, see:

* http://www.r-inla.org/home  
* Blangiardo, Marta, and Michela Cameletti. Spatial and Spatio-temporal Bayesian Models with R-INLA. John Wiley & Sons, 2015.
