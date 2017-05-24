# INLAOutputs

R package to process selected outputs form the `INLA` package.  

The `INLAOutputs` package has functions for the following tasks:

* Display fixed and random effects (marginals). When exponentiated (default), the fixed effects (FixedEffects function) represent posterior relative risks, the random effects (RandomEffects) reprsent area-specific posterior relative risks compared to the mean relative risk, and the combined fixed and random effects (SpecificEffects) represent area-specific relative risks.

* Calculate the proportion of variance spatially explained in Besg-York-Molliè models (VarSpatiallyExplained function).

* Calculate the area-specific excess risk in Besg-York-Molliè models (SpatialEffectsExcess). The excess risk is given by the probabilty of the relative risk is greater than 1.  

* Display computed WAIC or DIC of multiple models (WAIC and DIC functions).  

To install `INLAOutputs`, use `install_github('oswaldosantos/INLAOutputs')` (make sure that `devtools` package is loaded).

For details, see:

* http://www.r-inla.org/home  
* Blangiardo, Marta, and Michela Cameletti. Spatial and Spatio-temporal Bayesian Models with R-INLA. John Wiley & Sons, 2015.
