# INLAOutputs

R package to process outputs form `INLA` package.  

The `INLAOutputs` package has functions for the following tasks:

* Exponentation of random marginals from Besag-York-Mollié (BYM) models, to produce posterior means of area-specific relative risks (odds ratio), compared to the mean relative risk (odds ratio).  

	* Exponentiated random marginals represent relative risks for the following likelihoods: poisson, zeroinflated.poisson.0, zeroinflated.poisson.1, zeroinflated.poisson.2, nbinomial, zeroinflated.nbinomial.0, zeroinflated.nbinomial.1, zeroinflated.nbinomial.2.  

	* Exponentiated random marginals represent odds ratios for the following likelihoods: binomial, zeroinflated.binomial.0, zeroinflated.binomial.1. 


* Exponentation of fixed marginals to produce posterior relative risks or odds ratios. The correspondence between the likelihood and the meassure of association is the same as above.  

* Compute the probability of the area-specific relative risks (odds ratio) being > 1, using the mean relative risk (odds ratio) as the reference category.  

* For BYM models, compute the proportion of variance explained by the spatially structured component.  

* Sort and display the DIC and WAIC of INLA models.  


To install `INLAOutputs`, use `install_github('oswaldosantos/INLAOutputs')` (make sure that `devtools` package is loaded).

For details, see:

* http://www.r-inla.org/home  
* Blangiardo, Marta, and Michela Cameletti. Spatial and Spatio-temporal Bayesian Models with R-INLA. John Wiley & Sons, 2015.
