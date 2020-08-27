# GIGdist

The generalised inverse Gaussian (GIG) distribution is a probability distribution for positive continuous random variables that contains the gamma distribution and inverse-gamma distribution as its special cases.

This package provides a stable implementation of calculating the moments of the GIG distribution that involves the ratio between two besselK functions. This can encounter overflow and underflow issues if not addressed.


## Properties

The main property that makes the GIG distribution so useful is that it can be used to generate the generalised hyperbolic (GH) distribution which is a probability distribution for real random variables. This distribution contains the Student's t-distribution and the variance gamma (VG) distribution as a special case.
