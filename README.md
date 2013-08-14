darch
=====

### Create deep architectures in the R programming language


The darch package is build on the basis of the code from G. E. Hinton and R. R. Salakhutdinov 
(available under [Matlab Code for deep belief nets](http://www.cs.toronto.edu/~hinton/MatlabForSciencePaper.html 
"Matlab for science paper") : last visit: 01.08.2013).

This package is for generating neural networks with many layers (deep architectures) and train them with the method 
introduced by the publications "A fast learning algorithm for deep belief nets" (G. E. Hinton, S. Osindero, Y. W. 
Teh) and "Reducing the dimensionality of data with neural networks" (G. E. Hinton, R. R. Salakhutdinov). This method 
includes a pre training with the contrastive divergence method publishing by G.E Hinton (2002) and a fine tuning 
with common known training algorithms like backpropagation or conjugate gradient.


#### References
Hinton, G. E., S. Osindero, Y. W. Teh, A fast learning algorithm for deep belief nets,
Neural Computation 18(7), S. 1527-1554, DOI: 
[10.1162/neco.2006.18.7.1527](http://dx.doi.org/10.1162/neco.2006.18.7.1527), 2006.

Hinton, G. E., R. R. Salakhutdinov, Reducing the dimensionality of data with neural
networks, Science 313(5786), S. 504-507, DOI: 
[10.1126/science.1127647](http://dx.doi.org/10.1126/science.1127647), 2006.

Hinton, G. E., Training products of experts by minimizing contrastive divergence,
Neural Computation 14(8), S. 1711-1800, DOI: 
[10.1162/089976602760128018](http://dx.doi.org/10.1162/089976602760128018), 2002.
