# ortho2raw.R
Recovers raw-scale coefficients from linear models using orthogonal polynomials.

Would not have been possible without <a href="https://stats.stackexchange.com/questions/31858/recovering-raw-coefficients-and-variances-from-orthogonal-polynomial-regression">the answer to this question Stack Exchange</a>.

Tested with objects created by 'MASS' v7.3-51.4 glm.nb() and 'MuMIn' v1.43.6 model.avg().
