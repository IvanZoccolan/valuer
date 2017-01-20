#valuer 1.1.1.9000
* Argument checks in fair_fee to prevent convergence failures (#3).
* Added va_bs_engine2 to price with a GBM fund and stochastic mortality.

# valuer 1.1.1

*  Fixed an heap overflow bug in valuer's C code that could crash R (#2).
*  Fixed the ratchet_payoff class (#1).
*  Added va_sde_engine3 to price under the assumption the interest rate is constant, the intersity of mortality is given by the Weibull function  and the underlying fund is specified by a system of SDEs.



