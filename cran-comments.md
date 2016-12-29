## Test environments
* local Ubuntu 12.04 LTS install, R  3.2.5
* Ubuntu precise (12.04.5 LTS) (on travis-ci), R 3.3.1
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.


## Release notes
In this release, I have:

* Fixed an heap overflow in calc_account.cpp. Tests and examples pass with:
* R devel with ASAN / UBSAN sanitizers (gcc 6.2.1, R 2016-12-11 r71774 )   (rocker-org)
* R devel with ASAN / UBSAN sanitizers using clang/clang++(clang version 3.8.1-16, R 2016-12-19 r71819)  (rocker-org)
* Note: 'AddressSanitizer: undefined-behavior' is raised by clang on Rcpp own code. 

## Downstream dependencies
Currently there are no downstream dependencies of valuer.
