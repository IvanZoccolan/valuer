## Test environments
* local Ubuntu 16.04.2 LTS install, R  3.4.3
* Ubuntu 14.04.5 LTS (on travis-ci), R 3.4.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or  WARNINGs

win-builder devel reported a NOTE

Possibly mis-spelled words in DESCRIPTION:
  MacKay (17:18)
  
However MacKay is spelled correctly.  


## Resubmission
* Fixed two out of three NOTEs in previuos submission 
<https://win-builder.r-project.org/incoming_pretest/180108_175637_valuer_112/00check.log
The remaining appears to be a false positive with the spell checker.

## Release notes
In this release, I have:

* Added the function va_pde_pricer to price a VA by means of PDE methods.
* Fixed NOTES in Current CRAN status: NOTE: 4, OK: 5
See: <https://CRAN.R-project.org/web/checks/check_results_valuer.html>

## Downstream dependencies
Currently there are no downstream dependencies of valuer.
