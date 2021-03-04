## Submission

This is a submission for a new minor release (0.0.6).

This release fixes a bug where the functions crash when only one IV is supplied.


## Test environments

* Windows Server 2008 R2 SP1, R-devel, 32/64 bit (on rhub)
* windows latest, release (github actions)
* Ubuntu 20.04, release (github actions)
* Ubuntu 20.04, devel (github actions)
* macOS latest, release (github actions)


## R CMD check results
There were no ERRORs or WARNINGs

There was one NOTE:

Maintainer: 'Manuel Neumann <manuel.neumann@mzes.uni-mannheim.de>'

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.2307/2669316
    From: inst/doc/OVA_Predictions_For_MNL.html
          README.md
    Status: 403
    Message: Forbidden

Found the following (possibly) invalid DOIs:
  DOI: 10.2307/2669316
    From: DESCRIPTION
    Status: Forbidden
    Message: 403
    
Both the URL and the DOI are valid and lead to the correct location.


## Downstram dependencies
There are currently no downstream dependencies for this package.
