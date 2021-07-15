## Submission

This is a submission for a new patch release (0.0.8).

The 0.0.8 release fixes a bug where the functions stopped because of an erroneously 
triggered error message.

The resubmission is necessary because a test failed in a 
r-devel-linux-x86_64-fedora-gcc environment. This test is now skipped because 
the error is not replicable on a local fedora-gcc-devel container.


## Test environments

* Windows Server 2008 R2 SP1, R-devel, 32/64 bit (R-hub)
* Windows x86_64-w64-mingw32 (64-bit), R-devel (win-builder)
* windows latest, release (github actions)
* Ubuntu 20.04, release (github actions)
* Ubuntu 20.04, devel (github actions)
* macOS latest, release (github actions)


## R CMD check results
There were no ERRORs or WARNINGs.

There was one NOTE:

Maintainer: 'Manuel Neumann <manuel.neumann@mzes.uni-mannheim.de>'

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.2307/2669316
    From: inst/doc/OVA_Predictions_For_MNL.html
          README.md
    Status: 403
    Message: Forbidden
  URL: https://doi.org/10.4232/1.13213
    From: inst/doc/OVA_Predictions_For_MNL.html
          README.md
    Status: Error
    Message: SSL certificate problem: unable to get local issuer certificate
  URL: https://www.doi.org/10.4232/1.13213
    From: man/gles.Rd
    Status: Error
    Message: SSL certificate problem: unable to get local issuer certificate

Found the following (possibly) invalid DOIs:
  DOI: 10.2307/2669316
    From: DESCRIPTION
    Status: Forbidden
    Message: 403
    
* All DOIs and URLs were manually checked and lead to the correct websites.


## Downstram dependencies
There are currently no downstream dependencies for this package.
