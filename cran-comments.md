## Resubmission

This is a new release. It does not include the tibble() function anymore since it may cause trouble with the new tibble release (see https://github.com/tidyverse/tibble/issues/689).

## Test environments

* Windows Server 2008 R2 SP1, R-devel, 32/64 bit (on rhub)
* OS Catalina 10.15, R 3.6.3
* Ubuntu Linux 16.04 LTS, R-release, GCC (on rhub)
* Fedora Linux, R-devel, clang, gfortran (on rhub)


## R CMD check results
There were no ERRORs or WARNINGs

There is one NOTE:

  Maintainer: 'Manuel Neumann <manuel.neumann@mzes.uni-mannheim.de>'
  
  Found the following (possibly) invalid DOIs:
    DOI: 10.2307/2669316
      From: DESCRIPTION
      Status: Forbidden
      Message: 403

* DOI is correct, but article is behind a pay-wall.



## Downstram dependencies
There are currently no downstream dependencies for this package.
