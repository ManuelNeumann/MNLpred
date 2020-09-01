## Resubmission

This is a submission for a new minor release (0.0.4).

It includes a very important error message for people that want to use models
with factor variables. 
It refers to a related issue on github that presents a workaround.

"http"" was changed to "https" in link


## Test environments

* Windows Server 2008 R2 SP1, R-devel, 32/64 bit (on rhub)
* Ubuntu Linux 16.04 LTS, R-release, GCC (on rhub)
* Fedora Linux, R-devel, clang, gfortran (on rhub)
* macOS 10.13, R-release (with travis-ci)


## R CMD check results
There were no ERRORs or WARNINGs

There are two NOTEs:

Maintainer: ‘Manuel Neumann <manuel.neumann@mzes.uni-mannheim.de>’

Found the following (possibly) invalid URLs:
  URL: https://www.doi.org/10.4232/1.13213
    From: man/gles.Rd
    Status: Error
    Message: libcurl error code 60:
       SSL certificate problem: unable to get local issuer certificate
       (Status without verification: OK)

* URL links to the correct website by Gesis

Found the following (possibly) invalid DOIs:
  DOI: 10.2307/2669316
    From: DESCRIPTION
    Status: Forbidden
    Message: 403

* DOI is correct, but article is behind a pay-wall.

* checking for future file timestamps ... NOTE
unable to verify current time

* worldclockapi.com seems to be down


## Downstram dependencies
There are currently no downstream dependencies for this package.
