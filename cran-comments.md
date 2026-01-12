## This is a new submission

We received a mail by Kurt Hornik describing the following problem:

You have in plot_iota2_alluvial.Rd

  An example for interpreting the plot can be found in the vignette
  \href{../iotarelr.html}{Get started} or via

but that points nowhere in HTML help.  

I guess you want

  \href{../doc/iotarelr.html}{Get started}

instead?


Please correct before 2025-08-25 to safely retain your package on CRAN.

We fixed the problem, updated the documentation, and updated some other aspects.

## Test environments
* local Windows 11 install, R 4.4.2
* R-CMD-Checks on 
  - {os: macos-latest,   r: 'release'}
  - {os: windows-latest, r: 'release'}
  - {os: ubuntu-latest,   r: 'devel', http-user-agent: 'release'}
  - {os: ubuntu-latest,   r: 'release'}
  - {os: ubuntu-latest,   r: 'oldrel-1'}

## R CMD Check results
There were no ERRORs or WARNINGs or NOTEs.
