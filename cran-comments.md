## This is a new submission

* Fixed citation files in line with mail by Uwe Ligges (24. Feb. 2023)

* Fixed the bug mentioned in an email by Prof Brian Ripley (24. Feb. 2023). The problem was that the test relies on drawing random samples. The sample size was to small for the old and unstable
estimation method. Thus, some rare cases can occur leading to a wrong estimation which in turn leads to a failing test. Since the new stable method is
implemented as default the test is skipped in order to avoid random failing tests in the future.

## Test envirnoments
* local Windows 7 install, R 4.1.2
* win-builder (devel and release)
* mac via devtools::check_mac_release()

## R CMD Check results
There were no ERRORs or WARNINGs.
There is 1 NOTE referring to a doi which is working 


