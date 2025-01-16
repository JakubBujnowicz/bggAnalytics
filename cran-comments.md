Updated the documentation with the latest roxygen2 CRAN build.


## R CMD check results

0 errors | 0 warnings | 0 notes


## R-hub check results

Two NOTEs during r-hub check:

* **OK** -- Ubuntu Linux 20.04.1 LTS, R-release, GCC,
    [full log.](https://builder.r-hub.io/status/bggAnalytics_0.2.1.tar.gz-7fb12e4d67f048f8acf48f217a1c03b8)
* **NOTE** -- Windows Server 2022, R-devel, 64 bit
    ```
    * checking for detritus in the temp directory ... NOTE
    Found the following files/directories:
    'lastMiKTeXException'
    ```
    Supposedly it [can be ignored](https://github.com/r-hub/rhub/issues/503).
    [Full log.](https://builder.r-hub.io/status/bggAnalytics_0.2.1.tar.gz-5d68324639d8436ab89392196c2da7f8)
* **NOTE** -- Fedora Linux, R-devel, clang, gfortran
    ```
    * checking HTML version of manual ... NOTE
    Skipping checking HTML validation: no command 'tidy' found
    ```
    Not really sure how to fix it and what is the source of the problem.
    [Full log.](https://builder.r-hub.io/status/bggAnalytics_0.2.1.tar.gz-452b2fdf7f604842a9d41a978654c3c4)


## winbuilder check results

OK, [full log.](https://win-builder.r-project.org/410r4Qm6y8z6/00check.log)
