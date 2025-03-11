# bggAnalytics (development version)

## Fixed issues
* Adjusted `bggGames` fetching to the latest changes in bggAPI (maximum of 20
entries per call), done by limiting `chunk_size` to 20.
* Package options are now set by default on package loading.

## Other changes
* Updated documentation.
* R6 and data.table are now longer depended on, now they are imported


# bggAnalytics 0.2.0 (2021-09-23)

* Slight optimization of variable fetching.
* Added `ranks` and `families` and  variables to `bggGames` and `bggCollection` classes.
* Added a `lastmodified` variable to the `bggCollection` class.
* Improved the consistency of the `unsqueeze()` function output.
* Fixed bugged calls to internal package functions.
* Fixed a bug which caused `fetch()` variables to have improper names.


# bggAnalytics 0.1.0 (2020-10-07)

* First working version of the package.

