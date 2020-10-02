# bggAnalytics
R package for simple downloading and organisation of board game data using BoardGameGeek's [XML API2](https://boardgamegeek.com/wiki/page/BGG_XML_API2). Users of R programming language should have no problems extracting data about their BGG games collection, or in fact, any games from BGG. This should allow for automatisation of the collection exporting task, which has to be done manually on the BGG website.

## Installation
The package will be uploaded to CRAN in the future. In the meanwhile, it can be installed using `devtools` package.
```r
library(devtools)
install_github("JakubBujnowicz/bggAnalytics")
```

## How To Use?
The package uses [R6](https://r6.r-lib.org/articles/Introduction.html) classes and `data.table` package as a backbone for data. Hence usage is heavily leaning on reference, which is quite uncommon for R programming language.

Easiest way to start is to create a bggCollection object for a certain user. For showcase purposes, I'm using my BGG account called `Beo_`.
```r
library(bggAnalytics)
my_collection <- bggCollection$new(username = "Beo_")
my_collection
```

Another step would be to fetch some data - everything (from the set of currently supported variables) can be fetched using `fetch` method.
```r
my_collection$fetch()
# Or
my_collection$fetch(c("name", "yearpublished"))
```

Method called `extend` can be used in the same manner to extend object's `data` slot by given variables.
```r
my_collection$data
# Currently only objectid
my_collection$extend(c("name", "yearpublished"))
my_collection$data
```
Note that this extension is done by reference, hence no assignment is necessary.

One doesn't have to rely on a given user's collection to download game's data. You can find a game ID by looking at it's BGG hyperlink, e.g. https://boardgamegeek.com/boardgame/167791/terraforming-mars is a link for *Terraforming Mars*, hence 167791 is it's ID.
```r
games <- bggGames$new(ids = 167791)
games$extend()
games$data
```
Naturally, multiple IDs may be supplied to `bggGames` object. One can always extract IDs from a collection by `my_collection$ids`.

## Roadmap
#### Nearest future
1. Provide the documentation for all classes and exported functions.
2. Provide a method for users to easily check available variables and parameters of every class (function/data?).
3. Supply a viewing-friendly version of fetched data along with interactive app in Shiny - [prototype here](https://bujnowiczapps.shinyapps.io/boardgamecollection/).

#### Long-term plans
1. Add support for other structures from BGG XML API.
2. Upload the package to CRAN.
