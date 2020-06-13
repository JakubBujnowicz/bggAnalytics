# Fetches ######################################################################
fetch_name <- function(xml)
    nodes2text(xml, "name")

fetch_image <- function(xml)
    nodes2text(xml, "image")

fetch_thumbnail <- function(xml)
    nodes2text(xml, "thumbnail")

fetch_yearpublished <- function(xml)
    nodes2number(xml, "yearpublished")

fetch_numplays <- function(xml)
    nodes2number(xml, "numplays")

# Status
fetch_own <- function(xml)
    attr2number(xml, "status", "own")

fetch_prevowned <- function(xml)
    attr2number(xml, "status", "prevowned")

fetch_fortrade <- function(xml)
    attr2number(xml, "status", "fortrade")

fetch_want <- function(xml)
    attr2number(xml, "status", "want")

fetch_wanttoplay <- function(xml)
    attr2number(xml, "status", "wanttoplay")

fetch_wanttobuy <- function(xml)
    attr2number(xml, "status", "wanttobuy")

fetch_wishlist <- function(xml)
    attr2number(xml, "status", "wishlist")

fetch_preordered <- function(xml)
    attr2number(xml, "status", "preordered")


# Requires stats to be included in XML -----------------------------------------
fetch_minplayers <- function(xml)
    attr2number(xml, "stats", "minplayers")

fetch_maxplayers <- function(xml)
    attr2number(xml, "stats", "maxplayers")

fetch_minplaytime <- function(xml)
    attr2number(xml, "stats", "minplaytime")

fetch_maxplaytime <- function(xml)
    attr2number(xml, "stats", "maxplaytime")

fetch_playingtime <- function(xml)
    attr2number(xml, "stats", "playingtime")

fetch_numowned <- function(xml)
    attr2number(xml, "stats", "numowned")

# Expands ######################################################################
