# pollplayers ##################################################################
.fetch_pollplayers <- function(xml)
{
    polls <- html_node(xml, xpath = "poll[@name = 'suggested_numplayers']")
    polls <- lapply(polls, html_nodes, xpath = "results")

    numplayers <- lapply(polls, .attr2text, xpath = ".",
                         attr = "numplayers", scalar = TRUE)
    best <- lapply(polls, .attr2number,
                   xpath = "result[@value = 'Best']",
                   attr = "numvotes", scalar = TRUE)
    recommended <- lapply(polls, .attr2number,
                          xpath = "result[@value = 'Recommended']",
                          attr = "numvotes", scalar = TRUE)
    not_recc <- lapply(polls, .attr2number,
                       xpath = "result[@value = 'Not Recommended']",
                       attr = "numvotes", scalar = TRUE)

    result <- list()
    for (i in seq_along(polls)) {
        result[[i]] <- data.table(numplayers = numplayers[[i]],
                                  best = best[[i]],
                                  recommended = recommended[[i]],
                                  notrecommended = not_recc[[i]])
    }

    return (result)
}
