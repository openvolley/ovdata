#' Example volleyball team roster files
#'
#' @param choice string: one or more files to return
#' - "Stadium Mirandola" - the team roster for Stadium Mirandola (file from <http://volleynetworkitalia.it/>)
#' @param as string: either "path" (return the path to the file) or "parsed" (parse the file into an R data structure using [datavolley::dv_read_sq()]
#' @param simplify logical: by default if `as` is "parsed", the returned list is the same length as `choice` where each entry is a parsed object. If `simplify` is `TRUE` and we have asked for a single file (i.e. `length(choice) == 1`, then return just that object (not a list containing that object)
#' @return If `as` is "path", a character vector with the file path(s), otherwise a list
#'
#' @seealso [ovdata_example()]
#'
#' @examples
#' myfile <- ovdata_example_roster()
#' ## read this file with
#' ## x <- datavolley::dv_read_sq(myfile)
#'
#' ## or have it read automatically
#' x <- ovdata_example_roster(as = "parsed")
#'
#' @export
ovdata_example_roster <- function(choice = "Stadium Mirandola", as = "path", simplify = TRUE) {
    assert_that(is.character(choice))
    assert_that(is.string(as))
    assert_that(is.flag(simplify), !is.na(simplify))
    as <- tolower(as)
    as <- match.arg(as, c("path", "parsed"))
    out <- rep(NA_character_, length(choice))
    parms <- rep(list(NULL), length(choice))
    for (i in seq_along(choice)) {
        switch(tolower(choice[i]),
               "stadium mirandola" = {
                   parms[[i]] <- list(encoding = "windows-1250")
                   out[i] <- system.file("extdata/roster/99.sq", package = "ovdata")
               },
               stop("unrecognized 'choice' value (", choice[i], ")")
               )
        if (!nzchar(out[i]) || is.na(out[i])) stop("could not find file ", choice[i])
    }
    if (as == "parsed") {
        this_parms <- list(encoding = "guess") ## defaults
        for (p in names(parms[[i]])) this_parms[[p]] <- parms[[i]][[p]]
        out <- lapply(seq_along(out), function(i) {
            do.call(dv_read_sq, c(list(out[i]), this_parms))
        })
        if (simplify && length(out) == 1) out <- out[[1]]
    }
    out
 }
