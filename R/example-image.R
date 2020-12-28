#' Example volleyball images
#'
#' @param choice string: one or more files to return
#' - "190301_kats_beds" - an image from a match between GKS Katowice and MKS Bedzin during the 2018/19 Polish Plus Liga
#' @return A character vector with the file path(s)
#'
#' @examples
#' myfile <- ovdata_example_image()
#'
#' @export
ovdata_example_image <- function(choice = "190301_kats_beds") {
    assert_that(is.character(choice))
    out <- rep(NA_character_, length(choice))
    for (i in seq_along(choice)) {
        switch(tolower(choice[i]),
               "190301_kats_beds" = {
                   out[i] <- system.file("extdata/images/2019_03_01-KATS-BEDS-frame.png", package = "ovdata")
               },
               stop("unrecognized 'choice' value (", choice[i], ")")
               )
        if (!nzchar(out[i]) || is.na(out[i])) stop("could not find file ", choice[i])
    }
    out
}
