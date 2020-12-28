#' Example volleyball match scout files
#'
#' @param choice string: one or more files to return
#' - "190301_kats_beds" - a DataVolley file from a match between GKS Katowice and MKS Bedzin during the 2018/19 Polish Plus Liga (file courtesy Mark Lebedew)
#' - "PM06" - (DataVolley) the December 2012 men's Slovenian national championship semifinal between ACH Volley and Maribor (file from <http://www.odbojka.si/>)
#' - "mlafin_braslovce_nkbm" - (DataVolley) the January 2015 Slovenian junior women's final between Braslovče and Nova KBM Branik (file from <http://www.odbojka.si/>)
#' - "clickscout" - an example Click & Scout DataVolley file
#' - "2017_AVL_mens_HEAT_vs_UTSSU" - (Perana/VBStats) Men's Australian Volleyball League 2017: Canberra Heat vs UTSSU (file courtesy Chau Le)
#' - "2017_AVL_womens_HEAT_vs_UTSSU" - (Perana/VBStats) Women's Australian Volleyball League 2017: Canberra Heat vs UTSSU (file courtesy Chau Le)
#' - "stuttgart_schwerin_2018" - (DataVolley) the 2018 women's final (first of 3) from the German Bundesliga, Allianz MTV Stuttgart vs SSC Palmberg Schwerin (file courtesy Michael Mattes)
#'
#' @param as string: either "path" (return the path to the file) or "parsed" (parse the file into an R data structure using [datavolley::dv_read()] or [peranavolley::pv_read()]
#' @param simplify logical: by default if `as` is "parsed", the returned list is the same length as `choice` where each entry is a parsed object. If `simplify` is `TRUE` and we have asked for a single file (i.e. `length(choice) == 1`, then return just that objecty (not a list containing that object)
#' @return If `as` is "path", a character vector with the file path(s), otherwise a list
#'
#' @examples
#' myfile <- ovdata_example()
#' x <- dv_read(myfile)
#' summary(x)
#'
#' x <- ovdata_example("clickscout", as = "parsed")
#' summary(x)
#'
#' @export
ovdata_example <- function(choice = "190301_kats_beds", as = "path", simplify = TRUE) {
    assert_that(is.character(choice))
    assert_that(is.string(as))
    assert_that(is.flag(simplify), !is.na(simplify))
    as <- tolower(as)
    as <- match.arg(as, c("path", "parsed"))
    out <- rep(NA_character_, length(choice))
    parms <- rep(list(NULL), length(choice))
    for (i in seq_along(choice)) {
        switch(tolower(choice[i]),
               "190301_kats_beds" = {
                   out[i] <- system.file("extdata/scout/&190301_kats_beds.dvw", package = "ovdata")
               },
               "mlafin_braslovce_nkbm" = {
                   out[i] <- system.file("extdata/scout/mlafin_braslovce_nkbm.dvw", package = "ovdata")
               },
               "pm06" = {
                   out[i] <- system.file("extdata/scout/PM06.dvw", package = "ovdata")
               },
               "clickscout" = {
                   out[i] <- system.file("extdata/scout/&example-click-scout.dvw", package = "ovdata")
               },
               "2017_avl_mens_heat_vs_utssu" = {
                   out[i] <- system.file("extdata/scout/20170923_2017_AVL_MENS_CHM_17_vs_UTSSUM_17.psvb", package = "ovdata")
               },
               "2017_avl_womens_heat_vs_utssu" = {
                   out[i] <- system.file("extdata/scout/20170923_2017_AVL_WOMENS_CHW_17_vs_UTSSUW_17.psvb", package = "ovdata")
               },
               "stuttgart_schwerin_2018" = {
                   parms[[i]] <- list(skill_evaluation_decode = "german")
                   out[i] <- system.file("extdata/scout/&stuttgart-schwerin-2018.dvw", package = "ovdata")
               },
               stop("unrecognized 'choice' value (", choice[i], ")")
               )
        if (!nzchar(out[i]) || is.na(out[i])) stop("could not find file ", choice[i])
    }
    if (as == "parsed") {
        out <- lapply(seq_along(out), function(i) {
            if (grepl("\\.psvb$", tolower(out[i]))) {
                ## pv
                do.call(pv_read, c(list(out[i]), parms[[i]]))
            } else {
                ## dv
                this_parms <- list(skill_evaluation_decode = "guess") ## defaults
                for (p in names(parms[[i]])) this_parms[[p]] <- parms[[i]][[p]]
                do.call(dv_read, c(list(out[i]), this_parms))
            }
        })
        if (simplify && length(out) == 1) out <- out[[1]]
    }
    out
 }
