% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/example-scout.R
\name{ovdata_example}
\alias{ovdata_example}
\title{Example volleyball match scout files}
\usage{
ovdata_example(choice = "190301_kats_beds", as = "path", simplify = TRUE)
}
\arguments{
\item{choice}{string: one or more files to return
\itemize{
\item "190301_kats_beds" - the full DataVolley file from a match between GKS Katowice and MKS Bedzin during the 2018/19 Polish Plus Liga (file courtesy Mark Lebedew)
\item "190301_kats_beds-clip" - a single rally from the "190301_kats_beds" DataVolley file. This rally corresponds to the video clip contained in \code{ovdata_example_video("190301_kats_beds")}
\item "PM06" - (DataVolley) the December 2012 men's Slovenian national championship semifinal between ACH Volley and Maribor (file from \url{http://www.odbojka.si/})
\item "mlafin_braslovce_nkbm" - (DataVolley) the January 2015 Slovenian junior women's final between Braslovče and Nova KBM Branik (file from \url{http://www.odbojka.si/})
\item "clickscout" - an example Click & Scout DataVolley file
\item "2017_AVL_mens_HEAT_vs_UTSSU" - (Perana/VBStats) Men's Australian Volleyball League 2017: Canberra Heat vs UTSSU (file courtesy Chau Le)
\item "2017_AVL_womens_HEAT_vs_UTSSU" - (Perana/VBStats) Women's Australian Volleyball League 2017: Canberra Heat vs UTSSU (file courtesy Chau Le)
\item "stuttgart_schwerin_2018" - (DataVolley) the 2018 women's final (first of 3) from the German Bundesliga, Allianz MTV Stuttgart vs SSC Palmberg Schwerin (file courtesy Michael Mattes)
\item "DCup7" - the 2020 Austrian Women's Volley Cup played between Hartberg and UVC Graz (file from \url{https://www.volleynet.at/dvdownload/information/f-Damen/})
\item "NCA-CUB" - Nicaragua vs Cuba women from the Pan American Cup, August 2022 (vsm format, courtesy Christophe Elek)
}}

\item{as}{string: either "path" (return the path to the file) or "parsed" (parse the file into an R data structure using \code{\link[datavolley:dv_read]{datavolley::dv_read()}} or \code{\link[peranavolley:pv_read]{peranavolley::pv_read()}}}

\item{simplify}{logical: by default if \code{as} is "parsed", the returned list is the same length as \code{choice} where each entry is a parsed object. If \code{simplify} is \code{TRUE} and we have asked for a single file (i.e. \code{length(choice) == 1}, then return just that object (not a list containing that object)}
}
\value{
If \code{as} is "path", a character vector with the file path(s), otherwise a list
}
\description{
Example volleyball match scout files
}
\examples{
myfile <- ovdata_example()
x <- dv_read(myfile)
summary(x)

x <- ovdata_example("clickscout", as = "parsed")
summary(x)

}
\seealso{
\code{\link[=ovdata_example_video]{ovdata_example_video()}}
}
