#' Osiris firm sales data
#'
#' The Osiris dataset contains information on listed and major unlisted/delisted
#' companies across the world from 2006 to 2018. This dataset only includes the year, ID,
#' country, sales, and japan variables from the full Osiris dataset to give the user
#' an example of the dataset format.
#'
#' @format A data frame with 10,270 rows and 5 variables
#' \describe{
#'   \item{year}{Year}
#'   \item{ID}{BvD(Bureau van Dijk) ID}
#'   \item{country}{Address of incorp. - Country}
#'   \item{sales}{Sales}
#'   \item{japan}{Whether the firm is from Japan or not}
#' }
#'
#' @source This dataset is from the following; Bureau van Dijk
#' \url{https://www.bvdinfo.com/en-gb/our-products/data/international/osiris}.
"osiris"



#' Australian election study data
#'
#' The aes dataset contains the answers to the surveys that were done in 2019 for the election.
#' This dataset only includes the id, year, party, and gender from the full survey data. The year
#' column comes from the transformations of two different questions to see whether the voter voted for the
#' same party in 2016 and 2019 or not, and if not, who did they vote for before?
#'
#' @format A data frame with 1,468 rows and 4 variables
#' \describe{
#'   \item{id}{The id of the respondent}
#'   \item{year}{Year}
#'   \item{party}{Party that the respondent votes for in the House of Representatives}
#'   \item{gender}{Gender of the respondent}
#' }
#'
#' @source This dataset is from the following; Australian Election Study
#' \url{https://dataverse.ada.edu.au/file.xhtml?fileId=18013&version=3.0}
"aeles"



#' Simulated data with some change (category)
#'
#' This data has changed from category A to E between two-time points.
#'
#' @format A data frame with 400 rows and 4 variables
#' \describe{
#'   \item{id}{The id of the organisation}
#'   \item{time}{time}
#'   \item{gp}{Either X or Y}
#'   \item{qnt}{Quantile group for the two times}
#' }
#'
#' @examples
#' d <- anim_prep_cat(cat_change, id = id, values = qnt,
#' time = time, group = gp)
#'
#' d_p <- wallaby_plot(d, height = 1)
#'
#' d_p_anim <- anim_animate(d_p)
"cat_change"



#' Simulated data with some change (numerical)
#'
#' This data contained the numerical values for each observation.
#'
#' @format A data frame with 400 rows and 4 variables
#' \describe{
#'   \item{id}{The id of the organisation}
#'   \item{time}{time}
#'   \item{gp}{Either X or Y}
#'   \item{values}{Numerical values represent sales}
#' }
#'
#' @examples
#' d <- anim_prep(dbl_change, id = id, values = values,
#' time = time, group = gp)
#'
#' d_p <- wallaby_plot(d, height = 1)
#'
#' d_p_anim <- anim_animate(d_p)
"dbl_change"
