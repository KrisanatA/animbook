#'Osiris firm sales data
#'
#'The Osiris dataset contains information on listed, and major unlisted/delisted
#'companies across the world from 2006 to 2018. This dataset only includes the year, ID,
#'country, sales, and japan variables from the full Osiris dataset to give the user
#'an example of the dataset format.
#'
#'@format A data frame with 10,270 rows and 5 variables
#'\describe{
#'  \item{year}{Year}
#'  \item{ID}{BvD(Bureau van Dijk) ID}
#'  \item{country}{Address of incorp. - Country}
#'  \item{sales}{Sales}
#'  \item{japan}{Whether the firm is froom Japan or not}
#'}
#'
#'@source This dataset is from the following; Bureau van Dijk
#'\url{https://www.bvdinfo.com/en-gb/our-products/data/international/osiris}.
"osiris"



#'Australian election study data
#'
#'The aes dataset contains the answer of the surveys which is done in 2019 for the election.
#'This dataset only includes the id, year, party, gender from the the full survey data. The year
#'column come from the transformations of two different question to see whether the voter voted the
#'same party in 2016 and 2019 or not and if not who did they voted for before.
#'
#'@format A data frame with 1,468 rows and 4 variables
#'\describe{
#'  \item{id}{The id of the respondent}
#'  \item{year}{Year}
#'  \item{party}{Party that the respondent vote for in the House of Representatives}
#'  \item{gender}{Gender of the respondent}
#'}
#'
#'@source This dataset is from the following; Australian Election Study
#'\url{https://dataverse.ada.edu.au/file.xhtml?fileId=18013&version=3.0}
"aeles"
