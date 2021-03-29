# occupations

human_occupations <- readr::read_tsv("occupations/human_occupations.tsv",
                                     col_names = F)
class(human_occupations) <- "data.frame"
usethis::use_data(human_occupations, overwrite = T)

vogon_occupations <- readr:::read_tsv("occupations/vogon_occupations.tsv",
                                      col_names = F)
class(vogon_occupations) <- "data.frame"
usethis::use_data(vogon_occupations, overwrite = T)

dentrassi_occupations <- readr:::read_tsv("occupations/dentrassi_occupations.tsv",
                                          col_names = F)
class(dentrassi_occupations) <- "data.frame"
usethis::use_data(dentrassi_occupations, overwrite = T)

occupations <- function(n, race){
  occupation <- hitchr::dentrassi_occupations[sample(1:nrow(hitchr::dentrassi_occupations),
                                                     size = n, replace = T), ]
}
