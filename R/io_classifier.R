#' Sort Catch Numbers Into Species Categories
#'
#' This function uses itis_reclassify(), developed by Emily Markowitz, to create species category classifications and then sorts catch numbers into those classifications.
#'
#' @param data A data frame that includes catch numbers in dollars and TSN.
#' @param list A list of lists that includes categories at the top level and TSN numbers at the second level. Defaults to Comm.Catch.Spp.List.
#' @param year A numeric variable that can be used to filter for a specific year. Defaults to NA, which returns all years.
#' @param recal A binary variable that determines whether the API should be queried or a default value used. Defaults to TRUE
#' @param tsn A dataset that can be used in place of the API call. Defaults to NULL
#' @importFrom magrittr %>%
#' @export
io_classifier <- function(data, species = Comm.Catch.Spp.List, year = NA, recall = T, tsn = NULL){

  commercial_data = data %>%
    dplyr::mutate(State = as.character(State), abbvst = as.character(abbvst), abbvreg = as.character(abbvreg))
  species_list = species

  if(recall == T){
    temp <- unique(commercial_data$TSN) %>% FishEconProdOutput::itis_reclassify(tsn = .,
                                                                                categories = species_list)
    tsn_id = as.data.frame(temp[1][[1]])

    temp2 <- unique(commercial_data$TSN) %>% FishEconProdOutput::itis_reclassify(tsn = .,
                                                                                 categories = list("Finfish" = c(914179, #  Infraphylum  Gnathostomata
                                                                                                                 -914181))) # Tetrapoda; - = do NOT include
    tsn_id2 = as.data.frame(temp2[1][[1]]) %>% dplyr::rename(category2 = category) %>% dplyr::select(-valid, -rank, -sciname)

    tsn_id = tsn_id %>% full_join(tsn_id2)

  } else {

    tsn_id = tsn

  }

  tsn_id = tsn_id %>%
    #dplyr::filter(category != "Other" & category != "Uncategorized"  & category2 != "Other" & category2 != "Uncategorized") %>%
    dplyr::select(TSN, category, category2)

  tsn_id$TSN<-as.numeric(as.character(tsn_id$TSN))


  if(length(commercial_data$abbvst[commercial_data$abbvst=="WFL"])>0){
    commercial_data$State[commercial_data$abbvst=="EFL"]<-"Florida"
    commercial_data$Region[commercial_data$abbvst=="EFL"]<-"Gulf of Mexico"
    commercial_data$State[commercial_data$abbvst=="WFL"]<-"Florida"
    commercial_data$abbvst[commercial_data$abbvst=="WFL"]<-"FL"
    commercial_data$abbvst[commercial_data$abbvst=="EFL"]<-"FL"
  }

  commercial.data.merged<-dplyr::left_join(x = commercial_data,
                                           y = tsn_id,
                                           by = "TSN")


  commercial.data.out = commercial.data.merged %>%
    dplyr::mutate(category = case_when(
      is.na(category) ~ category2,
      category == "Other" & category2 == "Finfish" ~ category2,
      category == "Uncategorized" & category2 == "Finfish" ~ category2,
      TRUE ~ category
    )) %>%
    dplyr::select(-category2) %>%
    dplyr::filter(DOLLARS>0 & !is.na(DOLLARS) & !is.na(category)) %>%
    dplyr::select(State, year, DOLLARS, fips, Region, category) %>%
    dplyr::group_by(Region, State, fips, year, category) %>%
    dplyr::summarize(dollars = sum(DOLLARS))

  if(!is.na(year)){
    commercial.data.out = commercial.data.out %>%
      dplyr::rename(`Species Category` = category, base_catch = dollars, Year = year) %>%
      dplyr::filter(Year == year)
  }
  if(is.na(year)){
    commercial.data.out = commercial.data.out %>%
      dplyr::rename(`Species Category` = category, base_catch = dollars, Year = year)
  }

  commercial.data.out$`Species Category`[commercial.data.out$`Species Category`=="Finfish"]<-"All Other Finfish"

  return(commercial.data.out)

}
