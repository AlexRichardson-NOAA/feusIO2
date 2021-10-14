#' Clean the Output From the IO Model
#'
#' This function presents the output from io_calculator() in a variety of specifiable formats.
#'
#' @param impact A data frame that was output by io_calculator().
#' @param format A string variable with several options for specifying output format.
#' @param xlsx A boolean variable that exports the output as tabs in an xlsx file if True.
#' @param fp A data frame containing state names and fips codes. Defaults to standard with EFL = 12 and WFL = 12.5.
#' @param maxyr The catch year - only used for FEUS.
#' @importFrom magrittr %>%
#' @export
io_cleaner <- function(impact, format = "national", xlsx = F, fp = fips, maxyr = 2018) {
  output = c()
  fips = fp

  impacts = impact %>%
    tidyr::pivot_longer(PI_Direct_Impact:E_Total,
                        names_to = "names",
                        values_to = "values") %>%
    dplyr::mutate(
      Impact_Type = dplyr::case_when(
        stringr::str_detect(names, "PI_") ~ "Income Impacts",
        stringr::str_detect(names, "TV_") ~ "Total Value Added",
        stringr::str_detect(names, "O_") ~ "Output Impacts",
        stringr::str_detect(names, "E_") ~ "Employment Impacts"
      )
    ) %>%
    dplyr::mutate(
      Group = dplyr::case_when(
        stringr::str_detect(names, "Direct_Impact") ~ "Direct",
        stringr::str_detect(names, "Indirect_Impact") ~ "Indirect",
        stringr::str_detect(names, "Induced_Impact") ~ "Induced",
        stringr::str_detect(names, "Total") ~ "Total"
      )
    ) %>%
    dplyr::select(-names) %>%
    tidyr::pivot_wider(
      id_cols = c(
        fips,
        `Economic Category`,
        `Species Category`,
        spec_no,
        Impact_Type,
        Group,
        Imports
      ),
      names_from = Group,
      values_from = values
    ) %>%
    dplyr::mutate(`Economic Category` = factor(
      `Economic Category`,
      levels = c(
        "Harvesters",
        "Processors",
        "Wholesalers",
        "Grocers",
        "Restaurants",
        "Brokers/importers"
      )
    )) %>%
    dplyr::arrange(fips, `Economic Category`, Impact_Type, spec_no)

  if (format == "national" | format == "all") {
    impacts_national = impacts %>% dplyr::filter(fips == 0)

    impacts_national_imports = impacts_national %>%
      dplyr::filter(spec_no == 0 & `Economic Category` == "Brokers/importers") %>%
      dplyr::arrange(`Economic Category`, Impact_Type) %>%
      dplyr::mutate(`Economic Category` = "Imports and Brokers")

    impacts_national = impacts_national %>%
      dplyr::filter(`Economic Category` != "Brokers/importers") %>%
      dplyr::group_by(`Economic Category`, Impact_Type) %>%
      dplyr::summarize(
        Direct = sum(Direct),
        Indirect = sum(Indirect),
        Induced = sum(Induced),
        Total = sum(Total)
      ) %>%
      dplyr::arrange(`Economic Category`, Impact_Type)

    impacts_national_seafood = impacts_national %>%
      dplyr::bind_rows(impacts_national_imports) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(Impact_Type) %>%
      dplyr::summarize(
        Direct = sum(Direct),
        Indirect = sum(Indirect),
        Induced = sum(Induced),
        Total = sum(Total)
      ) %>%
      dplyr::arrange(Impact_Type) %>%
      dplyr::mutate(`Economic Category` = "Harvesters and Seafood Industry")


    impacts_national_out = impacts_national %>%
      dplyr::bind_rows(impacts_national_imports) %>%
      dplyr::bind_rows(impacts_national_seafood)

    output = append(output, list("national_impacts" = impacts_national_out))

  }

  if (format == "state summary" | format == "all") {
    impacts_state_sum = impacts %>% dplyr::filter(fips != 0)

    impacts_state_sum_imports = impacts_state_sum %>%
      dplyr::filter(`Economic Category` == "Brokers/importers") %>%
      dplyr::arrange(`Economic Category`, Impact_Type) %>%
      dplyr::mutate(`Economic Category` = "Imports and Brokers") %>%

      impacts_state_sum = impacts_state_sum %>%
      dplyr::filter(`Economic Category` != "Brokers/importers") %>%
      dplyr::group_by(`Economic Category`, Impact_Type) %>%
      dplyr::summarize(
        Direct = sum(Direct),
        Indirect = sum(Indirect),
        Induced = sum(Induced),
        Total = sum(Total)
      ) %>%
      dplyr::arrange(`Economic Category`, Impact_Type)

    impacts_state_sum_seafood = impacts_state_sum %>%
      dplyr::bind_rows(impacts_state_sum_imports) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(Impact_Type) %>%
      dplyr::summarize(
        Direct = sum(Direct),
        Indirect = sum(Indirect),
        Induced = sum(Induced),
        Total = sum(Total)
      ) %>%
      dplyr::arrange(Impact_Type) %>%
      dplyr::mutate(`Economic Category` = "Harvesters and Seafood Industry")


    impacts_state_sum_out = impacts_state_sum %>%
      dplyr::bind_rows(impacts_state_sum_imports) %>%
      dplyr::bind_rows(impacts_state_sum_seafood)

    output = append(output, list("state_impacts" = impacts_state_sum_out))

  }



  if (format == "sector" | format == "all") {
    for (n in unique(impacts$`Economic Category`)) {
      impacts_sum = impacts %>%
        dplyr::filter(`Economic Category` == n)

      if (length(impacts_sum$fips) > 0) {
        impacts_sum = impacts_sum %>%
          dplyr::group_by(spec_no, Impact_Type) %>%
          dplyr::summarize(
            Direct = sum(Direct),
            Indirect = sum(Indirect),
            Induced = sum(Induced),
            Total = sum(Total)
          ) %>%
          dplyr::arrange(spec_no, Impact_Type)


        assign(paste0("impacts_sum_", n), impacts_sum)

        temp = list(get(paste0("impacts_sum_", n)))
        names(temp) <- paste0("impacts_sum_", n)
        output = append(output, temp)
      }
    }
  }

  if (format == "states" | format == "all") {
    for (n in 1:length(fp$fips)) {
      impacts_sum = impacts %>%
        dplyr::filter(fips == fp$fips[n])

      if (length(impacts_sum$fips) > 0 & impacts_sum$fips[1] != 0) {
        impacts_sum_imports = impacts_sum %>%
          dplyr::filter(`Economic Category` == "Brokers/importers") %>%
          dplyr::arrange(`Economic Category`, Impact_Type) %>%
          dplyr::mutate(`Economic Category` = "Imports and Brokers")

        impacts_sum = impacts_sum %>%
          dplyr::filter(`Economic Category` != "Brokers/importers") %>%
          dplyr::group_by(`Economic Category`, Impact_Type) %>%
          dplyr::summarize(
            Direct = sum(Direct),
            Indirect = sum(Indirect),
            Induced = sum(Induced),
            Total = sum(Total)
          ) %>%
          dplyr::arrange(`Economic Category`, Impact_Type)

        impacts_sum_seafood = impacts_sum %>%
          dplyr::bind_rows(impacts_sum_imports) %>%
          dplyr::ungroup() %>%
          dplyr::group_by(Impact_Type) %>%
          dplyr::summarize(
            Direct = sum(Direct),
            Indirect = sum(Indirect),
            Induced = sum(Induced),
            Total = sum(Total)
          ) %>%
          dplyr::arrange(Impact_Type) %>%
          dplyr::mutate(`Economic Category` = "Harvesters and Seafood Industry")


        impacts_sum %>%
          dplyr::bind_rows(impacts_sum_imports) %>%
          dplyr::bind_rows(impacts_sum_seafood)

        assign(paste0("impacts_sum_", fips$state_abbr[n]), impacts_sum)

        temp = list(get(paste0("impacts_sum_", fips$state_abbr[n])))
        names(temp) <- paste0("impacts_sum_", fips$state_abbr[n])
        output = append(output, temp)
      }
    }
  }

  if(format == "impact" | format == "all"){
    for (n in unique(impacts$Impact_Type)) {
      impacts_sum = impacts %>%
        dplyr::filter(Impact_Type == n & fips != 0)

      if (length(impacts_sum$fips) > 0) {
        impacts_sum = impacts_sum %>%
          dplyr::group_by(`Economic Category`, spec_no) %>%
          dplyr::summarize(
            Direct = sum(Direct),
            Indirect = sum(Indirect),
            Induced = sum(Induced),
            Total = sum(Total)
          ) %>%
          dplyr::arrange(`Economic Category`, spec_no)

        assign(paste0("impacts_sum_", stringr::str_replace_all(n, " ", "_")), impacts_sum)

        temp = list(get(paste0("impacts_sum_", stringr::str_replace_all(n, " ", "_"))))
        names(temp) <- paste0("impacts_sum_", stringr::str_replace_all(n, " ", "_"))
        output = append(output, temp)
      }
    }
  }

  if(format == "FEUS" | format == "Manual"){
    impacts.imports.states = impacts %>%
      dplyr::filter(`Economic Category` == "Brokers/importers") %>%
      dplyr::filter(fips != 0) %>%
      dplyr::left_join(fips) %>%
      dplyr::ungroup() %>%
      dplyr::select(-fips) %>%
      dplyr::rename(Metric = `Economic Category`, Sector = Impact_Type, State1 = state_abbr) %>%
      dplyr::mutate(Year = maxyr,
                    Direct = Direct/1000,
                    Indirect = Indirect/1000,
                    Induced = Induced/1000,
                    Total = Total/1000) %>%
      dplyr::mutate(Metric = "Importers")

    impacts.imports.us <- impacts %>%
      dplyr::filter(`Economic Category` == "Brokers/importers") %>%
      dplyr::filter(fips == 0) %>%
      dplyr::left_join(fips) %>%
      dplyr::ungroup() %>%
      dplyr::select(-fips) %>%
      dplyr::rename(Metric = `Economic Category`, Sector = Impact_Type, State1 = state_abbr) %>%
      dplyr::mutate(Year = maxyr,
                    Direct = Direct/1000,
                    Indirect = Indirect/1000,
                    Induced = Induced/1000,
                    Total = Total/1000) %>%
      dplyr::mutate(Metric = "Importers")


    impacts.states = impacts %>%
      dplyr::filter(`Economic Category` != "Brokers/importers") %>%
      dplyr::filter(fips != 0) %>%
      dplyr::group_by(fips, `Economic Category`, Impact_Type, Imports) %>%
      dplyr::summarize(Direct = sum(Direct),
                       Indirect = sum(Indirect),
                       Induced = sum(Induced),
                       Total = sum(Total)) %>%
      dplyr::left_join(fips) %>%
      dplyr::ungroup() %>%
      dplyr::select(-fips) %>%
      dplyr::rename(Metric = `Economic Category`, Sector = Impact_Type, State1 = state_abbr) %>%
      dplyr::mutate(Year = maxyr,
                    Direct = Direct/1000,
                    Indirect = Indirect/1000,
                    Induced = Induced/1000,
                    Total = Total/1000)

    impacts.us = impacts %>%
      dplyr::filter(`Economic Category` != "Brokers/importers") %>%
      dplyr::filter(fips == 0) %>%
      dplyr::group_by(fips, `Economic Category`, Impact_Type, Imports) %>%
      dplyr::summarize(Direct = sum(Direct),
                       Indirect = sum(Indirect),
                       Induced = sum(Induced),
                       Total = sum(Total)) %>%
      dplyr::left_join(fips) %>%
      dplyr::ungroup() %>%
      dplyr::select(-fips) %>%
      dplyr::rename(Metric = `Economic Category`, Sector = Impact_Type, State1 = state_abbr) %>%
      dplyr::mutate(Year = maxyr,
                    Direct = Direct/1000,
                    Indirect = Indirect/1000,
                    Induced = Induced/1000,
                    Total = Total/1000)

    impacts.allsectors.states = impacts.states %>%
      dplyr::bind_rows(impacts.imports.states) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(Sector, Imports, Year, State1) %>%
      dplyr::summarize(Direct = sum(Direct),
                       Indirect = sum(Indirect),
                       Induced = sum(Induced),
                       Total = sum(Total)) %>%
      dplyr::mutate(Metric = "Total Impacts")

    impacts.allsectors.us = impacts.us %>%
      dplyr::bind_rows(impacts.imports.us) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(Sector, Imports, Year) %>%
      dplyr::summarize(Direct = sum(Direct),
                       Indirect = sum(Indirect),
                       Induced = sum(Induced),
                       Total = sum(Total)) %>%
      dplyr::mutate(State1 = "US", Metric = "Total Impacts")

    output = dplyr::bind_rows(impacts.states, impacts.us, impacts.imports.states, impacts.imports.us, impacts.allsectors.states, impacts.allsectors.us) %>%
      dplyr::mutate(Index_Local = row_number())
  }

  if(xlsx == F){
    return(output)
  }

  if(xlsx == T){
    dir = getwd()
    for(n in 1:length(output)){
      xlsx::write.xlsx(output[n], file = paste0(dir, "/",output,"_impacts.xlsx"), sheetName = names(output)[n], append = T, row.names = F)
    }
    return(output)
  }
}
