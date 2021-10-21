#' Run a Fisheries Input/Output Model
#'
#' This function takes in commercial fisheries catch numbers, IMPLAN multipliers, a GDP deflator, and imports numbers and outputs economic impacts.
#'
#' @param catch A data frame that details catch numbers at the state-species category level for a single year, including variables fips (FIPs number, 0 for US), spec_no (a numeric variable for species category), and base_catch (raw catch numbers in dollars).
#' @param import_numbers A data frame that includes imports numbers in dollars at the state level for a single year, including fips (FIPs number, 0 for US) and imports. Defaults to False for no imports.
#' @param implan_multipliers A data frame that includes 17 multipliers at the state-species category-economic category for a single year. Defaults to David Records numbers.
#' @param deflator A numeric value that adjusts jobs numbers from the current year to the year the multipliers were made; defaults to 0.8734298, which represents 2017 to 2014.
#' @param imports_state_multipliers A data frame that includes multipliers governing the percentages of imports going to each economic category for a single year. Defaults to 2017 numbers. Set to False for no imports.
#' @param manual A binary value indicating whether values were manually input. Defaults to FALSE
#' @importFrom magrittr %>%
#' @export
io_calculator <- function(catch, import_numbers = F, implan_multipliers = multipliers, deflator = 0.8734298, import_state_multipliers = imports_states, manual = FALSE) {

  base_catch = catch %>% dplyr::mutate(spec_no = dplyr::case_when(
    `Species Category` == "Shrimp" ~ 1,
    `Species Category` == "Crab" ~ 2,
    `Species Category` == "Lobster" ~ 3,
    `Species Category` == "East Coast Groundfish" ~ 4,
    `Species Category` == "HMS" ~ 5,
    `Species Category` == "Reef Fish" ~ 6,
    `Species Category` == "West Coast Groundfish " ~ 7,
    `Species Category` == "West Coast Groundfish" ~ 7,
    `Species Category` == "West Coast Whiting " ~ 8,
    `Species Category` == "West Coast Whiting" ~ 8,
    `Species Category` == "Halibut" ~ 9,
    `Species Category` == "Menhaden and Industrial" ~ 10,
    `Species Category` == "Salmon" ~ 11,
    `Species Category` == "Sea Scallop" ~ 12,
    `Species Category` == "Pelagic Herring and Mackerel" ~ 13,
    `Species Category` == "Surf Clam and Ocean Quahog " ~ 14,
    `Species Category` == "Surf Clam and Ocean Quahog" ~ 14,
    `Species Category` == "Surf Clam/Ocean Quahog" ~ 14,
    `Species Category` == "Other Trawl" ~ 15,
    `Species Category` == "All Other Finfish" ~ 16,
    `Species Category` == "All Other Shellfish  " ~ 17,
    `Species Category` == "All Other Shellfish" ~ 17,
    `Species Category` == "Freshwater " ~ 18,
    `Species Category` == "Freshwater" ~ 18,
    `Species Category` == "Inshore and Miscellaneous" ~ 19,
    `Species Category` == "Bait" ~ 20,
    `Species Category` == "Imports" ~ 0)) %>%
    dplyr::select(-`Species Category`) %>%
    dplyr::filter(!is.na(spec_no))

  if(length(base_catch$base_catch[base_catch$State == "US"])==0) {
    US_base_catch = base_catch %>%
      dplyr::select(base_catch, spec_no, Year) %>%
      dplyr::group_by(spec_no, Year) %>%
      dplyr::summarize(base_catch = sum(base_catch)) %>%
      dplyr::mutate(Region = "National",
                    State = "US",
                    fips = 0)
  } else {
    US_base_catch = base_catch %>% dplyr::filter(State == "US")
    base_catch = base_catch %>% dplyr::filter(State != "US")
  }

  base_catch = dplyr::bind_rows(US_base_catch, base_catch)

  imports = import_numbers
  multipliers = implan_multipliers
  import_states = import_state_multipliers

  ############
  # Cleaning #
  ############

  if(imports!=F) {
    imports = import_states %>%
      dplyr::left_join(imports) %>%
      dplyr::mutate(imports = imports * value) %>%
      dplyr::select(fips, `Economic Category` = name, base_catch = imports) %>%
      dplyr::mutate(`Species Category` = "Imports")
  } else {

  }

  if(manual == TRUE){
    multipliers = multipliers %>%
      dplyr::filter(fips %in% unique(base_catch$fips))
  }


  multipliers_harvesters = multipliers %>% dplyr::filter(`Economic Category` == "Harvesters")

  multipliers_processors = multipliers %>% dplyr::filter(`Economic Category` == "Processors")

  multipliers_wholesalers = multipliers %>% dplyr::filter(`Economic Category` == "Wholesalers")

  multipliers_grocers = multipliers %>% dplyr::filter(`Economic Category` == "Grocers")

  multipliers_restaurants = multipliers %>% dplyr::filter(`Economic Category` == "Restaurants")

  multipliers_brokers = multipliers %>% dplyr::filter(state_name == "All jurisdictions") %>%
    dplyr::select(-state_abbr, -fips)

  ##############
  # Harvesters #
  ##############

  multipliers_harvesters = multipliers_harvesters %>%
    dplyr::mutate(Species.Category = `Species Category`) %>%
    dplyr::left_join(base_catch, by = c("spec_no", "fips")) %>%
    dplyr::mutate(
      PI_Direct_Impact = `Personal Income Direct Impacts` * base_catch,
      PI_Indirect_Impact = `Personal Income Indirect Impacts` * `RPC RPC` *
        base_catch,
      PI_Induced_Impact = `Personal Income Induced Impacts` * `RPC RPC` *
        base_catch,
      PI_Total = PI_Direct_Impact + PI_Indirect_Impact + PI_Induced_Impact,
      TV_Direct_Impact = `Total Value Direct Impacts` * base_catch,
      TV_Indirect_Impact = `Total Value Indirect Impacts` * `RPC RPC` *
        base_catch,
      TV_Induced_Impact = `Total Value Induced Impacts` * `RPC RPC` *
        base_catch,
      TV_Total = TV_Direct_Impact + TV_Indirect_Impact + TV_Induced_Impact,
      O_Direct_Impact = `Output Direct Impacts` * base_catch,
      O_Indirect_Impact = `Output Indirect Impacts` * `RPC RPC` * base_catch,
      O_Induced_Impact = `Output Induced Impacts` * `RPC RPC` * base_catch,
      O_Total = O_Direct_Impact + O_Indirect_Impact + O_Induced_Impact,
      E_Direct_Impact = `Employment Direct Impacts` * base_catch * deflator / 1000000,
      E_Indirect_Impact = `Employment Indirect Impacts` * `RPC RPC` *
        base_catch * deflator / 1000000,
      E_Induced_Impact = `Employment Induced Impacts` * `RPC RPC` * base_catch * deflator / 1000000,
      E_Total = E_Direct_Impact + E_Indirect_Impact + E_Induced_Impact
    )

  harvesters_output = multipliers_harvesters %>% dplyr::select(
    fips,
    `Economic Category`,
    `Species Category`,
    spec_no,
    PI_Direct_Impact,
    PI_Indirect_Impact,
    PI_Induced_Impact,
    PI_Total,
    TV_Direct_Impact,
    TV_Indirect_Impact,
    TV_Induced_Impact,
    TV_Total,
    O_Direct_Impact,
    O_Indirect_Impact,
    O_Induced_Impact,
    O_Total,
    E_Direct_Impact,
    E_Indirect_Impact,
    E_Induced_Impact,
    E_Total
  )



  ##############
  # Processors #
  ##############

  multipliers_processors = multipliers_processors %>%
    dplyr::mutate(Species.Category = `Species Category`) %>%
    dplyr::left_join(base_catch, by = c("spec_no", "fips")) %>%
    dplyr::mutate(processor_inputs = base_catch * Harvesters)

  if (imports != F) {
    for (n in unique(multipliers_processors$fips)) {
      multipliers_processors$processor_inputs[multipliers_processors$`Species Category` == "Imports" &
                                                multipliers_processors$fips == n] <-
        imports$base_catch[imports$fips == n &
                             imports$`Economic Category` == "Processors"]
    }
  }

  multipliers_processors = multipliers_processors %>%
    dplyr::mutate(processor_markup = processor_inputs * markup) %>%
    dplyr::mutate(
      PI_Direct_Impact = `Personal Income Direct Impacts` * processor_markup,
      PI_Indirect_Impact = `Personal Income Indirect Impacts` * `RPC RPC` *
        processor_markup,
      PI_Induced_Impact = `Personal Income Induced Impacts` * `RPC RPC` *
        processor_markup,
      PI_Total = PI_Direct_Impact + PI_Indirect_Impact + PI_Induced_Impact,
      TV_Direct_Impact = `Total Value Direct Impacts` * processor_markup,
      TV_Indirect_Impact = `Total Value Indirect Impacts` * `RPC RPC` *
        processor_markup,
      TV_Induced_Impact = `Total Value Induced Impacts` * `RPC RPC` *
        processor_markup,
      TV_Total = TV_Direct_Impact + TV_Indirect_Impact + TV_Induced_Impact,
      O_Direct_Impact = `Output Direct Impacts` * processor_markup,
      O_Indirect_Impact = `Output Indirect Impacts` * `RPC RPC` * processor_markup,
      O_Induced_Impact = `Output Induced Impacts` * `RPC RPC` * processor_markup,
      O_Total = O_Direct_Impact + O_Indirect_Impact + O_Induced_Impact,
      E_Direct_Impact = `Employment Direct Impacts` * processor_markup * deflator / 1000000,
      E_Indirect_Impact = `Employment Indirect Impacts` * `RPC RPC` *
        processor_markup * deflator / 1000000,
      E_Induced_Impact = `Employment Induced Impacts` * `RPC RPC` * processor_markup * deflator / 1000000,
      E_Total = E_Direct_Impact + E_Indirect_Impact + E_Induced_Impact
    )

  processors_output = multipliers_processors %>% dplyr::select(
    fips,
    `Economic Category`,
    `Species Category`,
    spec_no,
    PI_Direct_Impact,
    PI_Indirect_Impact,
    PI_Induced_Impact,
    PI_Total,
    TV_Direct_Impact,
    TV_Indirect_Impact,
    TV_Induced_Impact,
    TV_Total,
    O_Direct_Impact,
    O_Indirect_Impact,
    O_Induced_Impact,
    O_Total,
    E_Direct_Impact,
    E_Indirect_Impact,
    E_Induced_Impact,
    E_Total
  )

  processor_inputs = multipliers_processors %>% dplyr::select(fips, spec_no, processor_inputs, processor_markup)



  ###############
  # Wholesalers #
  ###############

  multipliers_wholesalers = multipliers_wholesalers %>%
    dplyr::mutate(Species.Category = `Species Category`) %>%
    dplyr::left_join(base_catch, by = c("spec_no", "fips")) %>%
    dplyr::left_join(processor_inputs, by = c("spec_no", "fips")) %>%
    dplyr::mutate(wholesaler_inputs = base_catch * Harvesters + Processors * (processor_inputs +
                                                                                processor_markup))
  if(imports != F) {
    for (n in unique(multipliers_wholesalers$fips)) {
      multipliers_wholesalers$wholesaler_inputs[multipliers_wholesalers$`Species Category` == "Imports" &
                                                  multipliers_wholesalers$fips == n] <-
        imports$base_catch[imports$fips == n &
                             imports$`Economic Category` == "Wholesalers"]
    }
  }

  multipliers_wholesalers = multipliers_wholesalers %>%
    dplyr::mutate(wholesaler_markup = wholesaler_inputs * markup) %>%
    dplyr::mutate(
      PI_Direct_Impact = `Personal Income Direct Impacts` * wholesaler_markup,
      PI_Indirect_Impact = `Personal Income Indirect Impacts` * `RPC RPC` *
        wholesaler_markup,
      PI_Induced_Impact = `Personal Income Induced Impacts` * `RPC RPC` *
        wholesaler_markup,
      PI_Total = PI_Direct_Impact + PI_Indirect_Impact + PI_Induced_Impact,
      TV_Direct_Impact = `Total Value Direct Impacts` * wholesaler_markup,
      TV_Indirect_Impact = `Total Value Indirect Impacts` * `RPC RPC` *
        wholesaler_markup,
      TV_Induced_Impact = `Total Value Induced Impacts` * `RPC RPC` *
        wholesaler_markup,
      TV_Total = TV_Direct_Impact + TV_Indirect_Impact + TV_Induced_Impact,
      O_Direct_Impact = `Output Direct Impacts` * wholesaler_markup,
      O_Indirect_Impact = `Output Indirect Impacts` * `RPC RPC` * wholesaler_markup,
      O_Induced_Impact = `Output Induced Impacts` * `RPC RPC` * wholesaler_markup,
      O_Total = O_Direct_Impact + O_Indirect_Impact + O_Induced_Impact,
      E_Direct_Impact = `Employment Direct Impacts` * wholesaler_markup * deflator / 1000000,
      E_Indirect_Impact = `Employment Indirect Impacts` * `RPC RPC` *
        wholesaler_markup * deflator / 1000000,
      E_Induced_Impact = `Employment Induced Impacts` * `RPC RPC` * wholesaler_markup * deflator / 1000000,
      E_Total = E_Direct_Impact + E_Indirect_Impact + E_Induced_Impact
    )

  wholesalers_output = multipliers_wholesalers %>% dplyr::select(
    fips,
    `Economic Category`,
    `Species Category`,
    spec_no,
    PI_Direct_Impact,
    PI_Indirect_Impact,
    PI_Induced_Impact,
    PI_Total,
    TV_Direct_Impact,
    TV_Indirect_Impact,
    TV_Induced_Impact,
    TV_Total,
    O_Direct_Impact,
    O_Indirect_Impact,
    O_Induced_Impact,
    O_Total,
    E_Direct_Impact,
    E_Indirect_Impact,
    E_Induced_Impact,
    E_Total
  )

  wholesaler_inputs = multipliers_wholesalers %>% dplyr::select(fips,
                                                                spec_no,
                                                                wholesaler_inputs,
                                                                wholesaler_markup)


  ###########
  # Grocers #
  ###########

  multipliers_grocers = multipliers_grocers %>%
    dplyr::mutate(Species.Category = `Species Category`) %>%
    dplyr::left_join(base_catch, by = c("spec_no", "fips")) %>%
    dplyr::left_join(processor_inputs, by = c("spec_no", "fips")) %>%
    dplyr::left_join(wholesaler_inputs, by = c("spec_no", "fips")) %>%
    dplyr::mutate(
      grocer_inputs = base_catch * Harvesters + Processors * (processor_inputs +
                                                                processor_markup) + Wholesalers * (wholesaler_inputs + wholesaler_markup)
    )

  if (imports != F) {
    for (n in unique(multipliers_grocers$fips)) {
      multipliers_grocers$grocer_inputs[multipliers_grocers$`Species Category` == "Imports" &
                                          multipliers_grocers$fips == n] <-
        imports$base_catch[imports$fips == n &
                             imports$`Economic Category` == "Grocers"]
    }
  }

  multipliers_grocers = multipliers_grocers %>%
    dplyr::mutate(grocer_markup = grocer_inputs * markup) %>%
    dplyr::mutate(
      PI_Direct_Impact = `Personal Income Direct Impacts` * grocer_markup,
      PI_Indirect_Impact = `Personal Income Indirect Impacts` * `RPC RPC` *
        grocer_markup,
      PI_Induced_Impact = `Personal Income Induced Impacts` * `RPC RPC` *
        grocer_markup,
      PI_Total = PI_Direct_Impact + PI_Indirect_Impact + PI_Induced_Impact,
      TV_Direct_Impact = `Total Value Direct Impacts` * grocer_markup,
      TV_Indirect_Impact = `Total Value Indirect Impacts` * `RPC RPC` *
        grocer_markup,
      TV_Induced_Impact = `Total Value Induced Impacts` * `RPC RPC` *
        grocer_markup,
      TV_Total = TV_Direct_Impact + TV_Indirect_Impact + TV_Induced_Impact,
      O_Direct_Impact = `Output Direct Impacts` * grocer_markup,
      O_Indirect_Impact = `Output Indirect Impacts` * `RPC RPC` * grocer_markup,
      O_Induced_Impact = `Output Induced Impacts` * `RPC RPC` * grocer_markup,
      O_Total = O_Direct_Impact + O_Indirect_Impact + O_Induced_Impact,
      E_Direct_Impact = `Employment Direct Impacts` * grocer_markup * deflator / 1000000,
      E_Indirect_Impact = `Employment Indirect Impacts` * `RPC RPC` *
        grocer_markup * deflator / 1000000,
      E_Induced_Impact = `Employment Induced Impacts` * `RPC RPC` * grocer_markup * deflator / 1000000,
      E_Total = E_Direct_Impact + E_Indirect_Impact + E_Induced_Impact
    )

  grocers_output = multipliers_grocers %>% dplyr::select(
    fips,
    `Economic Category`,
    `Species Category`,
    spec_no,
    PI_Direct_Impact,
    PI_Indirect_Impact,
    PI_Induced_Impact,
    PI_Total,
    TV_Direct_Impact,
    TV_Indirect_Impact,
    TV_Induced_Impact,
    TV_Total,
    O_Direct_Impact,
    O_Indirect_Impact,
    O_Induced_Impact,
    O_Total,
    E_Direct_Impact,
    E_Indirect_Impact,
    E_Induced_Impact,
    E_Total
  )

  grocer_inputs = multipliers_grocers %>% dplyr::select(fips, spec_no, grocer_inputs, grocer_markup)



  ###############
  # Restaurants #
  ###############

  multipliers_restaurants = multipliers_restaurants %>%
    dplyr::mutate(Species.Category = `Species Category`) %>%
    dplyr::left_join(base_catch, by = c("spec_no", "fips")) %>%
    dplyr::left_join(processor_inputs, by = c("spec_no", "fips")) %>%
    dplyr::left_join(wholesaler_inputs, by = c("spec_no", "fips")) %>%
    dplyr::left_join(grocer_inputs, by = c("spec_no", "fips")) %>%
    dplyr::mutate(
      restaurant_inputs = base_catch * Harvesters + Processors * (processor_inputs +
                                                                    processor_markup) + Wholesalers * (wholesaler_inputs + wholesaler_markup) + Grocers *
        (grocer_inputs + grocer_markup)
    )

  if(imports != F) {
    for (n in unique(multipliers_restaurants$fips)) {
      multipliers_restaurants$restaurant_inputs[multipliers_restaurants$`Species Category` == "Imports" &
                                                  multipliers_restaurants$fips == n] <-
        imports$base_catch[imports$fips == n &
                             imports$`Economic Category` == "Restaurants"]
    }
  }

  multipliers_restaurants = multipliers_restaurants %>%
    dplyr::mutate(restaurant_markup = restaurant_inputs * markup) %>%
    dplyr::mutate(
      PI_Direct_Impact = `Personal Income Direct Impacts` * restaurant_markup,
      PI_Indirect_Impact = `Personal Income Indirect Impacts` * `RPC RPC` *
        restaurant_markup,
      PI_Induced_Impact = `Personal Income Induced Impacts` * `RPC RPC` *
        restaurant_markup,
      PI_Total = PI_Direct_Impact + PI_Indirect_Impact + PI_Induced_Impact,
      TV_Direct_Impact = `Total Value Direct Impacts` * restaurant_markup,
      TV_Indirect_Impact = `Total Value Indirect Impacts` * `RPC RPC` *
        restaurant_markup,
      TV_Induced_Impact = `Total Value Induced Impacts` * `RPC RPC` *
        restaurant_markup,
      TV_Total = TV_Direct_Impact + TV_Indirect_Impact + TV_Induced_Impact,
      O_Direct_Impact = `Output Direct Impacts` * restaurant_markup,
      O_Indirect_Impact = `Output Indirect Impacts` * `RPC RPC` * restaurant_markup,
      O_Induced_Impact = `Output Induced Impacts` * `RPC RPC` * restaurant_markup,
      O_Total = O_Direct_Impact + O_Indirect_Impact + O_Induced_Impact,
      E_Direct_Impact = `Employment Direct Impacts` * restaurant_markup * deflator / 1000000,
      E_Indirect_Impact = `Employment Indirect Impacts` * `RPC RPC` *
        restaurant_markup * deflator / 1000000,
      E_Induced_Impact = `Employment Induced Impacts` * `RPC RPC` * restaurant_markup * deflator / 1000000,
      E_Total = E_Direct_Impact + E_Indirect_Impact + E_Induced_Impact
    )

  restaurants_output = multipliers_restaurants %>% dplyr::select(
    fips,
    `Economic Category`,
    `Species Category`,
    spec_no,
    PI_Direct_Impact,
    PI_Indirect_Impact,
    PI_Induced_Impact,
    PI_Total,
    TV_Direct_Impact,
    TV_Indirect_Impact,
    TV_Induced_Impact,
    TV_Total,
    O_Direct_Impact,
    O_Indirect_Impact,
    O_Induced_Impact,
    O_Total,
    E_Direct_Impact,
    E_Indirect_Impact,
    E_Induced_Impact,
    E_Total
  )

  ###########
  # Brokers #
  ###########
  if(import_numbers!=F) {
    broker_output = import_numbers %>%
      dplyr::bind_cols(multipliers_brokers) %>%
      dplyr::mutate(broker_markup = (imports) * 1.186554) %>%
      dplyr::mutate(
        PI_Direct_Impact = `Personal Income Direct Impacts` * broker_markup,
        PI_Indirect_Impact = `Personal Income Indirect Impacts` * `RPC RPC` *
          broker_markup,
        PI_Induced_Impact = `Personal Income Induced Impacts` * `RPC RPC` *
          broker_markup,
        PI_Total = PI_Direct_Impact + PI_Indirect_Impact + PI_Induced_Impact,
        TV_Direct_Impact = `Total Value Direct Impacts` * broker_markup,
        TV_Indirect_Impact = `Total Value Indirect Impacts` * `RPC RPC` *
          broker_markup,
        TV_Induced_Impact = `Total Value Induced Impacts` * `RPC RPC` *
          broker_markup,
        TV_Total = TV_Direct_Impact + TV_Indirect_Impact + TV_Induced_Impact,
        O_Direct_Impact = `Output Direct Impacts` * broker_markup,
        O_Indirect_Impact = `Output Indirect Impacts` * `RPC RPC` * broker_markup,
        O_Induced_Impact = `Output Induced Impacts` * `RPC RPC` * broker_markup,
        O_Total = O_Direct_Impact + O_Indirect_Impact + O_Induced_Impact,
        E_Direct_Impact = `Employment Direct Impacts` * broker_markup * deflator / 1000000,
        E_Indirect_Impact = `Employment Indirect Impacts` * `RPC RPC` *
          broker_markup * deflator / 1000000,
        E_Induced_Impact = `Employment Induced Impacts` * `RPC RPC` * broker_markup * deflator / 1000000,
        E_Total = E_Direct_Impact + E_Indirect_Impact + E_Induced_Impact
      ) %>% view()
    dplyr::select(
      fips,
      `Economic Category`,
      `Species Category`,
      spec_no,
      PI_Direct_Impact,
      PI_Indirect_Impact,
      PI_Induced_Impact,
      PI_Total,
      TV_Direct_Impact,
      TV_Indirect_Impact,
      TV_Induced_Impact,
      TV_Total,
      O_Direct_Impact,
      O_Indirect_Impact,
      O_Induced_Impact,
      O_Total,
      E_Direct_Impact,
      E_Indirect_Impact,
      E_Induced_Impact,
      E_Total
    )
  } else {
    broker_output = NULL
  }



  ################
  # Final Output #
  ################

  final_output = dplyr::bind_rows(
    harvesters_output,
    processors_output,
    wholesalers_output,
    grocers_output,
    restaurants_output,
    broker_output
  )
  final_output[is.na(final_output)] <- 0

  if(imports==F){
    final_output = final_output %>%
      dplyr::mutate(Imports = "No")
  }
  if(imports!=F){
    final_output = final_output %>%
      dplyr::mutate(Imports = "Yes")
  }

  return(final_output)

}
