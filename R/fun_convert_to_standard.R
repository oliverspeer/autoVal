# Function to convert units to a standard form----------------------------
fun_convert_to_standard <- function(unit) {
  install_unit("IU", "umol/min", "international unit")
  # Define a dictionary for common unit conversions
  unit_dict <- list(
    "ug/l" = set_units(1, "ug/l"),
    "ng/ml" = set_units(1, "ng/ml"),
    "nmol/l" = set_units(1, "nmol/l"),
    "umol/l" = set_units(1, "umol/l"),
    "pmol/l" = set_units(1, "pmol/l"),
    "ng/l" = set_units(1, "ng/l"),
    "pg/ml" = set_units(1, "pg/ml"),
    "ug/dl" = set_units(1, "ug/dl"),
    "ng/dl" = set_units(1, "ng/dl"),
    "mU/l" = set_units(1, "mIU/l"),
    "mIU/ml" = set_units(1, "mIU/ml"),
    "uIU/ml" = set_units(1, "uIU/ml"),
    "U/l" = set_units(1, "IU/l"),
    "nmol/l" = set_units(1, "nmol/l"),
    "µg/l" = set_units(1, ug/l),
    "ng/mL" = set_units(1, ng/ml),
    "ng/ml" = set_units(1, ng/ml),
    "nmol/l" = set_units(1, nmol/l),
    "µmol/l" = set_units(1, umol/l),
    "pmol/l" = set_units(1, pmol/l),
    "pmol/L" = set_units(1, pmol/l),
    "ng/l" = set_units(1, ng/l),
    "pg/mL" = set_units(1, pg/ml),
    "µg/dL" = set_units(1, ug/dl),
    "µg/l" = set_units(1, ug/l),
    "ng/dL" = set_units(1, ng/dl),
    "mU/l" = set_units(1, mIU/l),
    "mlU/l" = set_units(1, mIU/l), # Assumed conversion for demonstration
    "U/l" = set_units(1, IU/l),
    "µIU/mL" = set_units(1, uIU/ml),
    "mIU/mL" = set_units(1, mIU/ml),
    "nmol/L" = set_units(1, nmol/l), # assuming case insensitivity
    "nmol/l" = set_units(1, nmol/l),
    "g/mol" = set_units(1, g/mol)# assuming case insensitivity
  )
  
  
  
  # Check if unit exists in dictionary
  if (!unit #_lower 
      %in% names(unit_dict)) {
    message(paste("Unit", unit, "not found in dictionary."))
    return(NULL)
  }
  
  return(unit_dict[[unit]])
}