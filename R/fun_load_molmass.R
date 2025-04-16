fun_load_molmass <- function(molmass_file) {
  df.mol.mass <- read_excel(molmass_file)
  sum.dat <- df.mol.mass |> 
    setDT() |> 
    mutate(molar_mass = as.numeric(`molar_mass(g/mol)`))

  return(sum.dat)
}