# function to tidy up data --------------------------------------
fun_write_tidy_data<- function(data = dxi.data, dt_name = "DT.tidy.dxi") {
  
  # prepare  id.cols that are not to be melted
  id.cols <- names(data)[!grepl("_\\d+", names(data))]
  
  # Melt the data.table
  DT.m1 = melt(
    data,
    id.vars = id.cols,
    variable.name = "Bezeichnung_Methode",
    value.name = "Werte",
    na.rm = TRUE
  )
  
  # Split 'Bezeichnung_Methode' into two columns 'Bezeichnung' and 'Methode'
  DT.m1[, c("Bezeichnung", "Methode") := tstrsplit(Bezeichnung_Methode, "_", fixed = TRUE)
  ][, Bezeichnung_Methode := NULL][, Methode := as.numeric(Methode)]
  
  dt_name <- setDT(DT.m1)
  
  
  # change the column names to be SQL compatible
  setnames(dt_name, old = c("Geb.datum", "Geschl.", "Auftragg."), new = c("DOB", "Geschlecht", "KundenID"))
  
  # change DOB and Datum to numeric
  # dt_name[, DOB := as.numeric(DOB)]
  # dt_name[, Datum := as.numeric(Datum)]
  
  # extract year, quarter, month, week, day from Datum
  dt_name[, Jahr := year(Datum)
  ][, Quartal := quarter(Datum)
  ][, Monat := month(Datum)
  ][, Woche := week(Datum)
  ][, Tag := day(Datum)
  ][, DOB := as.character(DOB)
  ][, Datum := as.character(Datum)
  ]
  
  
  
  return(dt_name)
}
