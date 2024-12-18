# prepare libraries and database connection--------------------------

#setwd("H:/R/autoVal_H")
setwd("C:/R_local/autoVal")
source("StartUp.R")
StartUpRoutine()

# read out col.names from TABLE DxIvalData
col.names.old <- dbListFields(con, "DxIvalData")
col.names.new <- readRDS("C:/R_local/autoVal/val.dat.colnames.RDS")
col.names.new <- c(col.names.new, rep(NA, length(col.names.old)-length(col.names.new)))

# write out mapping template
library(writexl)

mapping.template <- data.frame(
  old_name = col.names.old,
  new_name = col.names.new,
  stringsAsFactors = FALSE
)

write_xlsx(mapping.template, "C:/R_local/autoVal/mapping_template.xlsx")

#mapping it done in Excel
# read in mapping template
mapping.final <- read_excel("C:/R_local/autoVal/mapping_template.xlsx")


# create new table structure in database while copying data from old table

select.parts <- sapply(seq_len(nrow(mapping.final)), function(i) {
  old.col <- mapping.final$old_name[i]
  new.col <- mapping.final$new_name[i]
  
  new.col <- paste0("`", new.col, "`")
  
  if (old.col %in% col.names.old) {
    # existierende Spalte aus TABLE1 umbenennen
    paste(old.col, "AS", new.col)
  } else {
    # nicht existierende Spalte, mit NULL füllen
    paste("NULL AS", new.col)
  }
})

query <- paste0("CREATE TABLE DxIvalData25 AS SELECT ", paste(select.parts, collapse = ", "), " FROM DxIvalData;")
dbExecute(con, query)


dbExecute(con, query)
