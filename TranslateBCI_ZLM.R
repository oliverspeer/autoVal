# translation table BCI TestNames to ZLM Bezeichnung
# libraries
library(data.table)
library(tidyverse)
library(readxl)
library(nephro)
library(DBI)
library(RSQLite)
library(stringdist)

# set working directory ----------------------------------------------------
source("StartUp.R")
StartUpRoutine()
# setwd("C:/R_local/labStat")
# con <- dbConnect(SQLite(), dbname = "C:/R_local/labStat/ClinicalChemistry_test.db")

# read data ----------------------------------------------------------------


dbListTables(con)

query.bez <- "SELECT DISTINCT a. Methode, a.Bezeichnung
                FROM MeasurementData a
                JOIN MethodData m ON a.Methode = m.Methode
                 WHERE m.Gerät = 'DxI';"

(query.bez.result <- dbGetQuery(con, query.bez))
fwrite(query.bez.result, "C:/R_local/autoVal/query_bez_DxI_result.csv")

query.testName <- "SELECT DISTINCT TestOrderCode, TestName
                    FROM DxIvalData;"
query.testName.result <- dbGetQuery(con, query.testName)

# read DXI9000 validation data
setwd("C:/R_local/autoVal")

# read DXI data
val.dat <- read_excel("240307rawdata.xlsx")
setDT(val.dat)

# extract $ TestOrderCode and $ TestName into a new data.table
DXI.testName <- val.dat[, .(TestOrderCode, TestName)] |> unique()

# Combined function for string distance measurement & check for key term- and abbreviations-presence
adjust_for_key_terms_and_abbreviation <- function(string1, string2, method = "osa", key_terms = c("t3", "t4", "25", "fol", "hs", "thy")) {
  
  # Calculate string distance
  base_distance <- stringdist(tolower(string1), tolower(string2), method = method, nthread = 4)
  
  # Adjust for key terms
  for (term in key_terms) {
    if (grepl(term, tolower(string1)) && grepl(term, tolower(string2))) {
      base_distance <- base_distance / 2 # Adjusting the score for key term presence
    }
  }
  
  # Adjust for potential abbreviation (one string is a subset of the other)
  # Simple heuristic: if one string is a subset of the other, consider it a potential abbreviation
  if (grepl(tolower(string1), tolower(string2)) | grepl(tolower(string2), tolower(string1))) {
    base_distance <- base_distance / 8 # Further adjusting the score for abbreviation
  }
  
  return(base_distance)
}

# Assuming query.bez.result and DXI.testName data frames are already defined

# Create a matrix to store distances
dist.mat <- matrix(nrow = nrow(query.bez.result), ncol = nrow(DXI.testName))
rownames(dist.mat) <- query.bez.result$Bezeichnung
colnames(dist.mat) <- DXI.testName$TestName

# Calculate distances using the combined custom function
for (i in 1:nrow(query.bez.result)) {
  for (j in 1:nrow(DXI.testName)) {
    dist.mat[i, j] <- adjust_for_key_terms_and_abbreviation(query.bez.result$Bezeichnung[i], DXI.testName$TestName[j])
  }
}

# Find the closest match
closest.match <- apply(dist.mat, 1, which.min)

# Add the closest match to query.bez.result
query.bez.result$TestName <- DXI.testName$TestName[closest.match]

# merge DXI.testName and query.bez.result on closest.match
query.bez.result <- merge(query.bez.result, DXI.testName, by.x = "TestName", by.y = "TestName", all.x = TRUE)
 
# change order of columns from 1,2,3,4 to 2,3,1,4
query.bez.result <- query.bez.result[, c(2, 3, 1, 4)]

# Write the results to a file
fwrite(query.bez.result, "C:/R_local/autoVal/query_bez_DxI_result.csv", sep = ";")

# read the results 
query.bez.result <- fread("C:/R_local/autoVal/query_bez_DxI_result.csv", sep = ";", encoding = "UTF-8")


