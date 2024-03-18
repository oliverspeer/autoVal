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
setwd("C:/R_local/labStat")

# read data ----------------------------------------------------------------
con <- dbConnect(SQLite(), dbname = "C:/R_local/labStat/ClinicalChemistry_test.db")

dbListTables(con)

query.bez <- "SELECT DISTINCT a.Bezeichnung, m.Gerät
                FROM MeasurementData a
                JOIN MethodData m ON a.Methode = m.Methode
                 WHERE m.Gerät = 'DxI';"

(query.bez.result <- dbGetQuery(con, query.bez))
fwrite(query.bez.result, "C:/R_local/autoVal/query_bez_DxI_result.csv")


# read DXI9000 validation data
setwd("C:/R_local/autoVal")

# read DXI data
val.dat <- read_excel("240307rawdata.xlsx")
setDT(val.dat)

# extract $ TestOrderCode and $ TestName into a new data.table
DXI.testName <- val.dat[, .(TestOrderCode, TestName)] |> unique()

# create a matrix to store distances between query.bez.result$Bezeichnung and DXI.testName$TestName
dist.mat <- matrix(nrow = nrow(query.bez.result), ncol = nrow(DXI.testName))
rownames(dist.mat) <- query.bez.result$Bezeichnung
colnames(dist.mat) <- DXI.testName$TestName

# function to adjust for abbreviation
adjust_for_abbreviation <- function(string1, string2, method = "osa") {
  base_distance <- stringdist::stringdist(string1, string2, method = method, nthread = 4)
  
  # Simple heuristic: if one string is a subset of the other, consider it a potential abbreviation
  if (grepl(string1, string2) | grepl(string2, string1)) {
    base_distance <- base_distance / 8  # Adjusting the score to reflect higher similarity
  }
  return(base_distance)
}


# calculate distances
for (i in 1:nrow(query.bez.result)) {
  for (j in 1:nrow(DXI.testName)) {
    dist.mat[i, j] <- adjust_for_abbreviation(query.bez.result$Bezeichnung[i], DXI.testName$TestName[j])
  }
}

# find the closest match
closest.match <- apply(dist.mat, 1, which.min)

# add the closest match to query.bez.result
query.bez.result$closest.match <- DXI.testName$TestName[closest.match]


