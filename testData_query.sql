
  SELECT DISTINCT 
    d.TestName AS DxI9000, 
    CAST(md.Werte AS REAL) AS x, 
    CAST(d.DoseResult_c AS REAL) AS y
  FROM MeasurementData md
  --JOIN MethodData m USING (Methode)
  JOIN TranslationData t USING (Methode)
  JOIN DxIvalData d USING (TestOrderCode)
  WHERE d.Probennummer = md.Probennummer
    AND d.DoseResult <> 'No result';