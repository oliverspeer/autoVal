
  SELECT 
    subquery.TestName AS DxI9000,
    -- subquery.Methode,
    -- subquery.TestOrderCode,
    COUNT(*) AS n_Doppel,
    subquery.TestCompleteDT AS letzteMessung
  FROM (
    SELECT DISTINCT
      a.Werte AS DxI800,
      d.DoseResult_c AS DxI9000,
      a.Bezeichnung,
      a.Methode,
      m.EINHEIT AS Einheit_800,
      d.DoseUnit AS Einheit_9000,
      d.Probennummer,
      d.TestName,
      d.TestCompleteDT
    FROM MeasurementData a
    JOIN MethodData m ON a.Methode = m.Methode
    JOIN TranslationData t ON a.Methode = t.Methode
    JOIN DxIvalData d ON t.TestOrderCode = d.TestOrderCode
    WHERE a.Probennummer = d.Probennummer 
      AND d.DoseResult <> 'No result'
  ) AS subquery
  GROUP BY subquery.TestName, 
        -- subquery.TestOrderCode, 
           subquery.Methode;
