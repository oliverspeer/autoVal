
                  SELECT
                      (SELECT DISTINCT md.Methode
                      FROM MeasurementData AS md
                      JOIN MethodData AS m ON md.Methode = m.Methode
                      JOIN TranslationData AS t ON md.Methode = t.Methode
                      JOIN DxIvalData AS d ON t.TestOrderCode = d.TestOrderCode
                      WHERE d.TestName = main.TestName AND main.DoseUnit IS NOT NULL
                      LIMIT 1) AS inlab,
                      
                    TestName AS DxI9000,
                    
                    COUNT(*) AS n,
                      
                      
                  (SELECT DISTINCT DoseUnit
                       FROM DxIvalData AS sub
                       WHERE sub.TestName = main.TestName AND sub.DoseUnit IS NOT NULL
                       LIMIT 1) AS Einheit,
                  
                  (SELECT 
                      -- md.Bezeichnung as DxI800,
                      COUNT(*) -- AS n_800
                      FROM MeasurementData AS md
                      JOIN MethodData AS m ON md.Methode = m.Methode
                      JOIN TranslationData AS t ON md.Methode = t.Methode
                      JOIN DxIvalData AS d ON t.TestOrderCode = d.TestOrderCode
                      WHERE d.Probennummer = md.Probennummer AND d.Probennummer IS NOT NULL AND d.TestName = main.TestName
                      ) AS n_800,
                      
                  (SELECT DISTINCT m.EINHEIT
                      FROM MeasurementData AS md
                      JOIN MethodData AS m ON md.Methode = m.Methode
                      JOIN TranslationData AS t ON md.Methode = t.Methode
                      JOIN DxIvalData AS d ON t.TestOrderCode = d.TestOrderCode
                      WHERE d.TestName = main.TestName AND main.DoseUnit IS NOT NULL
                      LIMIT 1) AS Einheit_800
                  
                FROM 
                  DxIvalData AS main
                WHERE
                  DoseResult <> 'No result'
                GROUP BY
                  TestName;