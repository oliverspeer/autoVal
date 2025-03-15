SELECT 
                  TestName AS DxI9000,
                  COUNT(*) AS _n_QC_Messungen
                FROM 
                  DxIvalData
                WHERE
                  Probennummer IS NULL 
                  AND SampleID REGEXP '[a-zA-Z]+1' --_\d'
                  AND DoseResult <> 'No result'
                GROUP BY
                  TestName;