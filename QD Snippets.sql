---------------------------------------------------
------------------- NOTES -------------------------
---------------------------------------------------

VI-SPDAT sub-scores are in VISPDATFamily. for family, single, and TAY
VI-SPDAT category scores are in VISPDAT. for family, single, and TAY

---------------------------------------------------
-------- calculate client age at entry ------------
---------------------------------------------------

(
	CONVERT(
		int,
		CONVERT(
			char(8),
			Enrollment.[EnrollDate],
			112)
		)
	-
	CONVERT(
		char(8),
		cmClient.[Birthdate],
		112
		)
)
/10000


---------------------------------------------------
---- get most recent VI-SPDAT score for client ----
---------------------------------------------------

(
	SELECT max(ScoreTotal) 
	FROM VISPDAT V (NOLOCK) 
	INNER JOIN
		(SELECT VI.ClientID, 
			(SELECT TOP 1 VIA.VulnerabilityID 
				FROM VulnerabilityIndex VIA (NOLOCK) 
				INNER JOIN Assessment A (NOLOCK) 
				ON A.AssessmentID = VIA.AssessmentID
					AND VIA.ActiveStatus<>'D' 
					AND A.ActiveStatus<>'D' 
					And VIA.ClientID=cmClient.ClientID
				ORDER BY VIA.AssessmentDate DESC, VulnerabilityID ) 
			AS VID
		FROM VulnerabilityIndex VI (NOLOCK) 
		GROUP BY VI.ClientID	
		) LastVI 
		ON LastVI.VID = V.VulnerabilityID		 
)


---------------------------------------------------
--- get first shelter entry in last six months ----
---------------------------------------------------

(SELECT min(E.[EnrollDate])
FROM Enrollment E (NOLOCK) 
INNER JOIN EnrollmentCase EC(NOLOCK) 
	ON E.CaseID=EC.CaseID
		AND EC.ActiveStatus <> 'D'
		AND E.[EnrollDate]>= DATEADD(MONTH, -6, GETDATE()) 
INNER JOIN Programs P (NOLOCK) 
	ON EC.ProgramID = P.ProgramID
		AND P.ProgramType = 1
WHERE E.ClientID = cmClient.[ClientID]
GROUP BY E.[ClientID]		 
)


---------------------------------------------------
------------ get number of case members -----------
---------------------------------------------------

(SELECT COUNT(*)
	FROM Enrollment E
	WHERE E.CaseID = Enrollment.CaseID
		AND E.ActiveStatus = 'A'
		-- AND E.ExitDate IS NULL		-- use this line for number of people currently enrolled in case
	GROUP BY E.CaseID)


---------------------------------------------------
----- use in filters with start parameter ---------
----- to include information for clients ----------
------------- active in period --------------------
---------------------------------------------------

(SELECT max(case 
		when E.[ExitDate] is null then getdate() 
			else E.[ExitDate] end
		) 
FROM Enrollment E (NOLOCK)  
INNER JOIN EnrollmentCase EC(NOLOCK)  	
ON E.CaseID=EC.CaseID 		
	AND EC.ActiveStatus <> 'D' 
WHERE E.ClientID = cmClient.[ClientID] 
GROUP BY E.[ClientID])

---------------------------------------------------
--------------- calculate COVID score -------------
---------------------------------------------------

(SELECT min(COVIDScore)
FROM
	(SELECT PL.[CaseID] AS [CaseID],
		(SELECT CASE WHEN CC.Age >= 60
				AND COUNT(CV_EC.Condition) > 0 then 1
				WHEN CC.Age >= 60
				OR COUNT(CV_EC.Condition) > 0 then 2 end
			FROM COVID19Screening CV_S (nolock)
			LEFT JOIN custom_COVID_in_existing_conditions CV_EC (nolock) 
				ON CV_S.COVIDID=CV_EC.COVIDID 
				AND CV_S.ActiveStatus <> 'D' 
			LEFT JOIN ClientCalculations CC
				ON CC.ClientID = CV_S.ClientID
			WHERE C.ClientID = CV_S.ClientID
			GROUP BY CV_S.ClientID, CC.Age) AS [COVIDScore]
	FROM Custom_VW_PrioritizationList PL (nolock)
	INNER JOIN cmClient C (nolock) 
		ON PL.ClientID = C.ClientID) AS T
WHERE Custom_VW_PrioritizationList.CaseID = T.CaseID
GROUP BY T.CaseID)

---------------------------------------------------
-------- use in filters to join client to ---------
------------ most recent enrollment ---------------
---------------------------------------------------

(SELECT max(E.[EnrollID])
FROM Enrollment E (NOLOCK) 
INNER JOIN EnrollmentCase EC(NOLOCK) 
	ON E.CaseID=EC.CaseID
		AND EC.ActiveStatus <> 'D'
INNER JOIN 
	(SELECT E.ClientID, max(E.[EnrollDate]) AS max_EnrollDate
		FROM Enrollment E (NOLOCK) 
		INNER JOIN EnrollmentCase EC(NOLOCK) 
			ON E.CaseID=EC.CaseID
				AND EC.ActiveStatus <> 'D'
		WHERE E.ClientID = HMIS_LivingSituation.[ClientID]
		GROUP BY E.[ClientID]) AS E_D
	ON E_D.max_EnrollDate = E.EnrollDate
		AND E_D.ClientID = E.ClientID
WHERE E.ClientID = HMIS_LivingSituation.[ClientID]
GROUP BY E.[ClientID]		 
)

