---------------------------------------------------
------------------- NOTES -------------------------
---------------------------------------------------

VI-SPDAT sub-scores are in VISPDATFamily. for family, single, and TAY
VI-SPDAT category scores are in VISPDAT. for family, single, and TAY

---------------------------------------------------
-------- calculate client age at entry ------------
---------------------------------------------------

DateDiff(year, cmClient.BirthDate, Enrollment.EnrollDate)

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
					And A.ClientID=Enrollment.ClientID
					AND A. AssessmentDate >= Enrollment.EnrollDate
					AND (A.AssessmentDate <= Enrollment.ExitDate
						OR Enrollment.ExitDate IS NULL)
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


---------------------------------------------------
---- use in filters to join CES enrollment to -----
---------- most recent housing referral -----------
---------------------------------------------------

(SELECT TOP 1 S.ServiceID
	FROM Service S WITH (NOLOCK) 
	INNER JOIN ServiceReferral SR  WITH (NOLOCK) ON S.ServiceID=SR.ServiceID AND S.EnrollID = Enrollment.EnrollID
	INNER JOIN cmProvider P ON SR.ReferToProviderID=P.ProviderID AND P.ActiveStatus <> 'D' AND P.ProviderName NOT LIKE '%Shelter%'
	INNER JOIN ServiceCode SC ON SR.ServiceCodeID=SC.ServiceCodeID AND SC.ActiveStatus <> 'D' AND SC.Service <> 'CES - Navigation Referral'
	WHERE S.ActiveStatus <> 'D' 
		AND (SC.Service IN ('CES - CHIP Referral','CES - OPH Referral','CES - PSH Referral','CES - RRH Referral','CES - TH Referral','CES - SSO Referral','CES - TH-RRH Referral')
			OR SC.Service LIKE '%SSVF Self-Match%'
			OR P.ProviderName LIKE '%SSVF%'
			OR P.ProviderName = 'InteCare - CES')
	ORDER BY S.BeginDate DESC)


---------------------------------------------------
---------- display information for most -----------
------------ recent outreach service --------------
---------------------------------------------------

SELECT TOP 1 concat(SC.Service, 
	concat(' (', 
		concat(FORMAT (S.BeginDate, 'MM/dd/yy'), ')'))) 
FROM cmClient C (nolock) 
INNER JOIN Service S (nolock) 
ON C.ClientID=S.ClientID
	AND S.BeginDate >= dateadd(month, -6,datefromparts(year(getdate()), month(getdate()), 1))
	AND S.ActiveStatus <> 'D' 
	AND C.ActiveStatus <> 'D'
	AND C.ClientID=cmClient.ClientID
INNER JOIN ServiceCode SC (nolock)
ON S.ServiceCodeID=SC.ServiceCodeID
	AND SC.OutreachContact = 'TRUE'
	AND SC.S <> 'CES - Client Contact'
	AND SC.ActiveStatus <> 'D'
ORDER BY S.BeginDate DESC

---------------------------------------------------
---------- use in join filter to join to ----------
---------- most recent HmisDataAssessment ---------
---------------------------------------------------

(SELECT TOP 1 HDA.AssessmentID
FROM cmClient C (nolock) 
INNER JOIN HmisDataAssessment HDA (nolock) 
ON C.ClientID=HDA.ClientID
	AND C.ClientID = cmClient.ClientID
	AND HDA.ActiveStatus <> 'D'
ORDER BY HDA.AssessmentDate DESC)




---------------------------------------------------
---------- prior residence/CLS groups -------------
---------------------------------------------------

Homeless Situation = (1, 16, 18)
Insitutional Situation = (4, 5, 6, 7, 15, 25)
Unknown = (8, 9, 99)
Transitional and Permanent Housing Situation = (2, 3, 10, 11, 14, 19, 20, 21, 28, 29, 31, 32, 33, 34, 35, 36)

---------------------------------------------------
------------- exit destination groups -------------
---------------------------------------------------

Homeless Situation = (1, 16, 18)
Institutional Situation = (4, 5, 6, 7, 15, 25)
Other/Unknown = (8, 9, 17, 24, 30, 99)
Temporary and Permanent Housing = (2, 3, 10, 11, 12, 13, 14, 19, 20, 21, 22, 23, 26, 27, 28, 29, 31, 32, 33, 34)
	Temporary = (2, 12, 13, 14, 27)
	Permanent = (3, 10, 11, 19, 20, 21, 22, 23, 26, 28, 29, 31, 32, 33, 34)

---------------------------------------------------
------------ average time from referral -----------
-------------- to move-in by program --------------
---------------------------------------------------

(SELECT AVG(DATEDIFF(DAY, S.BeginDate, ER.DateOfMoveIn))
FROM Programs P (nolock) 
LEFT JOIN EnrollmentCase EC (nolock) 
ON P.ProgramID=EC.ProgramID
	AND P.ProgramID = Programs.ProgramID
	AND EC.ActiveStatus <> 'D' 
INNER JOIN Enrollment E (nolock) 
ON EC.CaseID=E.CaseID
	AND E.ActiveStatus <> 'D' 
INNER JOIN EnrollmentRRH ER (nolock) 
ON E.EnrollID=ER.EnrollID 
	AND ER.DateOfMoveIn >= @StartDate
	AND ER.DateOfMoveIn <= @EndDate
INNER JOIN Service S (nolock) ON E.ClientID=S.ClientID
	AND S.ActiveStatus <> 'D' 
INNER JOIN CEEvent CEEvent (nolock) 
ON S.ServiceID=CEEvent.ServiceID
	AND CEEvent.ActiveStatus <> 'D'
WHERE P.ProgramID = CEEvent.Location)
	
---------------------------------------------------
------------- most recent HMIS data ---------------
--------- assessment (ee_universe, exit) ----------
-- change secondary sort to variable of interest --
---------------------------------------------------	
	
(SELECT TOP 1 HA.HmisID
FROM HmisDataAssessment HA (NOLOCK)
INNER JOIN Assessment A (NOLOCK)
ON HA.AssessmentID = A.AssessmentID
	AND HA.ActiveStatus <> 'D'
	AND A.ActiveStatus <> 'D'
	AND A.ClientID = Enrollment.ClientID
	AND A.AssessmentDate <= (
		CASE 
			WHEN Enrollment.ExitDate IS NULL 
				OR Enrollment.ExitDate > @EndDate THEN @EndDate
			ELSE Enrollment.ExitDate
		END)
ORDER BY A.AssessmentDate DESC, HA.HealthInsurance DESC)


---------------------------------------------------
-------------- select all from query --------------
---------------------------------------------------	

use *,1 as a SQL expression


---------------------------------------------------
------- most recent financial assessment ID -------
---------------------------------------------------
	
(SELECT TOP 1 FA.FinancialID
FROM FinancialAssessment FA (NOLOCK)
WHERE FA.ActiveStatus <> 'D'
	AND FA.ClientID = Enrollment.ClientID
	AND FA.AssessmentDate <= (
		CASE 
			WHEN Enrollment.ExitDate IS NULL 
				OR Enrollment.ExitDate > @EndDate THEN @EndDate
			ELSE Enrollment.ExitDate
		END)
ORDER BY FA.AssessmentDate DESC)


---------------------------------------------------
-------- enrollment length (with @EndDate) --------
---------------------------------------------------
	
DATEDIFF(DAY, EnrollDate, CASE 
			WHEN Enrollment.ExitDate IS NULL 
				OR Enrollment.ExitDate > @EndDate THEN @EndDate
			ELSE Enrollment.ExitDate
		END))

---------------------------------------------------
---------- relationship to HoH translator ---------
---------------------------------------------------
	
case
	when Enrollment.Relationship = 'SL' then 'Self'
	when Enrollment.Relationship = 'P' then 'Parent'
	when Enrollment.Relationship = 'S' then 'Son'
	when Enrollment.Relationship = 'D' then 'Daughter'
	when Enrollment.Relationship = 'DC' then 'Dependent Child'
	when Enrollment.Relationship = 'GP' then 'Grandparent'
	when Enrollment.Relationship = 'G' then 'Guardian'
	when Enrollment.Relationship = 'SP' then 'Spouse'
	when Enrollment.Relationship = 'F' then 'Other Family Member'
	when Enrollment.Relationship = 'O' then 'Other Non-Family'
	when Enrollment.Relationship = 'OC' then 'Other Caretaker'
end

