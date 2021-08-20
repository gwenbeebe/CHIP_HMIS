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

DATEDIFF(DAY, EnrollDate, CASE 
			WHEN Enrollment.ExitDate IS NULL 
				OR Enrollment.ExitDate > @EndDate THEN @EndDate
			ELSE Enrollment.ExitDate
		END))
		
NGJ!9LzK6uJ{jM'q		
		


(SELECT TOP 1 S.ServiceID 
FROM Service S (NOLOCK) 
INNER JOIN ServiceCode SC (NOLOCK)  
ON S.ServiceCodeID = SC.ServiceCodeID 	
AND cmClient.ClientID = S.ClientID 	
AND S.ActiveStatus <> 'D'  	
AND SC.ActiveStatus <> 'D' 	
AND (SC.Service = 'CES - Coordinated Outreach - Downtown' 		
	OR SC.Service LIKE 'CO - %') ORDER BY S.BeginDate DESC)


		
		
if [Program Name (group)] = "Emergency Shelter" then [Client ID] end
		

Enrollment.EnrollDate <= @EndDateInput@ AND (Enrollment.ExitDate >= @StartDateInput@ OR Enrollment.ExitDate IS NULL)
"we had spent"
"we increased our available funding by"

if [Numbers (Numbers)] = 1 then "In " + [Month1] + " we increased our available funding by" else "By the end of " + [Month1] + ", we had spent" END

(SELECT MAX(S.BeginDate) 	

FROM Service S 	
INNER JOIN ServiceReferral SR ON S.ServiceID=SR.ServiceID 
	AND SR.Result <> 100 	
	AND S.ClientID = Enrollment.ClientID 
	AND S.BeginDate <= Enrollment.EnrollDate 
	
	AND Enrollment.EnrollDate >= DATEADD(MONTH, 6, S.BeginDate) 
	AND EnrollmentRRH.DateOfMoveIn >= DATEADD(MONTH, -4, GETDATE())
	
INNER JOIN cmProvider P ON SR.ReferToProviderID=P.ProviderID 
	AND P.ActiveStatus <> 'D' 
	AND P.ProviderName NOT LIKE '%Shelter%' 	
INNER JOIN ServiceCode SC ON SR.ServiceCodeID=SC.ServiceCodeID 
	AND SC.ActiveStatus <> 'D' 
	AND SC.Service <> 'CES - Navigation Referral' 	
WHERE S.ActiveStatus <> 'D'  				
	AND (SC.Service IN ('CES - CHIP Referral','CES - OPH Referral','CES - PSH Referral','CES - RRH Referral','CES - TH Referral','CES - SSO Referral') 			
		OR SC.Service LIKE '%SSVF Self-Match%' 			
		OR P.ProviderName LIKE '%SSVF%' 			
		OR P.ProviderName = 'InteCare - CES'))

CASE WHEN (SELECT SUM(CASE WHEN UnitInventory > 0 THEN UnitInventory END)
FROM ProgramHousingInventory P
WHERE P.ProgramID = Programs.ProgramID
	AND P.StartDate <= GETDATE()
	AND (P.EndDate IS NULL
		OR P.EndDate > GETDATE())) > 0

THEN		

CASE WHEN COUNT(DISTINCT(Enrollment.CaseID)) > 0
AND (SELECT SUM(CASE WHEN UnitInventory > 0 THEN UnitInventory END)
FROM ProgramHousingInventory P
WHERE P.ProgramID = Programs.ProgramID
	AND P.StartDate <= GETDATE()
	AND (P.EndDate IS NULL
		OR P.EndDate > GETDATE())) > 0
THEN
COUNT(DISTINCT(Enrollment.CaseID))
* 100
/
(SELECT SUM(CASE WHEN UnitInventory > 0 THEN UnitInventory END)
FROM ProgramHousingInventory P
WHERE P.ProgramID = Programs.ProgramID
	AND P.StartDate <= GETDATE()
	AND (P.EndDate IS NULL
		OR P.EndDate > GETDATE()))
END

ELSE

CASE WHEN COUNT(DISTINCT(Enrollment.ClientID)) > 0
AND (SELECT SUM(HMISParticipatingBeds)
FROM ProgramHousingInventory P
WHERE P.ProgramID = Programs.ProgramID
	AND P.StartDate <= GETDATE()
	AND (P.EndDate IS NULL
		OR P.EndDate > GETDATE())) > 0
THEN
COUNT(DISTINCT(Enrollment.ClientID))
* 100
/
(SELECT SUM(HMISParticipatingBeds)
FROM ProgramHousingInventory P
WHERE P.ProgramID = Programs.ProgramID
	AND P.StartDate <= GETDATE()
	AND (P.EndDate IS NULL
		OR P.EndDate > GETDATE()))
END

END

CASE WHEN COUNT(DISTINCT(Enrollment.CaseID)) > 0
AND (SELECT SUM(CASE WHEN UnitInventory > 0 THEN UnitInventory END)
FROM ProgramHousingInventory P
WHERE P.ProgramID = Programs.ProgramID
	AND P.StartDate <= GETDATE()
	AND (P.EndDate IS NULL
		OR P.EndDate > GETDATE())) > 0
THEN
COUNT(DISTINCT(Enrollment.CaseID))
* 100
/
(SELECT SUM(CASE WHEN UnitInventory > 0 THEN UnitInventory END)
FROM ProgramHousingInventory P
WHERE P.ProgramID = Programs.ProgramID
	AND P.StartDate <= GETDATE()
	AND (P.EndDate IS NULL
		OR P.EndDate > GETDATE()))
END



SELECT SUM(CASE WHEN UnitInventory > 0 THEN UnitInventory END)
FROM ProgramHousingInventory P
WHERE P.ProgramID = Programs.ProgramID
	AND P.StartDate <= GETDATE()
	AND (P.EndDate IS NULL
		OR P.EndDate > GETDATE())

-- File Renaming
-- set working directory with
cd "path"
-- then paste in this code
-- update department name first!
Get-ChildItem -File -Recurse | Rename-Item -NewName {"CoC Coordination - $($_.BaseName) - $($_.CreationTime.ToString('MM.dd.yy'))$($_.Extension)"}


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

([Toggle] and not([Program Type (group)] = [Current Program Type])) 
or [Current Program Type] = "ALL" 
or [Program Type (group)] = "Rapid Re-Housing"
or [Program Type (group)] = "Other Permanent Housing"
or [Program Type (group)] = "Permanent Supportive Housing"






CASE WHEN (SELECT COUNT(DISTINCT(E.ClientID))
FROM Enrollment E (NOLOCK)
INNER JOIN EnrollmentCase EC (NOLOCK)
ON E.CaseID = EC.CaseID
	AND E.ActiveStatus <> 'D' 
	AND EC.ActiveStatus <> 'D' 
	AND EC.ProgramID = Programs.ProgramID
	AND E.ExitDate >= DATEADD(YEAR, -2, @StartDate)
	AND E.ExitDate <= DATEADD(YEAR, -2, @EndDate)
	AND E.ExitDestination IN (3, 10, 11, 19, 20, 21, 22, 23, 26, 28, 29, 31, 32, 33, 34)) > 0
THEN 

((SELECT COUNT(DISTINCT(E.ClientID))
FROM Enrollment E (NOLOCK)
INNER JOIN EnrollmentCase EC (NOLOCK)
ON E.CaseID = EC.CaseID
	AND E.ActiveStatus <> 'D' 
	AND EC.ActiveStatus <> 'D' 
	AND EC.ProgramID = Programs.ProgramID
	AND E.ExitDate >= DATEADD(YEAR, -2, @StartDate)
	AND E.ExitDate <= DATEADD(YEAR, -2, @EndDate)
	AND E.ExitDestination IN (3, 10, 11, 19, 20, 21, 22, 23, 26, 28, 29, 31, 32, 33, 34)
INNER JOIN Enrollment R (NOLOCK)
ON E.ClientID = R.ClientID
	AND R.ActiveStatus <> 'D' 
	AND R.EnrollDate > DATEADD(DAY, 14, E.ExitDate)
	AND R.EnrollDate <= DATEADD(YEAR, 2, E.ExitDate)
INNER JOIN EnrollmentCase EC_R 
ON R.CaseID = EC_R.CaseID
	AND EC_R.ActiveStatus <> 'D' 
INNER JOIN Programs P_R 
ON EC_R.ProgramID = P_R.ProgramID
	AND P_R.ActiveStatus <> 'D' 
	AND P_R.ProgramType IN (1, 2, 3, 4, 8, 9, 10, 55))

* 100
/

(SELECT COUNT(DISTINCT(E.ClientID))
FROM Enrollment E (NOLOCK)
INNER JOIN EnrollmentCase EC (NOLOCK)
ON E.CaseID = EC.CaseID
	AND E.ActiveStatus <> 'D' 
	AND EC.ActiveStatus <> 'D' 
	AND EC.ProgramID = Programs.ProgramID
	AND E.ExitDate >= DATEADD(YEAR, -2, @StartDate)
	AND E.ExitDate <= DATEADD(YEAR, -2, @EndDate)
	AND E.ExitDestination IN (3, 10, 11, 19, 20, 21, 22, 23, 26, 28, 29, 31, 32, 33, 34)))
END



use wingdings??

(CASE
	WHEN Service.UnitsOfMeasure IN ('H', 'M') THEN '<b><font color="red"




SELECT MAX(S.BeginDate)
FROM Service S (NOLOCK)
INNER JOIN CEEvent C (NOLOCK)
ON S.ServiceID = C.ServiceID
	AND S.ActiveStatus <> 'D'
	AND C.ActiveStatus <> 'D'
	AND S.ClientID = cmClient.ClientID
	AND C.Location = Programs.ProgramID
	
	
	
m = 3;
if (m < 0) {
  m = 11 + m;
} 
m;



AND (@CreatedFilter@ = 'FALSE'
OR Enrollment.CreatedBy = @UserID@)

AND (@AssignedFilter@ = 'FALSE'
OR Enrollment.ClntCaseID IS NULL
OR Enrollment.ClntCaseID IN 
	(SELECT cmCaseAssign.ClntCaseID
	FROM cmCaseAssign (nolock)
	WHERE ActiveStatus <> 'D'
		AND UserID = @UserID@
		AND BeginDate <= @EndDateInput@
		AND (EndDate IS NULL
			OR EndDate >= @StartDateInput@))
	)


AND (@CreatedFilter@ = 'FALSE'
OR Enrollment.CreatedBy = @UserID@)

AND (@AssignedFilter@ = 'FALSE'
OR Enrollment.ClntCaseID IS NULL
OR @UserID@ = 
	(SELECT TOP 1 CA.UserID
	FROM cmCaseAssign CA (nolock)
	WHERE CA.ActiveStatus <> 'D'
		AND CA.ClntCaseID = Enrollment.ClntCaseID
		AND CA.BeginDate <= @EndDateInput@
		AND (CA.EndDate IS NULL
			OR CA.EndDate >= @StartDateInput@)
	ORDER BY CA.BeginDate DESC)
	)


new Date(@CT_ChronicChecklist.Month_1@.getFullYear() - 3, @CT_ChronicChecklist.Month_1@.getMonth(), 1)

(CASE WHEN custom_data_completeness.IncomeAmountPresent = 1
	THEN '<font color="green">Okay</font>'
WHEN custom_data_completeness.IncomeAmountPresent = 0
	THEN '<b><font color="red">Missing</font></b>'
ELSE '-' END



SELECT 1000000191 AS ListItemTypeID, CDC.ProgramName as ListItemKey, CDC.ProgramName AS ListItemLabel, ISNULL(PT.Description, 'No Type Specified')  ListItemGroup
FROM custom_data_completeness CDC (NOLOCK)
INNER JOIN Enrollment E (NOLOCK) ON CDC.EnrollID = E.EnrollID 
AND E.EnrollDate <= @EndDateInput@ 
AND (E.ExitDate >= @StartDateInput@ OR E.ExitDate IS NULL)
INNER JOIN EnrollmentCase EC (NOLOCK) ON EC.CaseID = E.CaseID
INNER JOIN Programs P ON P.ProgramID = EC.ProgramID
ORDER BY PT.Description, O.ProgramName





@Month5@.toLocaleString('default', { month: 'short' }) + " " + @Month5@.toLocaleString('default', { year: '2-digit' })

CASE WHEN CT_YouthAssessment.FosterCare = 1 OR custom_summary_client_disabilities.[History of Foster Care] = 1 THEN 1 ELSE 0 END
new Date(@CT_ChronicChecklist.Month_1@.getFullYear(), @CT_ChronicChecklist.Month_1@.getMonth(), 1)

(SELECT COUNT(DISTINCT(E.ClientID))
FROM Enrollment E (NOLOCK)
INNER JOIN EnrollmentCase EC (NOLOCK) ON E.CaseID = EC.CaseID
	AND EC.ProgramID = Programs.ProgramID
	AND E.ActiveStatus = 'A'
	AND E.EnrollDate <= @EndDateInput@
	AND (
		(E.ExitDate >= @StartDateInput@
			AND E.ExitDate <= @EndDateInput@
			AND E.ExitDestination IN (3, 10, 11, 19, 20, 21, 22, 23, 26, 28, 29, 31, 32, 33, 34))
		OR E.ExitDate > @EndDateInput@
		OR E.ExitDate IS NULL))
*100
/
((SELECT COUNT(DISTINCT(E.ClientID))
FROM Enrollment E (NOLOCK)
INNER JOIN EnrollmentCase EC (NOLOCK) ON E.CaseID = EC.CaseID
	AND EC.ProgramID = Programs.ProgramID
	AND E.ActiveStatus = 'A'
	AND E.ExitDate >= @StartDateInput@ AND E.ExitDate <= @EndDateInput@)
- 
(SELECT COUNT(DISTINCT(E.ClientID))
FROM Enrollment E (NOLOCK)
INNER JOIN EnrollmentCase EC (NOLOCK) ON E.CaseID = EC.CaseID
	AND EC.ProgramID = Programs.ProgramID
	AND E.ActiveStatus = 'A'
	AND E.ExitDate >= @StartDateInput@
	and E.ExitDate <= @EndDateInput@ 
	and (E.ExitDestination in (6, 24)
		or (Programs.ProgramType <> 4 and E.ExitDestination in (25, 15))
		or (Programs.ProgramType = 4 and E.ExitDestination = 29))))
		
		
		
		
		
wingdings for checkboxes		
=IIF(Fields!Gender_BirthMale.Value = 1,  "þ", "o")		
		


'<span style="font-family:Wingdings; font-size:150%">' +
(CASE WHEN custom_data_completeness.IncomeAmountPresent = 1
	THEN '<font color="green">&#252;</font>'
WHEN custom_data_completeness.IncomeAmountPresent = 0
	THEN '<font color="red">&#251;</font>'
ELSE '&#150;' END)
+ '</span>'






(SELECT TOP 1 S.ServiceID
	FROM Service S WITH (NOLOCK) 
	INNER JOIN ServiceReferral SR WITH (NOLOCK) ON S.ServiceID=SR.ServiceID AND S.ClientID = Enrollment.ClientID
	INNER JOIN cmProvider P WITH (NOLOCK) ON SR.ReferToProviderID=P.ProviderID AND P.ActiveStatus <> 'D' AND P.ProviderName NOT LIKE '%Shelter%'
	INNER JOIN ServiceCode SC WITH (NOLOCK) ON SR.ServiceCodeID=SC.ServiceCodeID AND SC.ActiveStatus <> 'D' AND SC.Service <> 'CES - Navigation Referral'
	WHERE S.ActiveStatus <> 'D' 
		AND (SC.Service IN ('CES - CHIP Referral','CES - OPH Referral','CES - PSH Referral','CES - RRH Referral','CES - TH Referral','CES - SSO Referral','CES - TH-RRH Referral')
			OR SC.Service LIKE '%SSVF Self-Match%'
			OR P.ProviderName LIKE '%SSVF%'
			OR P.ProviderName = 'InteCare - CES')
	ORDER BY S.BeginDate DESC)
	
	
	
	(CASE WHEN (SR.Result = 10) OR (SR.ServiceID IS NOT NULL AND SR.Result IS NULL)



(SELECT TOP 1 D.DATE
FROM (
(SELECT E.ExitDate AS Date
FROM Enrollment E (NOLOCK)
WHERE E.ActiveStatus = 'A'
AND E.ClientID = cmClient.ClientID
AND E.ExitDate >= Service.BeginDate 
AND E.ExitDestination IN (3, 10, 11, 19, 20, 21, 22, 23, 26, 28, 29, 31, 32, 33, 34))
UNION
(SELECT ER.DateOfMoveIn AS Date
FROM Enrollment E (NOLOCK)
INNER JOIN EnrollmentRRH ER (NOLOCK)
ON ER.EnrollID = E.EnrollID
AND E.ActiveStatus = 'A'
AND E.ClientID = cmClient.ClientID
AND ER.DateOfMoveIn >= Service.BeginDate)) D
ORDER BY D.DATE DESC
)




(
	((@GrantTypeID@ || @parent.GrantTypeID@)=='RHY' || (/(^|,|\s)hry(,|$)/i.test(@GrantTypeList@ || @parent.GrantTypeList@))  || (/(^|,|\s)yhd(,|$|\s)/i.test(@ComponentList@ || @parent.ComponentList@ ))) && 
	@assessmenttype@ == '1'
) && 
((@Birthdate@) < @DateAdd(y, -18 )@ || (@hocrelation@)=='SL')



EnrollmentRRH DateOfMoveIn





TimeDates.DayNumberOfMonth = 1
AND TimeDate.FullDateAlternateKey <= BaseDate
AND TimeDate.FullDateAlternateKey >= DATEADD(YEAR, -3, BaseDate)