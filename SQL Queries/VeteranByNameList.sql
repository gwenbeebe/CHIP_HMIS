--==============================================
-------------  VETERAN BY NAME LIST  -----------
--==============================================
-- Gwen Beebe
-- Created 2.26.21
-------------------------------------------------


-------------------------------------------------
----------------  UPDATE HISTORY  ---------------
-------------------------------------------------
/*
2.26.21 -	Created limited tables to speed up querying by only including data that may be relevant
				- #Veterans table includes only active cmClient records for veterans
				- #TimeLimitedEnrollments table includes only active records from the Enrollment table that were open at some point in the last three years
				- #TimeLimitedEnrollments table includes only active records from the Service table that were active in the last three years
			Created first draft of homeless date table, including:
				- enrollments with a prior residence of ES/SH/TH or literal homelessness, actively fleeing DV, or into ES/SH/TH/SO programs
				- exits to ES/SH/TH or literal homelessness or impute today's date for current shelter stayers
				- services coded as street outreach contacts
				- current living situations recorded as ES/SH/TH or literal homelessness
*/


------------------------------------------------
---------  DROP REMAINING TEMP TABLES  ---------
------------------------------------------------
IF OBJECT_ID('tempdb.dbo.#Veterans') IS NOT NULL DROP TABLE #Veterans;
IF OBJECT_ID('tempdb.dbo.#TimeLimitedEnrollments') IS NOT NULL DROP TABLE #TimeLimitedEnrollments;
IF OBJECT_ID('tempdb.dbo.#TimeLimitedServices') IS NOT NULL DROP TABLE #TimeLimitedServices;

IF OBJECT_ID('tempdb.dbo.#HomelessDates') IS NOT NULL DROP TABLE #HomelessDates;


------------------------------------------------
----------  CREATE BASE TEMP TABLES  -----------
------------------------------------------------
-- create the #Veterans table
SELECT ClientID
INTO #Veterans
FROM dbo.cmClient WITH (NOLOCK)
WHERE ActiveStatus <> 'D'
	AND VeteranStatus = 1

-- create the #TimeLimitedEnrollments table
SELECT EnrollID, ClientID, CaseID, EnrollDate, EnrollAssessmentID, ExitDate, ExitDestination
INTO #TimeLimitedEnrollments
FROM dbo.Enrollment WITH (NOLOCK)
WHERE ActiveStatus <> 'D'
	AND EnrollDate <= SYSDATETIME()
	AND (ExitDate IS NULL 
		OR ExitDate >= DATEADD(year, -3, SYSDATETIME()))

-- create the #TimeLimitedServices table
SELECT ClientID, ProviderID, BeginDate, ServiceCodeID
INTO #TimeLimitedServices
FROM dbo.Service WITH (NOLOCK)
WHERE ActiveStatus <> 'D'
	AND BeginDate <= SYSDATETIME()
	AND (EndDate IS NULL 
		OR EndDate >= DATEADD(year, -3, SYSDATETIME()))


------------------------------------------------
-----  CREATE STATUS TABLE - HOMELESSNESS  -----
------------------------------------------------
--	get exit dates that indicate homelessness, or the current date for clients in ES/SH/TH
SELECT DISTINCT V.ClientID, (CASE WHEN E.ExitDate IS NULL THEN SYSDATETIME() ELSE E.ExitDate END) AS EffectiveDate
INTO #HomelessDates
FROM #Veterans V 
INNER JOIN #TimeLimitedEnrollments E ON E.ClientID = V.ClientID
LEFT OUTER JOIN dbo.EnrollmentCase EC WITH (NOLOCK) ON E.CaseID = EC.CaseID AND EC.ActiveStatus <> 'D'
LEFT OUTER JOIN dbo.Programs P WITH (NOLOCK) ON EC.ProgramID = P.ProgramID AND P.ActiveStatus <> 'D'
WHERE E.ExitDestination IN (1, 2, 16, 18)						-- include all exits to ES, SH, TH, or unsheltered homelessness
	OR (P.ProgramType IN (1, 2, 4, 8)							-- include all open enrollments in ES, SH, and TH programs
		AND E.ExitDate IS NULL)	
UNION
--	get enrollment dates that indicate homelessness
SELECT DISTINCT V.ClientID, E.EnrollDate
FROM #Veterans V 
INNER JOIN #TimeLimitedEnrollments E ON E.ClientID = V.ClientID
LEFT OUTER JOIN dbo.EnrollmentCase EC WITH (NOLOCK) ON E.CaseID = EC.CaseID AND EC.ActiveStatus <> 'D'
LEFT OUTER JOIN dbo.Programs P WITH (NOLOCK) ON EC.ProgramID = P.ProgramID AND P.ActiveStatus <> 'D'
LEFT OUTER JOIN dbo.HMISDataAssessment A WITH (NOLOCK) ON A.AssessmentID = E.EnrollAssessmentID AND A.ActiveStatus <> 'D'
LEFT OUTER JOIN dbo.DomesticViolenceAssessment DV WITH (NOLOCK) ON DV.AssessmentID = E.EnrollAssessmentID AND DV.ActiveStatus <> 'D'
WHERE A.PriorResidence IN (1, 2, 16, 18)						-- include all enrollments with a prior residence of ES, SH, TH, or unsheltered homelessness
	OR (DV.DomViolenceExp = 1									-- include all enrollments with DV history and currently fleeing
		AND DV.CurrentlyFleeing = 1)		
	OR P.ProgramType IN (1, 2, 4, 8)							-- include all enrollments to ES, SH, TH, and SO programs
UNION
--	get service dates that indicate homelessness
SELECT DISTINCT V.ClientID, S.BeginDate
FROM #Veterans V
INNER JOIN #TimeLimitedServices S ON V.ClientID = S.ClientID
LEFT OUTER JOIN dbo.ServiceCode SC ON S.ServiceCodeID = SC.ServiceCodeID AND SC.ActiveStatus <> 'D'
WHERE SC.OutreachContact = 'true'								-- include all services marked as outreach contacts
UNION
--	get current living situation dates that indicate homelessness
SELECT DISTINCT V.ClientID, CLS.LivingSituationDate
FROM #Veterans V
INNER JOIN dbo.HMIS_LivingSituation CLS WITH (NOLOCK) ON V.ClientID = CLS.ClientID AND CLS.ActiveStatus <> 'D'
WHERE CLS.LivingSituationDate >= DATEADD(year, -3, SYSDATETIME())
	AND CLS.LivingSituation IN (1, 2, 16, 18)					-- include all current living situations in the last three years recorded as ES, SH, TH, or unsheltered homelessness


------------------------------------------------
--------  CREATE STATUS TABLE - HOUSED  --------
------------------------------------------------
--	get exit dates that indicate housed status, or the current date for clients housed in RRH/PSH/OPH programs
SELECT DISTINCT V.ClientID, (CASE WHEN E.ExitDate IS NULL THEN SYSDATETIME() ELSE E.ExitDate END) AS EffectiveDate
--INTO #HomelessDates
FROM #Veterans V 
INNER JOIN #TimeLimitedEnrollments E ON E.ClientID = V.ClientID
LEFT OUTER JOIN dbo.EnrollmentRRH ER WITH (NOLOCK) ON ER.EnrollID = E.EnrollID
LEFT OUTER JOIN dbo.EnrollmentCase EC WITH (NOLOCK) ON E.CaseID = EC.CaseID AND EC.ActiveStatus <> 'D'
LEFT OUTER JOIN dbo.Programs P WITH (NOLOCK) ON EC.ProgramID = P.ProgramID AND P.ActiveStatus <> 'D'
WHERE E.ExitDestination IN (3, 10, 11, 19, 20, 21, 22, 23, 26, 28, 29, 31, 32, 33, 34)		-- include all exits to permanent destinations
	OR (P.ProgramType IN (3, 9, 10, 55)							-- include all open enrollments in housing programs with a HMID
		AND E.ExitDate IS NULL
		AND ER.DateOfMoveIn IS NOT NULL)	
UNION
--	get housing move in dates for RRH/PSH/OPH programs
SELECT DISTINCT V.ClientID, ER.DateOfMoveIn
--INTO #HomelessDates
FROM #Veterans V 
INNER JOIN #TimeLimitedEnrollments E ON E.ClientID = V.ClientID
LEFT OUTER JOIN dbo.EnrollmentRRH ER WITH (NOLOCK) ON ER.EnrollID = E.EnrollID
LEFT OUTER JOIN dbo.EnrollmentCase EC WITH (NOLOCK) ON E.CaseID = EC.CaseID AND EC.ActiveStatus <> 'D'
LEFT OUTER JOIN dbo.Programs P WITH (NOLOCK) ON EC.ProgramID = P.ProgramID AND P.ActiveStatus <> 'D'
WHERE P.ProgramType IN (3, 9, 10, 55)							-- include all HMIDs for housing programs
		AND ER.DateOfMoveIn IS NOT NULL		


------------------------------------------------
--------  JOIN AND LABEL STATUS TABLES  --------
------------------------------------------------
--SELECT DISTINCT ClientID, CAST (EffectiveDate AS DATE) AS EffectiveDate, 'Homeless' AS Status
--From #HomelessDates



------------------------------------------------
--------------  DROP TEMP TABLES  --------------
------------------------------------------------
DROP TABLE #Veterans
DROP TABLE #TimeLimitedEnrollments
DROP TABLE #TimeLimitedServices

DROP TABLE #HomelessDates