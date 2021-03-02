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
3.1.21 -	Created first draft of housed date table, including:
				- exits to housed situations or impute today's date for folks currently in a housing program with a HMID
				- housing move-in dates
				- services coded as rental assistance or deposit assistance in the HUD or SSVF categorizations
				- current living situations recorded as housed situations
			Added provider and event type columns to both homeless and housed tables
			Created joined table for validation and status determination
			Started status determination logic
				- housed = most recent status is housed
				- newly homeless = only homeless events, none older than three months old (event date >= three months ago)
				- inactive = most recent status is homeless, but it is more than three months ago (event date < three months ago)
				- return from housed = ?
				- active = ?
				- return from inactive = ?
			First draft of logic for getting days since last event
*/

USE Indy
------------------------------------------------
---------  DROP REMAINING TEMP TABLES  ---------
------------------------------------------------
IF OBJECT_ID('tempdb.dbo.#Veterans') IS NOT NULL DROP TABLE #Veterans;
IF OBJECT_ID('tempdb.dbo.#TimeLimitedEnrollments') IS NOT NULL DROP TABLE #TimeLimitedEnrollments;
IF OBJECT_ID('tempdb.dbo.#TimeLimitedServices') IS NOT NULL DROP TABLE #TimeLimitedServices;

IF OBJECT_ID('tempdb.dbo.#HomelessDates') IS NOT NULL DROP TABLE #HomelessDates;
IF OBJECT_ID('tempdb.dbo.#HousedDates') IS NOT NULL DROP TABLE #HousedDates;
IF OBJECT_ID('tempdb.dbo.#FullStatusAndDateTable') IS NOT NULL DROP TABLE #FullStatusAndDateTable;


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
SELECT ClientID, OrgID, EnrollID, BeginDate, ServiceCodeID
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
SELECT DISTINCT V.ClientID, (CASE WHEN E.ExitDate IS NULL THEN SYSDATETIME() ELSE E.ExitDate END) AS EffectiveDate, P.ProgramName,
	(CASE WHEN E.ExitDestination IN (1, 2, 16, 18) THEN 'Homeless Exit From Program' ELSE 'Still Enrolled in Program' END) AS EventType
INTO #HomelessDates
FROM #Veterans V 
INNER JOIN #TimeLimitedEnrollments E ON E.ClientID = V.ClientID
LEFT OUTER JOIN dbo.EnrollmentCase EC WITH (NOLOCK) ON E.CaseID = EC.CaseID AND EC.ActiveStatus <> 'D'
LEFT OUTER JOIN dbo.Programs P WITH (NOLOCK) ON EC.ProgramID = P.ProgramID AND P.ActiveStatus <> 'D'
WHERE E.ExitDestination IN (1, 2, 16, 18)						-- include all exits to ES, SH, TH, or unsheltered homelessness
	OR (P.ProgramType IN (1, 2, 4, 8)							-- include all open enrollments in ES, SH, and TH programs
		AND E.ExitDate IS NULL)	

UNION ALL

--	get enrollment dates that indicate homelessness
SELECT DISTINCT V.ClientID, E.EnrollDate, P.ProgramName, 'Literally Homeless Enrollment' AS EventType
FROM #Veterans V 
INNER JOIN #TimeLimitedEnrollments E ON E.ClientID = V.ClientID
LEFT OUTER JOIN dbo.EnrollmentCase EC WITH (NOLOCK) ON E.CaseID = EC.CaseID AND EC.ActiveStatus <> 'D'
LEFT OUTER JOIN dbo.Programs P WITH (NOLOCK) ON EC.ProgramID = P.ProgramID AND P.ActiveStatus <> 'D'
LEFT OUTER JOIN dbo.HMISDataAssessment A WITH (NOLOCK) ON A.AssessmentID = E.EnrollAssessmentID AND A.ActiveStatus <> 'D'
LEFT OUTER JOIN dbo.DomesticViolenceAssessment DV WITH (NOLOCK) ON DV.AssessmentID = E.EnrollAssessmentID AND DV.ActiveStatus <> 'D'
WHERE E.EnrollDate >= DATEADD(year, -3, SYSDATETIME())			-- only include enrollments in the last three years
	AND (A.PriorResidence IN (1, 2, 16, 18)						-- include all enrollments with a prior residence of ES, SH, TH, or unsheltered homelessness
		OR (DV.DomViolenceExp = 1								-- include all enrollments with DV history and currently fleeing
			AND DV.CurrentlyFleeing = 1)		
		OR P.ProgramType IN (1, 2, 4, 8))						-- include all enrollments to ES, SH, TH, and SO programs

UNION ALL

--	get service dates that indicate homelessness
SELECT DISTINCT V.ClientID, S.BeginDate, (CASE WHEN P.ProgramName IS NULL THEN O.Organization ELSE P.ProgramName END) AS ProgramName, 'Outreach Contact Service' AS EventType
FROM #Veterans V
INNER JOIN #TimeLimitedServices S ON V.ClientID = S.ClientID
LEFT OUTER JOIN dbo.ServiceCode SC ON S.ServiceCodeID = SC.ServiceCodeID AND SC.ActiveStatus <> 'D'
LEFT OUTER JOIN #TimeLimitedEnrollments E ON E.EnrollID = S.EnrollID
LEFT OUTER JOIN dbo.EnrollmentCase EC WITH (NOLOCK) ON E.CaseID = EC.CaseID AND EC.ActiveStatus <> 'D'
LEFT OUTER JOIN dbo.Programs P WITH (NOLOCK) ON EC.ProgramID = P.ProgramID AND P.ActiveStatus <> 'D'
LEFT OUTER JOIN dbo.osOrganization O WITH (NOLOCK) ON O.OrgID = S.OrgID AND O.ActiveStatus <> 'D'
WHERE SC.OutreachContact = 'true'								-- include all services marked as outreach contacts

UNION ALL

--	get current living situation dates that indicate homelessness
SELECT DISTINCT V.ClientID, CLS.LivingSituationDate, P.ProgramName, 'Literally Homeless CLS' AS EventType
FROM #Veterans V
INNER JOIN dbo.HMIS_LivingSituation CLS WITH (NOLOCK) ON V.ClientID = CLS.ClientID AND CLS.ActiveStatus <> 'D'
LEFT OUTER JOIN #TimeLimitedEnrollments E WITH (NOLOCK) ON CLS.EnrollID = E.EnrollID
LEFT OUTER JOIN EnrollmentCase EC WITH (NOLOCK) ON E.CaseID = EC.CaseID AND EC.ActiveStatus <> 'D'
LEFT OUTER JOIN dbo.Programs P WITH (NOLOCK) ON EC.ProgramID = P.ProgramID AND P.ActiveStatus <> 'D'
WHERE CLS.LivingSituationDate >= DATEADD(year, -3, SYSDATETIME())
	AND CLS.LivingSituation IN (1, 2, 16, 18)					-- include all current living situations in the last three years recorded as ES, SH, TH, or unsheltered homelessness


------------------------------------------------
--------  CREATE STATUS TABLE - HOUSED  --------
------------------------------------------------
--	get exit dates that indicate housed status, or the current date for clients housed in RRH/PSH/OPH programs
SELECT DISTINCT V.ClientID, (CASE WHEN E.ExitDate IS NULL THEN SYSDATETIME() ELSE E.ExitDate END) AS EffectiveDate, P.ProgramName,
	(CASE WHEN E.ExitDestination IN (3, 10, 11, 19, 20, 21, 22, 23, 26, 28, 29, 31, 32, 33, 34) THEN 'Housed Exit From Program' ELSE 'Still Housed in Program' END) AS EventType
INTO #HousedDates
FROM #Veterans V 
INNER JOIN #TimeLimitedEnrollments E ON E.ClientID = V.ClientID
LEFT OUTER JOIN dbo.EnrollmentRRH ER WITH (NOLOCK) ON ER.EnrollID = E.EnrollID
LEFT OUTER JOIN dbo.EnrollmentCase EC WITH (NOLOCK) ON E.CaseID = EC.CaseID AND EC.ActiveStatus <> 'D'
LEFT OUTER JOIN dbo.Programs P WITH (NOLOCK) ON EC.ProgramID = P.ProgramID AND P.ActiveStatus <> 'D'
WHERE E.ExitDestination IN (3, 10, 11, 19, 20, 21, 22, 23, 26, 28, 29, 31, 32, 33, 34)		-- include all exits to permanent destinations
	OR (P.ProgramType IN (3, 9, 10, 55)							-- include all open enrollments in housing programs with a HMID
		AND E.ExitDate IS NULL
		AND ER.DateOfMoveIn IS NOT NULL)	

UNION ALL

--	get housing move in dates for RRH/PSH/OPH programs
SELECT DISTINCT V.ClientID, ER.DateOfMoveIn, P.ProgramName, 'Housing Move-In Date' AS EventType
FROM #Veterans V 
INNER JOIN #TimeLimitedEnrollments E ON E.ClientID = V.ClientID
LEFT OUTER JOIN dbo.EnrollmentRRH ER WITH (NOLOCK) ON ER.EnrollID = E.EnrollID
LEFT OUTER JOIN dbo.EnrollmentCase EC WITH (NOLOCK) ON E.CaseID = EC.CaseID AND EC.ActiveStatus <> 'D'
LEFT OUTER JOIN dbo.Programs P WITH (NOLOCK) ON EC.ProgramID = P.ProgramID AND P.ActiveStatus <> 'D'
WHERE P.ProgramType IN (3, 9, 10, 55)							-- include all HMIDs for housing programs
		AND ER.DateOfMoveIn IS NOT NULL		

UNION ALL

--	get service dates for rental assistance and deposit assistance
SELECT DISTINCT V.ClientID, S.BeginDate, (CASE WHEN P.ProgramName IS NULL THEN O.Organization ELSE P.ProgramName END) AS ProgramName, 'Rent or Deposit Service' AS EventType
FROM #Veterans V
INNER JOIN #TimeLimitedServices S ON V.ClientID = S.ClientID
LEFT OUTER JOIN dbo.ServiceCode SC ON S.ServiceCodeID = SC.ServiceCodeID AND SC.ActiveStatus <> 'D'
LEFT OUTER JOIN #TimeLimitedEnrollments E ON E.EnrollID = S.EnrollID
LEFT OUTER JOIN dbo.EnrollmentCase EC WITH (NOLOCK) ON E.CaseID = EC.CaseID AND EC.ActiveStatus <> 'D'
LEFT OUTER JOIN dbo.Programs P WITH (NOLOCK) ON EC.ProgramID = P.ProgramID AND P.ActiveStatus <> 'D'
LEFT OUTER JOIN dbo.osOrganization O WITH (NOLOCK) ON O.OrgID = S.OrgID AND O.ActiveStatus <> 'D'
WHERE SC.HUDFinancialAssistanceType IN (1, 2)					-- include all services that are classified as HUD or SSVF rent or deposit assistance
	OR SC.SSVFFinancialAssistanceType IN (1, 2)

UNION ALL

--	get current living situation dates that indicate housed
SELECT DISTINCT V.ClientID, CLS.LivingSituationDate, P.ProgramName, 'Housed CLS' AS EventType
FROM #Veterans V
INNER JOIN dbo.HMIS_LivingSituation CLS WITH (NOLOCK) ON V.ClientID = CLS.ClientID AND CLS.ActiveStatus <> 'D'
LEFT OUTER JOIN #TimeLimitedEnrollments E WITH (NOLOCK) ON CLS.EnrollID = E.EnrollID
LEFT OUTER JOIN EnrollmentCase EC WITH (NOLOCK) ON E.CaseID = EC.CaseID AND EC.ActiveStatus <> 'D'
LEFT OUTER JOIN dbo.Programs P WITH (NOLOCK) ON EC.ProgramID = P.ProgramID AND P.ActiveStatus <> 'D'
WHERE CLS.LivingSituationDate >= DATEADD(year, -3, SYSDATETIME())
	AND CLS.LivingSituation IN (3, 10, 11, 14, 19, 20, 21, 28, 29, 31, 32, 33, 34, 35, 36)					-- include all current living situations in the last three years recorded as a housed situation


------------------------------------------------
--------  JOIN AND LABEL STATUS TABLES  --------
------------------------------------------------
SELECT DISTINCT ClientID, CAST (EffectiveDate AS DATE) AS EffectiveDate, 'Homeless' AS ClientStatus, ProgramName, EventType
--INTO #FullStatusAndDateTable
FROM #HomelessDates
WHERE EffectiveDate >= DATEADD(year, -3, SYSDATETIME())
UNION ALL
SELECT DISTINCT ClientID, CAST (EffectiveDate AS DATE) AS EffectiveDate, 'Housed' AS ClientStatus, ProgramName, EventType
FROM #HousedDates
WHERE EffectiveDate >= DATEADD(year, -3, SYSDATETIME())
ORDER BY ClientID, EffectiveDate DESC, ClientStatus DESC

--select * from #FullStatusAndDateTable


-- this gets the full table and the number of days since the last event (check in excel), but is slow
SELECT  ClientID, EffectiveDate, DATEDIFF(DAY, PrevDate, EffectiveDate) AS DaysSinceLastEvent, ClientStatus, ProgramName, EventType
FROM (
	SELECT  ClientID, EffectiveDate, ClientStatus, ProgramName, EventType,
		(SELECT MAX(EffectiveDate)
		FROM (
			SELECT DISTINCT ClientID, CAST (EffectiveDate AS DATE) AS EffectiveDate, 'Homeless' AS ClientStatus, ProgramName, EventType
			FROM #HomelessDates
			WHERE EffectiveDate >= DATEADD(year, -3, SYSDATETIME())
			UNION ALL
			SELECT DISTINCT ClientID, CAST (EffectiveDate AS DATE) AS EffectiveDate, 'Housed' AS ClientStatus, ProgramName, EventType
			FROM #HousedDates
			WHERE EffectiveDate >= DATEADD(year, -3, SYSDATETIME())) T2
		WHERE T2.ClientID = T1.ClientID
			AND T2.EffectiveDate < T1.EffectiveDate
        ) AS PrevDate
	FROM (
		SELECT DISTINCT ClientID, CAST (EffectiveDate AS DATE) AS EffectiveDate, 'Homeless' AS ClientStatus, ProgramName, EventType
		FROM #HomelessDates
		WHERE EffectiveDate >= DATEADD(year, -3, SYSDATETIME())
		UNION ALL
		SELECT DISTINCT ClientID, CAST (EffectiveDate AS DATE) AS EffectiveDate, 'Housed' AS ClientStatus, ProgramName, EventType
		FROM #HousedDates
		WHERE EffectiveDate >= DATEADD(year, -3, SYSDATETIME())) T1
	) AS T
ORDER BY ClientID, EffectiveDate DESC, ClientStatus DESC




------------------------------------------------
------------  CREATE SUMMARY TABLE  ------------
------------------------------------------------
--SELECT C.ClientID, Recent.RecentStatus,
--	COUNT(CASE WHEN C.EffectiveDate >= DATEADD(month, -3, SYSDATETIME()) AND C.ClientStatus = 'Homeless' THEN C.EffectiveDate END) AS RecentHomelessEvents,
--	COUNT(CASE WHEN C.EffectiveDate >= DATEADD(month, -3, SYSDATETIME()) AND C.ClientStatus = 'Housed' THEN C.EffectiveDate END) AS RecentHousedEvents,
--	COUNT(CASE WHEN C.EffectiveDate < DATEADD(month, -3, SYSDATETIME()) AND C.ClientStatus = 'Homeless' THEN C.EffectiveDate END) AS OldHomelessEvents,
--	COUNT(CASE WHEN C.EffectiveDate < DATEADD(month, -3, SYSDATETIME()) AND C.ClientStatus = 'Housed' THEN C.EffectiveDate END) AS OldHousedEvents,
--	(CASE 
--		WHEN Recent.RecentStatus = 'Housed' THEN 'Housed'
--		WHEN COUNT(CASE WHEN C.EffectiveDate >= DATEADD(month, -3, SYSDATETIME()) AND C.ClientStatus = 'Homeless' THEN C.EffectiveDate END) > 0
--			AND COUNT(CASE WHEN C.EffectiveDate < DATEADD(month, -3, SYSDATETIME()) AND C.ClientStatus = 'Homeless' THEN C.EffectiveDate END) = 0 
--			AND COUNT(CASE WHEN C.ClientStatus = 'Housed' THEN C.EffectiveDate END) = 0 
--			THEN 'Newly Homeless'
--		WHEN Recent.RecentStatus = 'Homeless' 
--			AND COUNT(CASE WHEN C.EffectiveDate >= DATEADD(month, -3, SYSDATETIME()) AND C.ClientStatus = 'Homeless' THEN C.EffectiveDate END) = 0
--			THEN 'Inactive'
--		ELSE '-' END) AS CurrentStatus
--FROM #FullStatusAndDateTable C
--LEFT OUTER JOIN (SELECT ClientID, ClientStatus AS RecentStatus
--    FROM (
--        SELECT ClientID, ClientStatus, RANK() 
--			OVER (PARTITION BY ClientID
--                ORDER BY ClientID, EffectiveDate DESC, ClientStatus DESC) AS Rank
--        FROM #FullStatusAndDateTable
--        ) RS WHERE Rank = 1) Recent ON C.ClientID = Recent.ClientID
--GROUP BY C.ClientID, Recent.RecentStatus

------------------------------------------------
--------------  DROP TEMP TABLES  --------------
------------------------------------------------
IF OBJECT_ID('tempdb.dbo.#Veterans') IS NOT NULL DROP TABLE #Veterans;
IF OBJECT_ID('tempdb.dbo.#TimeLimitedEnrollments') IS NOT NULL DROP TABLE #TimeLimitedEnrollments;
IF OBJECT_ID('tempdb.dbo.#TimeLimitedServices') IS NOT NULL DROP TABLE #TimeLimitedServices;

IF OBJECT_ID('tempdb.dbo.#HomelessDates') IS NOT NULL DROP TABLE #HomelessDates;
IF OBJECT_ID('tempdb.dbo.#HousedDates') IS NOT NULL DROP TABLE #HousedDates;
IF OBJECT_ID('tempdb.dbo.#FullStatusAndDateTable') IS NOT NULL DROP TABLE #FullStatusAndDateTable;
