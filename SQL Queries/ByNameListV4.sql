--==============================================
----------------  BY NAME LIST  ----------------
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
			Started status determination logic:
				- housed = most recent status is housed
				- newly homeless = only homeless events, none older than three months old (event date >= three months ago)
				- inactive = most recent status is homeless, but it is more than three months ago (event date < three months ago)
				- return from housed = ?
				- active = ?
				- return from inactive = ?
			First draft of logic for getting days since last event

3.2.21 -	Updated all 'three month' logic to use '90 days' instead
			Added identification date to summary table
				- identification date = most recent homeless event date with no homeless events in the 90 days preceding it
			Completed first draft of current status logic:
				- housed = most recent status is housed
				- newly homeless = only homeless events, none older than 90 days old (event date >= 90 days ago)
				- inactive = most recent status is homeless, but it is more than 90 days old (event date < 90 days ago)
				- return from housed = at least one homeless event in the last 90 days that is immediately preceded by a housed event
				- return from inactive = identification date is in the last 90 days, but the client has at least one homeless event more than 90 days old
				- active = all other situations
			Switched from temporary tables to CTEs to allow for view creation
4.19.21	-	Need to speed this up--summary table currently takes 10 seconds to run locally and 1.5 minutes in ClientTrack
				- changed housed check and ID date joins to use lag
4.20.21	-	Continued optimization, got run time down to 3 seconds when run locally
5.27.21 -	Combined select statements for services, CLSs, and exits per Greg's suggestion
7.2.21	-	Reduced lookback to one year instead of three to increase speed, per discussion with Matt H
7.19.21	-	Removed veteran restriction after beta testing in live site and exclude CES contacts from the outreach service check
8.20.21	-	Changed "Newly Homeless" to "New to List" and removed balance of state programs from enrollments
*/

USE Indy;
GO


CREATE OR ALTER VIEW [dbo].[Custom_VW_StatusTable]
AS

------------------------------------------------
----------  CREATE BASE TEMP TABLES  -----------
------------------------------------------------
-- create the Clients CTE
WITH Clients AS
(SELECT ClientID
FROM dbo.cmClient WITH (NOLOCK)
WHERE ActiveStatus = 'A'),

-- create the TimeLimitedEnrollments CTE
TimeLimitedEnrollments AS
(SELECT EnrollID, ClientID, CaseID, EnrollDate, EnrollAssessmentID, ExitDate, ExitDestination
FROM dbo.Enrollment WITH (NOLOCK)
WHERE ActiveStatus = 'A'
	AND EnrollDate <= SYSDATETIME()
	AND (ExitDate IS NULL 
		OR ExitDate >= DATEADD(year, -1, SYSDATETIME()))),

-- create the TimeLimitedServices CTE
TimeLimitedServices AS
(SELECT ClientID, OrgID, EnrollID, BeginDate, ServiceCodeID
FROM dbo.Service WITH (NOLOCK)
WHERE ActiveStatus = 'A'
	AND BeginDate <= SYSDATETIME()
	AND (EndDate IS NULL 
		OR EndDate >= DATEADD(year, -1, SYSDATETIME())))


------------------------------------------------
----  ADD HOMELESS EVENTS TO STATUS TABLE  -----
------------------------------------------------

--	get enrollment dates that indicate homelessness
SELECT DISTINCT C.ClientID, CAST (E.EnrollDate AS DATE) AS EffectiveDate, 'Homeless' AS ClientStatus, P.ProgramName, 'Literally Homeless Enrollment' AS EventType
FROM Clients C 
INNER JOIN TimeLimitedEnrollments E ON E.ClientID = C.ClientID
LEFT OUTER JOIN dbo.EnrollmentCase EC WITH (NOLOCK) ON E.CaseID = EC.CaseID AND EC.ActiveStatus = 'A'
LEFT OUTER JOIN dbo.Programs P WITH (NOLOCK) ON EC.ProgramID = P.ProgramID AND P.ActiveStatus = 'A'
LEFT OUTER JOIN dbo.HMISDataAssessment A WITH (NOLOCK) ON A.AssessmentID = E.EnrollAssessmentID AND A.ActiveStatus = 'A'
LEFT OUTER JOIN dbo.DomesticViolenceAssessment DV WITH (NOLOCK) ON DV.AssessmentID = E.EnrollAssessmentID AND DV.ActiveStatus = 'A'
WHERE E.EnrollDate >= DATEADD(year, -1, SYSDATETIME())												-- only include enrollments in the last year
	AND (A.PriorResidence IN (1, 2, 16, 18)															-- include all enrollments with a prior residence of ES, SH, TH, or unsheltered homelessness
		OR (DV.DomViolenceExp = 1																	-- include all enrollments with DV history and currently fleeing
			AND DV.CurrentlyFleeing = 1)		
		OR P.ProgramType IN (1, 2, 4, 8))															-- include all enrollments to ES, SH, TH, and SO programs

------------------------------------------------
---------  ADD HMIDs TO STATUS TABLE  ----------
------------------------------------------------

UNION ALL

--	get housing move in dates for RRH/PSH/OPH programs
SELECT DISTINCT C.ClientID, CAST(ER.DateOfMoveIn AS DATE), 'Housed' AS ClientStatus, P.ProgramName, 'Housing Move-In Date' AS EventType
FROM Clients C 
INNER JOIN TimeLimitedEnrollments E ON E.ClientID = C.ClientID
LEFT OUTER JOIN dbo.EnrollmentRRH ER WITH (NOLOCK) ON ER.EnrollID = E.EnrollID
LEFT OUTER JOIN dbo.EnrollmentCase EC WITH (NOLOCK) ON E.CaseID = EC.CaseID AND EC.ActiveStatus = 'A'
LEFT OUTER JOIN dbo.Programs P WITH (NOLOCK) ON EC.ProgramID = P.ProgramID AND P.ActiveStatus = 'A'
WHERE P.ProgramType IN (3, 9, 10, 55)																-- include all HMIDs for housing programs in the last year
		AND ER.DateOfMoveIn >= DATEADD(year, -1, SYSDATETIME())		

------------------------------------------------
-----  ADD SERVICE EVENTS TO STATUS TABLE  -----
------------------------------------------------

UNION ALL

--	get service dates for outreach contacts (homelessness), rental assistance, or deposit assistance (housed)
SELECT DISTINCT C.ClientID, CAST(S.BeginDate AS DATE), 
	(CASE WHEN SC.OutreachContact = 'true' THEN 'Homeless' ELSE 'Housed' END) AS ClientStatus, 
	(CASE WHEN P.ProgramName IS NULL THEN O.Organization ELSE P.ProgramName END) AS ProgramName,
	(CASE WHEN SC.OutreachContact = 'true' THEN 'Outreach Contact Service' ELSE 'Rent or Deposit Service' END) AS EventType
FROM Clients C
INNER JOIN TimeLimitedServices S ON C.ClientID = S.ClientID
LEFT OUTER JOIN dbo.ServiceCode SC ON S.ServiceCodeID = SC.ServiceCodeID AND SC.ActiveStatus = 'A'
LEFT OUTER JOIN TimeLimitedEnrollments E ON E.EnrollID = S.EnrollID
LEFT OUTER JOIN dbo.EnrollmentCase EC WITH (NOLOCK) ON E.CaseID = EC.CaseID AND EC.ActiveStatus = 'A'
LEFT OUTER JOIN dbo.Programs P WITH (NOLOCK) ON EC.ProgramID = P.ProgramID AND P.ActiveStatus = 'A'
LEFT OUTER JOIN dbo.osOrganization O WITH (NOLOCK) ON O.OrgID = S.OrgID AND O.ActiveStatus = 'A'
WHERE (SC.HUDFinancialAssistanceType IN (1, 2)														-- include all services that are classified as HUD or SSVF rent or deposit assistance
		OR SC.SSVFFinancialAssistanceType IN (1, 2))
	OR (SC.OutreachContact = 'true'																	-- include all services marked as outreach contacts
		AND SC.ServiceCodeID <> 2118)																	-- exclude CES contacts

------------------------------------------------
----------  ADD CLSs TO STATUS TABLE  ----------
------------------------------------------------

UNION ALL

--	get current living situation dates that indicate housed or homelessness
SELECT DISTINCT C.ClientID, CAST(CLS.LivingSituationDate AS DATE), 
	(CASE WHEN CLS.LivingSituation IN (1, 2, 16, 18) THEN 'Homeless' ELSE 'Housed' END) AS ClientStatus, P.ProgramName, 
	(CASE WHEN CLS.LivingSituation IN (1, 2, 16, 18) THEN 'Literally Homeless CLS' ELSE 'Housed CLS' END) AS EventType
FROM Clients C
INNER JOIN dbo.HMIS_LivingSituation CLS WITH (NOLOCK) ON C.ClientID = CLS.ClientID AND CLS.ActiveStatus = 'A'
LEFT OUTER JOIN TimeLimitedEnrollments E WITH (NOLOCK) ON CLS.EnrollID = E.EnrollID
LEFT OUTER JOIN dbo.EnrollmentCase EC WITH (NOLOCK) ON E.CaseID = EC.CaseID AND EC.ActiveStatus = 'A'
LEFT OUTER JOIN dbo.Programs P WITH (NOLOCK) ON EC.ProgramID = P.ProgramID AND P.ActiveStatus = 'A'
WHERE CLS.LivingSituationDate >= DATEADD(year, -1, SYSDATETIME())
	AND CLS.LivingSituation IN (3, 10, 11, 14, 19, 20, 21, 28, 29, 31, 32, 33, 34, 35, 36,			-- include all current living situations in the last year recorded as a housed situation
		1, 2, 16, 18)																				-- include all current living situations in the last year recorded as ES, SH, TH, or unsheltered homelessness

------------------------------------------------
----  ADD EXITS/RESIDENCES TO STATUS TABLE  ----
------------------------------------------------

UNION ALL

--	get exit dates that indicate homelessness, or the current date for clients in ES/SH/TH
SELECT DISTINCT C.ClientID, CAST ((CASE WHEN E.ExitDate IS NULL THEN SYSDATETIME() ELSE E.ExitDate END) AS DATE), 
	(CASE WHEN E.ExitDestination IN (1, 2, 16, 18) 
		OR (P.ProgramType IN (1, 2, 4, 8) AND E.ExitDate IS NULL) THEN 'Homeless' ELSE 'Housed' END) AS ClientStatus, P.ProgramName,
	(CASE WHEN E.ExitDestination IN (1, 2, 16, 18) THEN 'Homeless Exit From Program' 
		WHEN E.ExitDestination IN (3, 10, 11, 19, 20, 21, 22, 23, 26, 28, 29, 31, 32, 33, 34) THEN 'Housed Exit From Program'
	ELSE 'Still Enrolled in Program' END) AS EventType
FROM Clients C 
INNER JOIN TimeLimitedEnrollments E ON E.ClientID = C.ClientID
LEFT OUTER JOIN dbo.EnrollmentRRH ER WITH (NOLOCK) ON ER.EnrollID = E.EnrollID
LEFT OUTER JOIN dbo.EnrollmentCase EC WITH (NOLOCK) ON E.CaseID = EC.CaseID AND EC.ActiveStatus = 'A'
LEFT OUTER JOIN dbo.Programs P WITH (NOLOCK) ON EC.ProgramID = P.ProgramID AND P.ActiveStatus = 'A'
WHERE E.ExitDestination IN (1, 2, 16, 18,															-- include all exits to ES, SH, TH, or unsheltered homelessness
		3, 10, 11, 19, 20, 21, 22, 23, 26, 28, 29, 31, 32, 33, 34)									-- include all exits to permanent destinations
	OR (ExitDate IS NULL
		AND (P.ProgramType IN (1, 2, 4, 8)															-- include all open enrollments in ES, SH, and TH programs
			OR
			(P.ProgramType IN (3, 9, 10, 55)														-- include all open enrollments in housing programs with a HMID
				AND ER.DateOfMoveIn IS NOT NULL)))	


GO

CREATE OR ALTER VIEW [dbo].[Custom_VW_ByNameList]
AS
------------------------------------------------
------------  CREATE SUMMARY TABLE  ------------
------------------------------------------------
SELECT C.ClientID, IdDATE.IdentificationDate, Recent.RecentStatus, 
	(CASE 
		WHEN Recent.RecentStatus = 'Housed' THEN 'Housed'										-- housed = most recent status was housed
		WHEN COUNT(CASE WHEN C.EffectiveDate >= DATEADD(day, -90, SYSDATETIME()) AND C.ClientStatus = 'Homeless' THEN C.EffectiveDate END) > 0
			AND COUNT(CASE WHEN C.EffectiveDate < DATEADD(day, -90, SYSDATETIME()) AND C.ClientStatus = 'Homeless' THEN C.EffectiveDate END) = 0 
			AND COUNT(CASE WHEN C.ClientStatus = 'Housed' THEN C.EffectiveDate END) = 0 
			THEN 'Newly Homeless'																-- newly homeless = homeless in last 90 days but not before, and never housed
		WHEN Recent.RecentStatus = 'Homeless' 
			AND COUNT(CASE WHEN C.EffectiveDate >= DATEADD(day, -90, SYSDATETIME()) THEN C.EffectiveDate END) = 0
			THEN 'Inactive'																		-- inactive = most recent status was homeless but no events in last 90 days
		WHEN R.ReturnFlag = 1 THEN 'Return From Housed'											-- return from housed = at least one homeless event in the last 90 days was immediately preceded by a housed event
		WHEN IdDATE.IdentificationDate >= DATEADD(day, -90, SYSDATETIME())
			AND COUNT(CASE WHEN C.EffectiveDate < DATEADD(day, -90, SYSDATETIME()) AND C.ClientStatus = 'Homeless' THEN C.EffectiveDate END) > 0
			THEN 'Return From Inactive'															-- return from housed = no homeless events in the last 90 days preceded by a homeless event in 90 days, but not newly homeless
		ELSE 'Active' END) AS CurrentStatus														-- else active
FROM [dbo].[Custom_VW_StatusTable] C
-- this join gets the most recent status for each client
LEFT OUTER JOIN (SELECT ClientID, ClientStatus AS RecentStatus									
    FROM (SELECT ClientID, ClientStatus, RANK() 
			OVER (PARTITION BY ClientID
                ORDER BY EffectiveDate DESC, ClientStatus DESC) AS Rank
        FROM [dbo].[Custom_VW_StatusTable]) RS 
	WHERE Rank = 1) Recent ON C.ClientID = Recent.ClientID
-- this join gets the maximum potential ID date (the start date associated with the most recent period of homelessness)
LEFT OUTER JOIN
	(SELECT ClientID, MAX(EffectiveDate) AS IdentificationDate
	FROM (
		SELECT ClientID, EffectiveDate, ClientStatus,
			LAG(EffectiveDate, 1) OVER (PARTITION BY ClientID ORDER BY EffectiveDate) LastDate
		FROM [Custom_VW_StatusTable]
		WHERE ClientStatus = 'Homeless'
		) C
	WHERE LastDate IS NULL OR DATEDIFF(DAY, LastDate, EffectiveDate) >= 90						-- potential dates are homeless dates occuring at least 90 days after the preceding homeless date or with none preceding
	GROUP BY ClientID) IdDate ON IdDATE.ClientID = C.ClientID
-- this join checks whether any homeless events in the last 90 days were immediately preceded by a housed event
LEFT OUTER JOIN 
	(SELECT DISTINCT ClientID, 1 AS ReturnFlag													-- if any of the qualifying events were preceded by a housed event, create a flag on that client
	FROM (
		SELECT ClientID, EffectiveDate, ClientStatus,
			LAG(ClientStatus, 1) OVER (PARTITION BY ClientID ORDER BY EffectiveDate) LastStatus
		FROM [Custom_VW_StatusTable]
		) C
	WHERE ClientStatus = 'Homeless'
		AND EffectiveDate >= DATEADD(day, -90, SYSDATETIME())									-- for the return to housed check, we only need to look before homeless events in the last 90 days
		AND LastStatus = 'Housed') R ON R.ClientID = C.ClientID
GROUP BY C.ClientID, Recent.RecentStatus, IdDATE.IdentificationDate, R.ReturnFlag

GO