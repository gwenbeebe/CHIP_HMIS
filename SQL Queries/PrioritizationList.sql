/****** Object:  View [dbo].[Custom_VW_PrioritizationList]    Script Date: 12/18/2019 12:32:34 AM ******/
--SET ANSI_NULLS ON
--GO

--SET QUOTED_IDENTIFIER ON
--GO








--ALTER VIEW [dbo].[Custom_VW_PrioritizationList]
--AS
/*-----------------------------------------------------------------------------------------------------------------------------

*** CHIP ***

Prioritization List View.
@WorkSolutionsInc. 2016
T.S. - 07/14/2016 - Initial creation
T.S. - 08/09/2016 - Added EnrollID
T.S. - 09/29/2016 - Added Relationship to HoH, Gender, No. of Case Members, No. Children
T.S. - 11/18/2016 - Modified where clause, added ProgramID to results.
SWN  - 03/24/2017 - Added LastReferral ID, date, provider name, and referral result columns.
SWN  - 04/14/2017 - Added CT_PrioritizationList table, and cmClient.VeteranStatus, and "Referred" column.
SWN  - 05/17/2017 - (CHIP) Added HIV flag (1=Yes, 0=No), DomViolenceExp value from DomesticViolenceAssessment, 
				and CEAssessmentDate and CEInterventions from CT_CEAssessment.
				CEInterventions is a list of 0-11 letters, each one mapped to a chck box column from the assessment;
				a picklist with the letters and corresponding firled descriptions can be used with "Contains" operator
				to select clients with the corresponding box checked.
SWN - 06/01/2017 -	(CHIP) Added CEAssessment.ADAApproved, CEAssessment.NoBedrooms, CEAssessment.SexOffender, CEAssessment.Arson;
				modified DaysHomelessGrouping calculation.
SWN - 06/16/2017 -	(CHIP, IHCDA) Modified VISPDAT to retrieve the most recent assessment for the client 
				(based on VulnerabilityIndex.Assessmentdate), instead of the initial assessment linked to the enrollment.
				Added LastVISPDAT [AssessmentDate] and DaysSinceLastVISPDAT [as of today's date] as columns.
SWN - 10/17/2017 -	(CHIP) Added HMISAssessment table data (HMISAssessmentDate and ContinuouslyHomelessType view columns),
				and added VOAIntervention (L) to the CEA.CEInterventions list.
SWN - 11/09/2017 -	(CHIP) Added HMIS.DisablingCondition, HMIS.HMISTimesHomelessLast3Years.
SWN - 01/02/2018 -	(CHIP) Added new columns from CE Assessment.
SWN - 05/15/2018 -	(CHIP) Added Navigator (EnrollmentCase.CreatedBy), and 18-24 flag.
SWN - 10/05/2018 -	(CHIP) Added CEAAssessment.AssessmentID, .DVHomelessDate, and .DVLethalityScore.
SWN - 02/09/2018 -  Added Birthdate column.
T.S. - 03/14/2019 - Added first & last name columns.
T.S. - 06/20/2019 - Added a new intervention value.
T.S. - 10/25/2019 - Added latest voucher info.
T.S. - 04/17/2020 - Added "CH Status"
T.S. - 10/16/2020 - Added column "PrioritizationListStatus" for new filter & calculated 
				column "DaysLastCLS"
T.S. - 11/25/2020 - Change join of HMIS_LivingSituation table to use ClientID instead of EnrollID
	Change join type from left join to left outer join on status value join for combo value
	Change join type from left join to left outer join on living situation join
	Change casemembers subquery per Chip
GB	2.24.21 - added yes/no column to flag whether the client has an open housing referral

NOTE: The custom view dbo.CUSTOM_vw_LastReferral, and the custom table dbo.CT_CEAssessment, MUST exist in the database.
--------------------------------------------------------------------------------------------------------------------------------*/

SELECT 
	C.ClientID,	C.Birthdate, E.EnrollID, E.OrgID, EC.ProgramID, E.CaseID,
	C.Name,	C.FirstName, C.LastName, C.Gender, C.VeteranStatus, 
	P.[ProgramName], E.[EnrollDate], E.[ExitDate], 
	V.[ScoreTotal] AS [VISPDATScore], 
	A.[HomelessStartDate] AS [HomelessStartDate], DATEDIFF(Day,A.[HomelessStartDate], GETDATE()) AS [DaysHomeless],

	CASE	WHEN DATEDIFF(YY,A.[HomelessStartDate], GETDATE()) >= 5 THEN '5 or more Years'
			WHEN DATEDIFF(YY,A.[HomelessStartDate], GETDATE()) BETWEEN 3 AND 4 THEN '3-4 Years'
			WHEN DATEDIFF(Day,A.[HomelessStartDate], GETDATE()) > 365 THEN '1-2 Years'
			WHEN DATEDIFF(Day,A.[HomelessStartDate], GETDATE()) < 365 THEN 'Under 12 months' 
			ELSE '' END AS [DaysHomelessGrouping],
			
	CASE	WHEN V.[ScoreTotal] >= 0 AND V.[ScoreTotal] <= 3 THEN '0-3'
			WHEN V.[ScoreTotal] >= 4 AND V.[ScoreTotal] <= 7 THEN '4-7'
			WHEN V.[ScoreTotal] >= 8  THEN '8+'
			--WHEN V.[ScoreTotal] >= 10 THEN '10+'
			ELSE '' END AS [VISPDATScoreGrouping],

	CASE	WHEN DATEDIFF(Day,A.[HomelessStartDate], GETDATE()) > 365 THEN '2'
			WHEN DATEDIFF(Day,A.[HomelessStartDate], GETDATE()) < 365 THEN '1' 
			ELSE '' END	AS [DaysHomelessGroupingCode],

	CASE	WHEN V.[ScoreTotal] >= 0 AND V.[ScoreTotal] <= 3 THEN '1'
			WHEN V.[ScoreTotal] >= 4 AND V.[ScoreTotal] <= 7 THEN '2'
			WHEN V.[ScoreTotal] >= 8 AND V.[ScoreTotal] <= 9 THEN '3'
			WHEN V.[ScoreTotal] >= 10 THEN '4'
			ELSE '' END AS [VISPDATScoreGroupCode],
	V.AssessmentDate AS LastVISPDAT, DATEDIFF(DD, V.AssessmentDate, GETDATE()) AS DaysSinceLastVISPDAT,
	RelationshipVal.[ItemDesc] AS [RelationshiptoHoH],

	ISNULL(CB_CH.ItemDesc,'No') AS [CHStatus],--04/17/2020
	
	--11/25/2020 - Update per Gwen Beebe
	( SELECT COUNT(*)
		FROM [dbo].[Enrollment] 
		WHERE [dbo].[Enrollment].[CaseID] = E.[CaseID] 
			AND [dbo].[Enrollment].[ActiveStatus] = 'A'
			AND [dbo].[Enrollment].[ExitDate] IS NULL --11/25/2020
		GROUP BY [dbo].[Enrollment].[CaseID] ) AS [NoCaseMembers],

	ISNULL( ( SELECT COUNT(*)
		FROM [dbo].[Enrollment]
		LEFT OUTER JOIN [dbo].[ClientCalculations] ON [dbo].[ClientCalculations].[ClientID] = [dbo].[Enrollment].[ClientID]
		WHERE [dbo].[Enrollment].[CaseID] = E.[CaseID]
			AND [dbo].[ClientCalculations].[Age] < 18
		GROUP BY [dbo].[Enrollment].[CaseID] ), 0 ) AS [NoChildren],
	
	CASE WHEN EXISTS
		( SELECT * FROM [dbo].[Enrollment]
			INNER JOIN [dbo].[ClientCalculations] ON [dbo].[ClientCalculations].[ClientID] = [dbo].[Enrollment].[ClientID]
			WHERE [dbo].[Enrollment].[CaseID] = E.[CaseID] AND [dbo].[ClientCalculations].[Age] < 18 )
		THEN 1 ELSE 0 END AS [HasChildren],

	R.ServiceID AS ReferralServiceID, R.BeginDate As ReferralDate, R.ToProvider, R.ResultDesc As ReferralResult,
	CASE WHEN R.ServiceID IS NOT NULL THEN 1 ELSE 0 END AS Referred,	
	CASE WHEN 
		EXISTS(SELECT * FROM [dbo].[cmClntEval] CE INNER JOIN [dbo].[AssessmentEval] AE ON AE.EvalID = CE.EvalID
					WHERE CE.EvalCode = 'BRHMSHA' and CE.[status] = 1 AND CE.ActiveStatus='A' AND AE.AssessmentID = E.EnrollAssessmentID)
	THEN 1 ELSE 0 END AS HIV,
	DVA.DomViolenceExp, CEA.AssessmentID AS CEAssessmentID, CEA.AssessmentDate AS CEAssessmentDate, 
	CEA.CEInterventions, CEA.ADAApproved, CEA.NoBedrooms, CEA.SexOffender, CEA.Arson,
	HMIS.AssessmentDate AS HMISAssessmentDate, HMIS.ContinuouslyHomelessType,
	HMIS.DisablingCondition, HMIS.HMISTimesHomelessLast3Years,
	CEA.GPDType, CEA.SaveHavenType, CEA.DVViolence,		-- new columns added 1/2/18
	CEA.DVHomelessDate, CEA.DVLethalityScore,			-- added 10/5/18
	EC.CreatedBy AS NavigatorID, Nav.UserName AS NavigatorName,
	cc.Age, CASE WHEN CC.Age BETWEEN 18 AND 24 THEN 1 ELSE 0 END AS Age18to24,
	VASH.[VoucherTrackingInfoDate] AS DateofMostRecentHUDVASHStatus,
	VASH.[ItemDesc] AS MostRecentHUDVASHStatus,
	AL.PrioritizationStatus,PriorStatusVal.ItemDesc AS [PrioritizationListStatus],
	DATEDIFF(Day,LS.LivingSituationDate, GETDATE()) AS [DaysLastCLS], --Days Since Last Current Living Situation
	(CASE WHEN (SR.Result = 10) OR (SR.ServiceID IS NOT NULL AND SR.Result IS NULL) THEN 'Yes' ELSE 'No' END) AS OpenHousingReferral	-- 2.24.21
	
FROM 
	[dbo].[cmClient] C WITH (NOLOCK) 
	INNER JOIN [dbo].[ClientCalculations] CC ON CC.ClientID = C.ClientID 
	INNER JOIN [dbo].[Enrollment] E WITH (NOLOCK) ON C.[ClientID] = E.[ClientID] AND E.[ActiveStatus] <> 'D' 
	INNER JOIN [dbo].[EnrollmentCase] EC WITH (NOLOCK) ON E.[CaseID] = EC.[CaseID] AND EC.[ActiveStatus] <> 'D' 
	INNER JOIN [dbo].[osUsers] Nav WITH (NOLOCK) ON Nav.UserID = EC.CreatedBy 
	INNER JOIN [dbo].[Programs] P WITH (NOLOCK) ON EC.[ProgramID] = P.[ProgramID] AND P.[ActiveStatus] <> 'D'
	LEFT OUTER JOIN [dbo].[HmisDataAssessment] A WITH (NOLOCK) ON E.[EnrollAssessmentID] = A.[AssessmentID] AND A.[ActiveStatus] <> 'D'
	
	LEFT OUTER JOIN			
	-- 6/14/17 -- Instead of getting VISPDAT from the enrollment, retrieve the latest assessment for the client
	( SELECT LastVI.ClientID, VI2.AssessmentDate, VI2.AssessmentID, V.* FROM VISPDAT V (NOLOCK) 
		INNER JOIN
		( SELECT VI.ClientID, 
			(	SELECT TOP 1 VIA.VulnerabilityID FROM VulnerabilityIndex VIA (NOLOCK) 
					-- 2/9/18 - VI records are 'deleted' by soft-deleting the corresponding Assessment record
					INNER JOIN Assessment A (NOLOCK) ON A.AssessmentID = VIA.AssessmentID
				WHERE VIA.ClientID = VI.ClientID AND VIA.ActiveStatus<>'D' AND A.ActiveStatus<>'D' 
				ORDER BY VIA.AssessmentDate DESC, VulnerabilityID ) AS VID
		FROM VulnerabilityIndex VI (NOLOCK) GROUP BY VI.ClientID	
		) LastVI ON LastVI.VID = V.VulnerabilityID
			INNER JOIN VulnerabilityIndex (NOLOCK) VI2 ON VI2.VulnerabilityID = LastVI.VID			 
	) V ON V.ClientID = C.ClientID

	--Fixed 12/18/2019
	LEFT OUTER JOIN			
	( SELECT LastVI.ClientID, VI2.VoucherTrackingInfoDate,method.ItemDesc FROM VASH_VoucherTracking VASH (NOLOCK) 
		INNER JOIN
		( SELECT VI.ClientID,
			(	SELECT TOP 1 VIA.VoucherTrackingID 
				FROM VASH_VoucherTracking VIA (NOLOCK) 
				WHERE VIA.ClientID = VI.ClientID AND VIA.ActiveStatus<>'D'  
				ORDER BY VIA.VoucherTrackingInfoDate DESC, VoucherTrackingID ) AS VID
		FROM VASH_VoucherTracking VI (NOLOCK) GROUP BY VI.ClientID	
		) LastVI ON LastVI.VID = VASH.VoucherTrackingID
			INNER JOIN VASH_VoucherTracking (NOLOCK) VI2 ON VI2.VoucherTrackingID = LastVI.VID	
			INNER JOIN [dbo].[cmComboBoxItem] method (NOLOCK) 
				ON VASH.[VoucherTrackingMethod] = method.[Item]
				AND method.[ComboBox] = 'Voucher Tracking Mathod' AND method.[ComboboxGrp] = 'VoucherTracking'
				AND method.[ActiveStatus] <> 'D'
	) VASH ON VASH.ClientID = C.ClientID

	LEFT OUTER JOIN cmComboBoxItem				RelationshipVal WITH (NOLOCK) 
		ON E.[Relationship] = RelationshipVal.[Item] 
			AND ( [RelationshipVal].[Combobox] = 'relationship' AND [RelationshipVal].[ComboboxGrp] = 'CMFML' ) 
			AND RelationshipVal.ActiveStatus <> 'D'
	LEFT OUTER JOIN [dbo].[CUSTOM_vw_LastReferral]	R ON R.EnrollID = E.EnrollID

	--2.24.21
--	LEFT OUTER JOIN [dbo].[Service]

	LEFT OUTER JOIN [dbo].[DomesticViolenceAssessment] DVA (NOLOCK) ON DVA.AssessmentID = E.EnrollAssessmentID
	LEFT OUTER JOIN [dbo].[HmisDataAssessment] HMIS (NOLOCK) ON HMIS.AssessmentID = E.EnrollAssessmentID
	
	--10/16/2020
	--V.AssessmentID
	LEFT OUTER JOIN CEAssessmentLink AL (NOLOCK) ON AL.AssessmentID = V.AssessmentID
	LEFT OUTER JOIN dbo.ComboboxList('PrioritizationStatus','TriageAssessment') AS PriorStatusVal ON AL.PrioritizationStatus = PriorStatusVal.Item

	--11/25/2020 - Get latest living situation for client for days since calculation
	LEFT OUTER JOIN [dbo].HMIS_LivingSituation LS WITH (NOLOCK) ON C.ClientID = LS.ClientID 
		AND LS.[LivingSituationID]	= 
			(		SELECT TOP 1 [dbo].[HMIS_LivingSituation].[LivingSituationID]
				FROM [dbo].[HMIS_LivingSituation] WITH (NOLOCK)
				WHERE C.[ClientID] = [dbo].[HMIS_LivingSituation].[ClientID]
					AND (  [dbo].[HMIS_LivingSituation].[ActiveStatus] = 'A' )
				ORDER BY [dbo].[HMIS_LivingSituation].[LivingSituationDate] DESC
			)

	--04/17/2020
	LEFT OUTER JOIN
	( SELECT DISTINCT 
		HICH.AssessmentID,
		HICH.ClientID,
		CASE WHEN HICH.IsChronicallyHomeless = 1 OR HICH.IsChronicallyHomelessByAssoc = 1
			THEN 1
			ELSE HICH.IsChronicallyHomeless
		END ChronicHomeless
		FROM dbo.ChronicHomeless HICH  
	) CHStatus ON CHStatus.ClientID = C.ClientID
		AND CHStatus.AssessmentID = A.AssessmentID
	LEFT JOIN dbo.ComboboxList('HMIS','YesNo') AS CB_CH ON CHStatus.ChronicHomeless = CB_CH.Item
	
	LEFT OUTER JOIN 
	( SELECT AssessmentID, AssessmentDate, ADAApproved, NoBedrooms, SexOffender, Arson, 
			GPDType, SaveHavenType, DVViolence,		-- new columns added 1/2/18
			DVHomelessDate, DVLethalityScore,
		CASE WHEN InterventionPrevention = 1		THEN 'A' ELSE '' END
			+ CASE WHEN InterventionES = 1			THEN 'B' ELSE '' END
			+ CASE WHEN InterventionSubstanceAbuse = 1 THEN 'C' ELSE '' END
			+ CASE WHEN InterventionRRH = 1			THEN 'D' ELSE '' END
			+ CASE WHEN InterventionPSH = 1			THEN 'E' ELSE '' END
			+ CASE WHEN InterventionSafeHaven = 1	THEN 'F' ELSE '' END
			+ CASE WHEN InterventionTH = 1			THEN 'G' ELSE '' END
			+ CASE WHEN InterventionGPD = 1			THEN 'H' ELSE '' END 
			+ CASE WHEN InterventionSSVF = 1		THEN 'I' ELSE '' END 
			+ CASE WHEN InterventionHUDVASH = 1		THEN 'J' ELSE '' END 
			+ CASE WHEN InterventionOther  = 1		THEN 'K' ELSE '' END
			+ CASE WHEN InterventionVOA = 1			THEN 'L' ELSE '' END
			+ CASE WHEN InterventionHVAFESGRRH = 1	THEN 'M' ELSE '' END
		AS CEInterventions
		FROM [dbo].[CT_CEAssessment] (NOLOCK) WHERE ActiveStatus='A' 
	) CEA ON CEA.AssessmentID = E.EnrollAssessmentID

	-- 2.24.21
	LEFT OUTER JOIN [dbo].Service S_R WITH (NOLOCK) ON E.EnrollID=S_R.EnrollID
		AND S_R.ServiceID = 
		(SELECT TOP 1 S.ServiceID
		FROM [dbo].[Service] S WITH (NOLOCK) 
		INNER JOIN [dbo].[ServiceReferral] SR WITH (NOLOCK) ON S.ServiceID=SR.ServiceID AND S.EnrollID = S_R.EnrollID
		INNER JOIN [dbo].[cmProvider] P WITH (NOLOCK) ON SR.ReferToProviderID=P.ProviderID AND P.ActiveStatus <> 'D' AND P.ProviderName NOT LIKE '%Shelter%'
		INNER JOIN [dbo].[ServiceCode] SC WITH (NOLOCK) ON SR.ServiceCodeID=SC.ServiceCodeID AND SC.ActiveStatus <> 'D' AND SC.Service <> 'CES - Navigation Referral'
		WHERE S.ActiveStatus <> 'D' 
		AND E.EnrollID = S.EnrollID
		AND (SC.Service IN ('CES - CHIP Referral','CES - OPH Referral','CES - PSH Referral','CES - RRH Referral','CES - TH Referral','CES - SSO Referral')
			OR SC.Service LIKE '%SSVF Self-Match%'
			OR P.ProviderName LIKE '%SSVF%'
			OR P.ProviderName = 'InteCare - CES')
		ORDER BY S.BeginDate DESC
		)
	LEFT OUTER JOIN [dbo].[ServiceReferral] SR WITH (NOLOCK) ON S_R.ServiceID=SR.ServiceID

WHERE 
	( E.[ExitDate] IS NULL ) 
	AND ( C.[ActiveStatus] <> 'D' )
	AND ( P.[ProgramType] = 14 );
	
--GO


