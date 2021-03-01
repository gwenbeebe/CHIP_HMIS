/****** Object:  View [dbo].[CUSTOM_vw_Dashboard]    Script Date: 4/4/2019 10:00:55 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO
/*
-- ==================================================================================================
-- Author:		Tony Sloderbeck, @WorkSolutionsInc.
-- Create date:	04/03/2019
-- Description:	CHIP Client Dashboard Additional Values View
-- Purpose:		Provides additional values for the Client Dashboard
-- ==================================================================================================
--Change History
--Date			Changed by			Description
11/16/2020		Tony Sloderbeck		Added PriorResidence value & VI-SPDAT Type
12/07/2020		Tony Sloderbeck		Added Living Situation & Living Situation Date
12/09/2020		Tony Sloderbeck		Added active status checks
2.26.2021		Gwen Beebe			Updated CLS logic to get most recent living situation
*/
ALTER VIEW [dbo].[CUSTOM_vw_Dashboard]
AS

	SELECT
		C.[ClientID],
		V.[ScoreTotal],
		P.[DocumentVerifyDate],
		P.[VerifyCHStatusDate],
		Living.[LivingSituationID],
		Living.[LivingSituationDate],
		[CLiving].[ItemDesc] AS [LivingSituation],
		V.[VISPDATType]
	FROM
		[dbo].[cmClient] C WITH (NOLOCK)
		-- Latest VI-SPDAT assessment
		LEFT JOIN
		(	SELECT LastVI.[ClientID], VI2.[AssessmentDate], V.[ScoreTotal],
				CASE V.[Type]
					WHEN 1 THEN 'Single'
					WHEN 2 THEN 'Family'
					WHEN 3 THEN 'JD'
					WHEN 4 THEN 'TAY'
				END AS [VISPDATType]
			FROM [dbo].[VISPDAT] V WITH (NOLOCK)
			INNER JOIN
			(	SELECT VI.[ClientID], 
				( SELECT TOP 1 [VulnerabilityID] FROM [dbo].[VulnerabilityIndex] WITH (NOLOCK) 
					WHERE [ClientID] = VI.[ClientID] 
					AND [dbo].[VulnerabilityIndex].[ActiveStatus] <> 'D'
					ORDER BY [AssessmentDate] DESC, [VulnerabilityID]
				) AS VID
				FROM [dbo].[VulnerabilityIndex] VI WITH (NOLOCK) 
				WHERE VI.[ActiveStatus] <> 'D'
				GROUP BY VI.[ClientID]
			) LastVI ON LastVI.[VID] = V.[VulnerabilityID]
			INNER JOIN [dbo].[VulnerabilityIndex] VI2 WITH (NOLOCK) ON VI2.[VulnerabilityID] = LastVI.[VID]
			WHERE VI2.[ActiveStatus] <> 'D'
		) V ON V.[ClientID] = C.[ClientID]
		LEFT JOIN
		(	SELECT LastPL.[ClientID], PL3.[CreatedDate], P.[DocumentVerifyDate], P.[VerifyCHStatusDate] 
			FROM [dbo].[CT_PrioritizationList] P WITH (NOLOCK)
			INNER JOIN
				( SELECT PL2.[ClientID],
					( SELECT TOP 1 [PrioritizationListID] FROM [dbo].[CT_PrioritizationList] (NOLOCK)
						WHERE [ClientID] = PL2.[ClientID] 
						AND [dbo].[CT_PrioritizationList].[ActiveStatus] <> 'D'
						ORDER BY [CreatedDate] DESC, [PrioritizationListID]
					) AS PID
					FROM [dbo].[CT_PrioritizationList] PL2 (NOLOCK) 
					WHERE PL2.[ActiveStatus] <> 'D'
					GROUP BY PL2.[ClientID]
				) LastPL ON LastPL.[PID] = P.[PrioritizationListID]
			INNER JOIN [dbo].[CT_PrioritizationList] PL3 WITH (NOLOCK) ON PL3.[PrioritizationListID] = LastPL.[PID] AND PL3.[ActiveStatus] <> 'D'
			INNER JOIN [dbo].[Enrollment] E WITH (NOLOCK) ON PL3.[ClientID] = E.[ClientID] AND E.[ActiveStatus] <> 'D' 
			INNER JOIN [dbo].[EnrollmentCase] EC WITH (NOLOCK) ON E.[CaseID] = EC.[CaseID] AND EC.[ActiveStatus] <> 'D'
			INNER JOIN [dbo].[Programs] PR WITH (NOLOCK) ON EC.[ProgramID] = PR.[ProgramID] AND PR.[ActiveStatus] <> 'D'
			WHERE ( PR.[ProgramType] = 14 )
		) P ON P.[ClientID] = C.[ClientID]

		-- 2.26.21
		LEFT OUTER JOIN [dbo].[HMIS_LivingSituation] Living WITH (NOLOCK) ON Living.[ClientID] = C.[ClientID]
			AND Living.[LivingSituationID] = 
			(SELECT TOP 1 L.[LivingSituationID]
			FROM [dbo].[HMIS_LivingSituation] L WITH (NOLOCK) 
			WHERE L.ActiveStatus <> 'D' 
				AND L.[ClientID] = C.[ClientID]
			ORDER BY L.[LivingSituationDate] DESC, L.[LivingSituationID] DESC
		)
		LEFT OUTER JOIN [dbo].[cmComboboxItem] CLiving WITH (NOLOCK) ON CLiving.[Item] = Living.[LivingSituation] 
			AND CLiving.[ComboBox] = 'PriorResidence' 
			AND CLiving.[ComboBoxGrp] = 'HMIS2020' 
			AND CLiving.[ActiveStatus] <> 'D'

GO