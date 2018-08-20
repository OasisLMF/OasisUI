-- (c) 2013-2016 Oasis LMF Ltd.  Software provided for early adopter evaluation only.
--Flamingo Database Generation Script
--Author: Ben Hayes
--Date: 2017-07-27
--Version: 0.394.0


-------------------------------------------------------------------------------
--Database
-------------------------------------------------------------------------------
USE [Master]
GO
IF EXISTS (SELECT * FROM tempdb.sys.tables WHERE name like '#TempVariables%') DROP TABLE #TempVariables
GO
------------------------------------------------------------------------------
-----------------------------------------------------------------
------------------------------------------- update environment names here --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------- ****and update use statement at line 63**** ------------------------------------------------------------------------------------------------------------------------------------------------------------------
DECLARE	@EnvironmentName nvarchar(255)	= '%ENVIRONMENT_NAME%'
DECLARE	@LoginName nvarchar(255)		= @EnvironmentName
DECLARE	@DBPassword nvarchar(255)		= '%USER_PASSWORD%'
DECLARE	@Version nvarchar(255)			= '%VERSION%'

-----------------------------------------------------------------
------------------------------------------------------------------------------
CREATE TABLE #TempVariables (VariableType nvarchar(255), VariableName nvarchar(255))
INSERT #TempVariables ([VariableType], [VariableName]) VALUES ('FileLocationSQL','%FILE_LOCATION_SQL_SERVER%')
INSERT #TempVariables ([VariableType], [VariableName]) VALUES ('FileLocationShiny','/var/www/oasis/Files')
INSERT #TempVariables ([VariableType], [VariableName]) VALUES ('DatabaseName','Flamingo' + case when @EnvironmentName = '' then '' else '_' + @EnvironmentName end)
INSERT #TempVariables ([VariableType], [VariableName]) VALUES ('EnvironmentName',@EnvironmentName)
INSERT #TempVariables ([VariableType], [VariableName]) VALUES ('LoginName',@LoginName)
INSERT #TempVariables ([VariableType], [VariableName]) VALUES ('DBPassword',@DBPassword)
INSERT #TempVariables ([VariableType], [VariableName]) VALUES ('FileLocation',@EnvironmentName)
INSERT #TempVariables ([VariableType], [VariableName]) VALUES ('Version',@Version)

DECLARE @SQL NVARCHAR(2500)
SET @SQL = 'IF EXISTS(SELECT * FROM sys.databases WHERE name = '''+(SELECT [VariableName] FROM #TempVariables WHERE [VariableType] = 'DatabaseName')+''') alter database '+ (SELECT [VariableName] FROM #TempVariables WHERE [VariableType] = 'DatabaseName') + ' set single_user with rollback immediate'
EXEC sp_executesql @SQL
GO

DECLARE @SQL NVARCHAR(2500)
SET @SQL = 'IF EXISTS(SELECT * FROM sys.databases WHERE name = '''+(SELECT [VariableName] FROM #TempVariables WHERE [VariableType] = 'DatabaseName')+''') DROP Database ' + (SELECT [VariableName] FROM #TempVariables WHERE [VariableType] = 'DatabaseName')
EXEC sp_executesql @SQL
GO

DECLARE @SQL NVARCHAR(2500)
SET @SQL = 'CREATE DATABASE [' + (SELECT [VariableName] FROM #TempVariables WHERE [VariableType] = 'DatabaseName') + ']'
EXEC sp_executesql @SQL
GO

--User Details
DECLARE @SQL NVARCHAR(2500)
SET @SQL = 'IF EXISTS (SELECT * FROM sys.syslogins WHERE name = '''+(SELECT [VariableName] FROM #TempVariables WHERE [VariableType] = 'LoginName')+''') DROP Login ' + (SELECT [VariableName] FROM #TempVariables WHERE [VariableType] = 'LoginName')
EXEC sp_executesql @SQL
GO

DECLARE @SQL NVARCHAR(2500)
SET @SQL = '
CREATE LOGIN ['+(SELECT [VariableName] FROM #TempVariables WHERE [VariableType] = 'LoginName')+'] 
WITH PASSWORD=N''' + (SELECT [VariableName] FROM #TempVariables WHERE [VariableType] = 'DBPassword') + ''', 
DEFAULT_DATABASE=['+(SELECT [VariableName] FROM #TempVariables WHERE [VariableType] = 'DatabaseName')+'], 
CHECK_EXPIRATION=OFF, CHECK_POLICY=OFF'
EXEC sp_executesql @SQL
GO
DECLARE @SQL NVARCHAR(2500)
SET @SQL = 'ALTER SERVER ROLE [sysadmin] ADD MEMBER [' +(SELECT [VariableName] FROM #TempVariables WHERE [VariableType] = 'LoginName')+']'
EXEC sp_executesql @SQL
GO
------------------------------------------------------------------------------
-----------------------------------------------------------------
USE [Flamingo_%ENVIRONMENT_NAME%]   -------------------------------------------------------------------update here as well----------------------------------------------------------------------------------
GO
-----------------------------------------------------------------
------------------------------------------------------------------------------
DECLARE @SQL NVARCHAR(2500)
SET @SQL = 'CREATE USER ['+(SELECT [VariableName] FROM #TempVariables WHERE [VariableType] = 'LoginName')+'] FOR LOGIN ['+(SELECT [VariableName] FROM #TempVariables WHERE [VariableType] = 'LoginName')+']'
EXEC sp_executesql @SQL
GO
DECLARE @SQL NVARCHAR(2500)
SET @SQL = 'ALTER ROLE [db_owner] ADD MEMBER ['+(SELECT [VariableName] FROM #TempVariables WHERE [VariableType] = 'LoginName')+']'
EXEC sp_executesql @SQL
GO
--configure to allow assemblies
DECLARE @SQL NVARCHAR(2500)
SET @SQL = 'ALTER DATABASE ['+(SELECT [VariableName] FROM #TempVariables WHERE [VariableType] = 'DatabaseName')+'] SET TRUSTWORTHY ON;'
EXEC sp_executesql @SQL
GO
sp_configure 'show advanced options', 1;
RECONFIGURE;
GO
sp_configure 'Ad Hoc Distributed Queries', 1;
RECONFIGURE;
GO
-------------------------------------------------------------------------------
--Tables
-------------------------------------------------------------------------------
-----------------------
--Canonical Model
-----------------------
CREATE TABLE [dbo].[Account]
	(
	[AccountID] [int] PRIMARY KEY,
	[AccountName] [nvarchar](255) NULL,
	[Deleted] [bit] NULL
	)
/*
CREATE TABLE [dbo].[Correlation]
	(
	[CorrelationID] [int] PRIMARY KEY,
	[CorrelationName] [nvarchar](255) NULL,
	[ProgID] [int] NOT NULL
	)
CREATE TABLE [dbo].[CorrelationItem]
	(
	[CorrelationItemID] [int] PRIMARY KEY,
	[CorrelationID] [int] NOT NULL,
	[InterestRiskID] [int] NOT NULL,
	[InterestSubRiskID] [int] NOT NULL,
	[InterestExposureID] [int] NOT NULL
	)
*/
CREATE TABLE [dbo].[CoverageItem]
	(
	[CoverageItemID] [int] PRIMARY KEY,
	[PolicyCoverageID] [int] NOT NULL,
	[InterestRiskID] [int] NULL,
	[InterestSubRiskID] [int] NULL,
	[InterestExposureID] [int] NULL
	)
CREATE TABLE [dbo].[CoverageType]
	(
	[CoverageTypeID] [int] PRIMARY KEY,
	[CoverageTypeName] [nvarchar](255) NOT NULL,
	[CoverageTypeDesc] [nvarchar](255) NULL
	)
CREATE TABLE [dbo].[InterestExposure]
	(
	[InterestExposureID] [int] PRIMARY KEY,
	[InterestSubRiskID] [int] NOT NULL,
	[TIVCcy] [nvarchar](255) NULL,
	[TIV] [float] NULL,
	[CoverageTypeID] [int] NULL,
	[ProfileID] [int] NOT NULL
	)
CREATE TABLE [dbo].[InterestExposureValues]
	(
	[InterestExposureID] [int] NOT NULL,
	[DimensionID] [int] NOT NULL,
	[ProfileElementID] [int] NULL,
	[FieldValue] [nvarchar](255) NULL,
	[RuleID] [int] NULL
	)
CREATE TABLE [dbo].[InterestGroup]
	(
	[InterestGroupID] [int] PRIMARY KEY,
	[InterestGroupName] [nvarchar](255) NULL,
	[InterestGroupDesc] [nvarchar](255) NULL,
	[ScheduleID] [int] NOT NULL,
	[ProgID] [int] NOT NULL,
	[ProfileID] [int] NOT NULL
	)
CREATE TABLE [dbo].[InterestGroupValues]
	(
	[InterestGroupID] [int] NOT NULL,
	[DimensionID] [int] NOT NULL,
	[ProfileElementID] [int] NULL,
	[FieldValue] [nvarchar](255) NULL,
	[RuleID] [int] NULL
	)
CREATE TABLE [dbo].[InterestRisk]
	(
	[InterestRiskID] [int] PRIMARY KEY,
	[InterestName] [nvarchar](255) NULL,
	[InterestSeqNo] [int] NULL,
	[InterestGroupID] [int] NOT NULL,
	[ProfileID] [int] NOT NULL,
	[SourceRowId] [int] NULL
	)
CREATE TABLE [dbo].[InterestRiskValues]
	(
	[InterestRiskID] [int] NOT NULL,
	[DimensionID] [int] NOT NULL,
	[ProfileElementID] [int] NULL,
	[FieldValue] [nvarchar](255) NULL,
	[RuleID] [int] NULL
	)
CREATE TABLE [dbo].[InterestSubRisk]
	(
	[InterestSubRiskID] [int] PRIMARY KEY,
	[InterestSubRiskName] [nvarchar](255) NULL,
	[InterestSubRiskSeqNo] [int] NULL,
	[InterestRiskID] [int] NOT NULL,
	[ProfileID] [int] NOT NULL,
	[SourceRowId] [int] NULL
	)
CREATE TABLE [dbo].[InterestSubRiskValues]
	(
	[InterestSubRiskID] [int] NOT NULL,
	[DimensionID] [int] NOT NULL,
	[ProfileElementID] [int] NULL,
	[FieldValue] [nvarchar](255) NULL,
	[RuleID] [int] NULL
	)
CREATE TABLE [dbo].[Peril] 
	(
	[PerilId] [int] PRIMARY KEY, 
	[PerilCode] [nvarchar](255) NOT NULL, 
	[PerilDesc] [nvarchar](255) NULL
	)
CREATE TABLE [dbo].[Policy]
	(
	[PolicyID] [int] PRIMARY KEY,
	[PolicyName] [nvarchar](255) NULL,
	[PolicyGroupID] [int] NULL,
	[ProgID] [int] NOT NULL,
	[PolicyTypeID] [int] NULL,
	[PolicyRef] [nvarchar](255) NULL,
	[Order] [float] NULL,
	[OrderWholeInd] [int] NULL,
	[Inception] [datetime] NULL,
	[Expiry] [datetime] NULL,
	[AltAccountID] [int] NULL,
	[UWReference] [nvarchar](255) NULL,
	[ProfileID] [int] NOT NULL
	)
CREATE TABLE [dbo].[PolicyCoverage]
	(
	[PolicyCoverageID] [int] PRIMARY KEY,
	[PolicyCoverageName] [nvarchar](255) NULL,
	[CoverageTypeID] [int] NOT NULL,
	[PolicyPerilID] [int] NOT NULL,
	[ProfileID] [int] NOT NULL,
	[Level] [int] NULL
	)
CREATE TABLE [dbo].[PolicyCoverageValues]
	(
	[PolicyCoverageID] [int] NOT NULL,
	[DimensionID] [int] NOT NULL,
	[ProfileElementID] [int] NULL,
	[FieldValue] [nvarchar](255) NULL,
	[RuleID] [int] NULL
	)
CREATE TABLE [dbo].[PolicyLayer]
	(
	[PolicyLayerID] [int] PRIMARY KEY,
	[PolicyLayerName] [nvarchar](255) NULL,
	[PolicyID] [int] NOT NULL,
	[LayerNumber] [int] NOT NULL
	)
CREATE TABLE [dbo].[PolicyLayerValues]
	(
	[PolicyLayerID] [int] NOT NULL,
	[DimensionID] [int] NOT NULL,
	[ProfileElementID] [int] NULL,
	[FieldValue] [nvarchar](255) NULL,
	[RuleID] [int] NULL
	)
CREATE TABLE [dbo].[PolicyPeril]
	(
	[PolicyPerilID] [int] PRIMARY KEY,
	[PolicyPerilName] [nvarchar](255) NULL,
	[PolicyID] [int] NOT NULL,
	[PerilID] [int] NOT NULL
	)
CREATE TABLE [dbo].[PolicyType]
	(
	[PolicyTypeID] [int] PRIMARY KEY,
	[PolicyTypeName] [nvarchar](255) NULL
	)
CREATE TABLE [dbo].[PolicyValues]
	(
	[PolicyID] [int] NOT NULL,
	[DimensionID] [int] NOT NULL,
	[ProfileElementID] [int] NULL,
	[FieldValue] [nvarchar](255) NULL,
	[RuleID] [int] NULL
	)
CREATE TABLE [dbo].[Prog]
	(
	[ProgID] [int] PRIMARY KEY,
	[ProgName] [nvarchar](255) NULL,
	[AccountID] [int] NOT NULL,
	[TransformID] int NOT NULL,
	[Deleted] [bit] NULL,
	[Status] [nvarchar](255) NULL
	)
CREATE TABLE [dbo].[ProgOasis]
	(
	[ProgOasisId] [int] PRIMARY KEY,
	[ProgId] [int] NOT NULL,
	[ModelId] [int] NOT NULL,
	[SourceFileId] [int] NULL,
	[OasisKeyName] [nvarchar](255) NULL,
	[API1aDateTime] [datetime] NULL,
	[API1bDateTime] [datetime] NULL,
	[API1cDateTime] [datetime] NULL,
	[SessionId] [int] NULL,
	[TransformID] [int] NULL,
	[Status] [nvarchar](255) NULL
	)
CREATE TABLE [dbo].[ProgOasisKeys]
	(
	[ProgOasisKeyId] [int] PRIMARY KEY,
	[ProgOasisId] [int] NULL,
	[LocID] [bigint] NULL,
	[PerilID] [bigint] NULL,
	[CoverageID] [bigint] NULL,
	[AreaPerilId] [bigint] NULL,
	[VulnerabilityId] [bigint] NULL
	)
CREATE TABLE [dbo].[Schedule]
	(
	[ScheduleID] [int] PRIMARY KEY,
	[ScheduleName] [nvarchar](255) NULL,
	[ScheduleDesc] [nvarchar](255) NULL,
	[ProfileID] [int] NULL
	)
CREATE TABLE [dbo].[ScheduleValues]
	(
	[ScheduleID] [int] NOT NULL,
	[DimensionID] [int] NOT NULL,
	[ProfileElementID] [int] NULL,
	[FieldValue] [nvarchar](255) NULL,
	[RuleID] [int] NULL
	)
CREATE TABLE [dbo].[Transform]
	(
	[TransformID] [int] PRIMARY KEY,
	[TransformName] [nvarchar](255) NULL,
	[TransformDesc] [nvarchar](255) NULL,
	[TransformTypeID] [int] NOT NULL
	)
CREATE TABLE [dbo].[TransformType]
	(
	[TransformTypeID] [int] PRIMARY KEY,
	[TransformTypeName] [nvarchar](255) NULL,
	[TransformTypeDesc] [nvarchar](255) NULL
	)
-----------------------
--User Security
-----------------------
CREATE TABLE [dbo].[BFEUser]
	(
	[BFEUserID] [int] PRIMARY KEY,
	[BFEUserName] [nvarchar](255) NOT NULL,
	[CompanyID] [int] NOT NULL,
	[BFEUserLogin] [nvarchar](255) NOT NULL,
	[BFEUserPassword] [nvarchar](255) NOT NULL,
	[BFEUserDept] [nvarchar](255) NULL,
	[Deleted] [bit] NULL
	)
CREATE TABLE [dbo].[Company]
	(
	[CompanyID] [int] PRIMARY KEY,
	[CompanyName] [nvarchar](255) NULL,
	[CompanyDomicile] [nvarchar](255) NULL,
	[CompanyLegalName] [nvarchar](255) NULL,
	[CompanyRegistrationNo] [nvarchar](255) NULL,
	[Deleted] [bit] NULL
	)
CREATE TABLE [dbo].[ModelLicense]
	(
	[ModelLicenseID] [int] PRIMARY KEY,
	[ModelID] [int] NOT NULL,
	[CompanyID] [int] NOT NULL,
	[ModelLicenseName] [nvarchar](255) NULL,
	[ModelVersionDescription] [nvarchar](255) NULL,
	[LicenseStartDate] [datetime] NULL,
	[LicenseEndDate] [datetime] NULL,
	[LicenseType] [nvarchar](255) NULL,
	[LicenseContractRef] [nvarchar](255) NULL
	)
CREATE TABLE [dbo].[OasisUser]
	(
	[OasisUserID] [int] PRIMARY KEY,
	[OasisUserName] [nvarchar](255) NOT NULL,
	[ModelLicenseID] [int] NOT NULL,
	[OasisSystemID] [int] NOT NULL,
	[SystemLogin] [nvarchar](255) NOT NULL,
	[SystemPassword] [nvarchar](255) NOT NULL,
	[Django Login] [nvarchar](255) NOT NULL,
	[Django Password] [nvarchar](255) NOT NULL,
	[Deleted] [bit] NULL
	)
CREATE TABLE [dbo].[Owner]
	(
	[OwnerID] [int] PRIMARY KEY,
	[OwnerName] [nvarchar](255) NULL,
	[BFEUserID] [int] NULL,
	[OwnerDesc] [nvarchar](255) NULL,
	[Deleted] [bit] NULL
	)
CREATE TABLE [dbo].[Resource]
	(
	[ResourceID] [int] PRIMARY KEY,
	[ResourceTable] [nvarchar](255) NULL,
	[ResourceKey] [nvarchar](255) NULL,
	[ResourceQualifier] [nvarchar](255) NULL,
	[ResourceTypeID] [int] NOT NULL
	)
CREATE TABLE [dbo].[ResourceType]
	(
	[ResourceTypeID] [int] PRIMARY KEY,
	[ResourceTypeName] [nvarchar](255) NULL,
	[Source] [nvarchar](255) NULL
	)
CREATE TABLE [dbo].[SecurityGroup]
	(
	[SecurityGroupID] [int] PRIMARY KEY,
	[SecurotyGroupName] [nvarchar](255) NULL,
	[Deleted] [bit] NOT NULL
	)
CREATE TABLE [dbo].[SecurityGroupResource]
	(
	[SecurityGroupResourceID] [int] PRIMARY KEY,
	[SecurityGroupID] [int] NOT NULL,
	[ResourceID] [int] NOT NULL,
	[Mode] [nvarchar](255) NOT NULL,
	[ResourcePasswordC] [nvarchar](255) NULL,
	[ResourcePasswordR] [nvarchar](255) NULL,
	[ResourcePasswordU] [nvarchar](255) NULL,
	[ResourcePasswordD] [nvarchar](255) NULL,
	[ResourcePasswordP] [nvarchar](255) NULL
	)
CREATE TABLE [dbo].[UserLicense]
	(
	[UserLicenseID] [int] PRIMARY KEY,
	[BFEUserID] [int] NOT NULL,
	[OasisUserID] [int] NOT NULL
	)
CREATE TABLE [dbo].[UserSecurityGroup]
	(
	[UserSecurityGroupID] [int] PRIMARY KEY,
	[BFEUserID] [int] NOT NULL,
	[Security Group ID] [int] NOT NULL
	)
-----------------------
--Oasis Tables
-----------------------
CREATE TABLE [dbo].[OasisITEMS]
	(
	[item_id] [int] NOT NULL,
	[coverage_id] [int] NOT NULL,
	[areaperil_id] [int] NOT NULL,
	[vulnerability_id] [int] NOT NULL,
	[group_id] [int] NOT NULL
	)
CREATE TABLE [dbo].[OasisITEMDICT]
	(
	[item_id] [int] NOT NULL,
	[coverage_id] [int] NOT NULL,
	[location_id] [int] NULL,
	[location_desc] [nvarchar](255) NULL,
	[lob_id] [int] NULL,
	[lob_desc] [nvarchar](255) NULL,
	[county_id] [int] NULL,
	[county_desc] [nvarchar](255) NULL,
	[state_id] [int] NULL,
	[state_desc] [nvarchar](255) NULL
	)
CREATE TABLE [dbo].[OasisCOVERAGES]
	(
	[coverage_id] [int] NOT NULL,
	[tiv] [float] NOT NULL
	)
CREATE TABLE [dbo].[OasisGULSUMMARYXREF]
	(
	[coverage_id] [int] NOT NULL,
	[summary_id] [int] NOT NULL,
	[summaryset_id] [int] NOT NULL
	)
CREATE TABLE [dbo].[OasisFM_PROGRAMME]
	(
	[from_agg_id] [int] NOT NULL,
	[level_id] [int] NOT NULL,
	[to_agg_id] [int] NOT NULL
	)
	CREATE TABLE [dbo].[OasisFMDICT]
	(
	[output_id] [int] NULL,
	[item_id] [int] NULL,
	[agg_id] [int] NULL,
	[layer_id] [int] NULL,
	[policy_name] [nvarchar](255) NULL,
	[layer_name] [nvarchar](255) NULL
	)
CREATE TABLE [dbo].[OasisFM_POLICYTC]
	(
	[layer_id] [int] NOT NULL,
	[level_id] [int] NOT NULL,
	[agg_id] [int] NOT NULL,
	[policytc_id] [int] NOT NULL
	)
CREATE TABLE [dbo].[OasisFM_PROFILE]
	(
	[policytc_id] [int] NOT NULL, 
	[calcrule_id] [int] NOT NULL, 
	[allocrule_id] [int] NOT NULL, 
	[ccy_id] [int] NULL, 
	[deductible] [float] NULL, 
	[limit] [float] NULL, 
	[share_prop_of_lim] [float] NULL, 
	[deductible_prop_of_loss] [float] NULL, 
	[limit_prop_of_loss] [float] NULL, 
	[deductible_prop_of_tiv] [float] NULL, 
	[limit_prop_of_tiv] [float] NULL, 
	[deductible_prop_of_limit] [float] NULL
	)
CREATE TABLE [dbo].[OasisFM_PROFILE_ALLOC]
	(
	[policytc_id] [int] NOT NULL, 
	[calcrule_id] [int] NOT NULL, 
	[allocrule_id] [int] NOT NULL, 
	[ccy_id] [int] NULL, 
	[deductible] [float] NULL, 
	[limit] [float] NULL, 
	[share_prop_of_lim] [float] NULL, 
	[deductible_prop_of_loss] [float] NULL, 
	[limit_prop_of_loss] [float] NULL, 
	[deductible_prop_of_tiv] [float] NULL, 
	[limit_prop_of_tiv] [float] NULL, 
	[deductible_prop_of_limit] [float] NULL
	)
CREATE TABLE [dbo].[OasisFM_XREF]
	(
	[output_id] [int] NOT NULL,
	[agg_id] [int] NOT NULL,
	[layer_id] [int] NOT NULL
	)
CREATE TABLE [dbo].[OasisFM_XREF_ALLOC]
	(
	[output_id] [int] NOT NULL,
	[agg_id] [int] NOT NULL,
	[layer_id] [int] NOT NULL
	)
CREATE TABLE [dbo].[OasisFMSUMMARYXREF]
	(
	[output_id] [int] NOT NULL,
	[summary_id] [int] NOT NULL,
	[summaryset_id] [int] NOT NULL
	)
-----------------------
--Processes
-----------------------
CREATE TABLE [dbo].[Action]
	(
	[ActionID] [int] PRIMARY KEY,
	[ActionName] [nvarchar](255) NOT NULL,
	[ActionDesc] [nvarchar](255) NULL
	)
CREATE TABLE [dbo].[Parameter]
	(
	[ParameterID] [int] PRIMARY KEY,
	[ParameterName] [nvarchar](255) NOT NULL,
	[ParameterDesc] [nvarchar](255) NULL,
	[ActionID] [int] NOT NULL,
	[JSONLevel] [int] NULL,
	[ParameterFormat] [nvarchar](255) NULL
	)
CREATE TABLE [dbo].[Workflow]
	(
	[WorkflowID] [int] PRIMARY KEY,
	[WorkflowName] [nvarchar](255) NOT NULL
	)
CREATE TABLE [dbo].[WorkflowElement]
	(
	[WorkflowElementID] [int] PRIMARY KEY,
	[WorkflowElementName] [nvarchar](255) NOT NULL,
	[WorkflowID] [int] NOT NULL,
	[ActionID] [int] NOT NULL,
	[SequenceNumber] [int] NOT NULL
	)
CREATE TABLE [dbo].[WorkflowParameter]
	(
	[WorkflowParameterID] [int] PRIMARY KEY,
	[WorkflowElementID] [int] NOT NULL,
	[ParameterID] [int] NOT NULL,
	[WorkflowParameterValue] [nvarchar](255) NULL,
	[WorkflowParameterSource] [nvarchar](255) NULL
	)
CREATE TABLE [dbo].[ProcessRun]
	(
	[ProcessRunID] [int] PRIMARY KEY,
	[ProcessRunName] [nvarchar](255) NOT NULL,
	[ProgOasisID] [int] NOT NULL,
	[ProcessRunStatus] [nvarchar](255) NULL
	)
CREATE TABLE [dbo].[ElementRun]
	(
	[ElementRunID] [int] PRIMARY KEY,
	[ElementRunName] [nvarchar](255) NOT NULL,
	[WorkflowElementID] [int] NOT NULL,
	[ProcessRunID] [int] NOT NULL
	)
CREATE TABLE [dbo].[ElementRunLog]
	(
	[ElementRunLogID] [int] PRIMARY KEY,
	[ElementRunID] [int] NOT NULL,
	[LogTimestamp] [datetime] NOT NULL default getdate(),
	[LogStatus] [nvarchar](255) NOT NULL,
	[LogType] [nvarchar](255) NULL,
	[LogDescription] [nvarchar](255) NULL
	)
CREATE TABLE [dbo].[ParameterRun]
	(
	[ParameterRunID] [int] PRIMARY KEY,
	[ParameterRunName] [nvarchar](255) NULL,
	[ElementRunID] [int] NOT NULL,
	[WorkflowParameterID] [int] NOT NULL,
	[ParameterRunValue] [nvarchar](255) NULL,
	[ParameterRunFormat] [nvarchar](255) NULL,
	)
CREATE TABLE [dbo].[OutputRun]
	(
	[OutputRunID] [int] PRIMARY KEY,
	[OutputTypeID] [int] NOT NULL,
	[ElementRunID] [int] NOT NULL,
	[OutputID] [int] NOT NULL,
	[SummaryLevelRunID] [int] NOT NULL
	)
CREATE TABLE [dbo].[OutputType]
	(
	[OutputTypeID] [int] PRIMARY KEY,
	[PerspectiveID] [int] NOT NULL,
	[AnalysisTypeID] [int] NOT NULL,
	[SummaryLevelID] [int] NOT NULL
	)
CREATE TABLE [dbo].[SummaryLevel]
	(
	[SummaryLevelID] [int] PRIMARY KEY,
	[SummaryLevelName] [nvarchar](255) NOT NULL,
	[SummaryLevelSequence] [int] NOT NULL,
	[ParameterStub] [nvarchar](255) NOT NULL
	)
CREATE TABLE [dbo].[SummaryLevelRun]
	(
	[SummaryLevelRunID] [int] PRIMARY KEY,
	[SummaryLevelID] [int] NOT NULL,
	[ProcessRunID] [int] NOT NULL,
	[SummarySetID] [int] NOT NULL,
	[PerspectiveID] [int] NOT NULL
	)
CREATE TABLE [dbo].[Perspective]
	(
	[PerspectiveID] [int] PRIMARY KEY,
	[PerspectiveName] [nvarchar](255) NOT NULL
	)
CREATE TABLE [dbo].[AnalysisType]
	(
	[AnalysisTypeID] [int] PRIMARY KEY,
	[AnalysisTypeName] [nvarchar](255) NOT NULL,
	[AnalysisFileNameStub] [nvarchar](255) NOT NULL,
	[JSONLevel] int NULL,
	[ParameterStub] [nvarchar](255) NOT NULL,
	[LECFlag] int NULL
	)
-----------------------
--Models
-----------------------
CREATE TABLE [dbo].[Model]
	(
	[ModelID] [int] PRIMARY KEY,
	[ModelName] [nvarchar](255) NULL,
	[ModelFamilyID] [int] NOT NULL,
	[ModelDescription] [nvarchar](255) NULL,
	[VersionRef] [nvarchar](255) NULL,
	[ReleaseDate] [datetime] NULL,
	[Contact] [nvarchar](255) NULL,
	[ModelTypeID] [int] NOT NULL,
	[Deleted] [bit] NULL
	)
CREATE TABLE [dbo].[ModelCoverageType]
	(
	[ModelCoverageTypeId] [int] PRIMARY KEY,
	[ModelId] [int],
	[CoverageTypeId] [int],
	[ModelCoverageTypeName] [nvarchar](255),
	[ModelCoverageTypeDescription]  [nvarchar](255) NULL
	)
CREATE TABLE [dbo].[ModelFamily]
	(
	[ModelFamilyID] [int] PRIMARY KEY,
	[ModelFamilyName] [nvarchar](255) NULL,
	[SupplierID] [int] NOT NULL
	)
CREATE TABLE [dbo].[ModelPeril]
	(
	[ModelPerilId] [int] PRIMARY KEY,
	[ModelId] [int],
	[PerilId] [int],
	[ModelPerilName] [nvarchar](255),
	[ModelPerilDescription] [nvarchar](255) NULL
	)
CREATE TABLE [dbo].[ModelResource]
	(
	[ModelResourceID] [int] PRIMARY KEY,
	[ModelResourceName] [nvarchar](255) NULL,
	[ResourceTypeID] [int] NOT NULL,
	[OasisSystemID] [int] NULL,
	[ModelID] [int] NOT NULL,
	[ModelResourceValue] [nvarchar](255) NULL
	)
CREATE TABLE [dbo].[ModelType]
	(
	[ModelTypeID] [int] PRIMARY KEY,
	[ModelTypeName] [nvarchar](255) NULL
	)
CREATE TABLE [dbo].[OasisSystem]
	(
	[OasisSystemID] [int] PRIMARY KEY,
	[OasisSystemName] [nvarchar](255) NULL,
	[OasisSystemDescription] [nvarchar](255) NULL,
	[URL] [nvarchar](255) NULL,
	[Port] [int] NULL,
	[SysConfigID] [int] NULL
	)
CREATE TABLE [dbo].[OasisSystemService]
	(
	[OasisSystemServiceID] [int] PRIMARY KEY,
	[OasisSystemID] [int] NOT NULL,
	[ServiceID] [int] NOT NULL
	)
CREATE TABLE [dbo].[Service]
	(
	[ServiceID] [int] PRIMARY KEY,
	[ServiceName] [nvarchar](255) NULL,
	[ServiceDesc] [nvarchar](255) NULL,
	[ServiceTypeID] [int] NOT NULL,
	[ModelID] [int] NOT NULL
	)
CREATE TABLE [dbo].[ServiceType]
	(
	[ServiceTypeID] [int] PRIMARY KEY,
	[ServiceTypeName] [nvarchar](255) NOT NULL,
	[ServiceDesc] [nvarchar](255) NULL
	)
CREATE TABLE [dbo].[Supplier]
	(
	[SupplierID] [int] PRIMARY KEY,
	[SupplierName] [nvarchar](255) NULL,
	[SupplierDesc] [nvarchar](255) NULL,
	[SupplierLegalName] [nvarchar](255) NULL,
	[SupplierAddress] [nvarchar](255) NULL,
	[SupplierPostcode] [nvarchar](255) NULL,
	[SupplierTelNo] [nvarchar](255) NULL,
	[Deleted] [bit] NULL
	)
-----------------------
--File Management
-----------------------
CREATE TABLE [dbo].[File]
	(
	[FileId] [int] PRIMARY KEY,
	[FileName] [nvarchar](255) NULL,
	[FileDesc] [nvarchar](255) NULL,
	[SourceID] [int] NOT NULL,
	[OwnerID] [int] NOT NULL,
	[LocationID] [int] NOT NULL,
	[DateTimeCreated] [datetime] NULL,
	[DateTimeUpdated] [datetime] NULL,
	[DateTimeDeleted] [datetime] NULL,
	[OwnerNameCreated] [nvarchar](255) NULL,
	[OwnerNameUpdated] [nvarchar](255) NULL,
	[OwnerNameDeleted] [nvarchar](255) NULL,
	[FileTypeId] [int] NOT NULL
	)
CREATE TABLE [dbo].[FileResource]
	(
	[FileResourceID] [int] PRIMARY KEY,
	[FileID] [int] NOT NULL,
	[ResourceID] [int] NOT NULL
	)
CREATE TABLE [dbo].[FileType]
	(
	[FileTypeId] [int] PRIMARY KEY,
	[FileTypeDesc] [nvarchar](255) NULL
	)
CREATE TABLE [dbo].[Location]
	(
	[LocationID] [int] PRIMARY KEY,
	[LocationName] [nvarchar](255) NOT NULL,
	[LocationDesc] [nvarchar](255) NULL,
	[LocationPathUnix] [nvarchar](255) NULL
	)
CREATE TABLE [dbo].[OutputTypes]
	(
	[OutputTypeID] [int] PRIMARY KEY,
	[OutputType] [nvarchar](255) NULL,
	[ResultsType] [int] NULL,
	[ResultsTypeDesc] [nvarchar](255) NULL,
	[AnalysisType] [int] NULL,
	[AnalysisTypeDesc] [nvarchar](255) NULL,
	[SummaryType] [int] NULL,
	[SummaryTypeDesc] [nvarchar](255) NULL,
	[Allocated] [int] NULL,
	[FileTypeID] [int] NULL
	)
CREATE TABLE [dbo].[Source]
	(
	[SourceId] [int] PRIMARY KEY,
	[SourceName] [nvarchar](255) NULL,
	[SourceDesc] [nvarchar](255) NULL
	)
	
-----------------------
--Profiles
-----------------------
CREATE TABLE [dbo].[Field]
	(
	[FieldID] [int] PRIMARY KEY,
	[FieldName] [nvarchar](255) NULL,
	[FormatID] [int] NOT NULL,
	[FieldlLength] [int] NULL,
	[FieldRule] [int] NULL,
	[IsOasisField] [bit] NULL
	)
/*
CREATE TABLE [dbo].[FieldRule]
	(
	[FieldRuleID] [int]  PRIMARY KEY,
	[FieldID] [int] NOT NULL,
	[RuleID] [int] NOT NULL
	)
*/
CREATE TABLE [dbo].[Format]
	(
	[FormatID] [int] PRIMARY KEY,
	[FormatName] [nvarchar](255) NOT NULL,
	[FormatDesc] [nvarchar](255) NULL
	)
CREATE TABLE [dbo].[Profile]
	(
	[ProfileID] [int] PRIMARY KEY,
	[ProfileName] [nvarchar](255) NULL,
	[ProfileTypeID] [int] NOT NULL
	)
CREATE TABLE [dbo].[ProfileElement]
	(
	[ProfileElementID] [int] PRIMARY KEY,
	[ProfileElementName] [nvarchar](255) NULL,
	[ProfileID] [int] NOT NULL,
	[TableID] [int] NOT NULL,
	[FieldID] [int] NOT NULL
	)
CREATE TABLE [dbo].[ProfileElementRule]
	(
	[ProfileElementRuleID] [int] PRIMARY KEY,
	[ProfileElementID] [int] NOT NULL,
	[RuleID] [int] NOT NULL
	)
CREATE TABLE [dbo].[ProfileResource]
	(
	[ProfileResourceID] [int] PRIMARY KEY,
	[ProfileID] [int] NOT NULL,
	[ResourceID] [int] NOT NULL
	)
CREATE TABLE [dbo].[ProfileType]
	(
	[ProfileTypeID] [int] PRIMARY KEY,
	[ProfileTypeName] [nvarchar](255) NULL
	)
CREATE TABLE [dbo].[ProfileValueDetail]
	(
	[ProfileValueDetailID] [int] PRIMARY KEY,
	[ProfileElementID] [int],
	[PerilID] [int],
	[CoverageTypeID] [int],
	[ElementDimensionID] [int]
	)
/*
CREATE TABLE [dbo].[Rule]
	(
	[RuleID] [int] PRIMARY KEY,
	[RuleName] [nvarchar](255) NULL,
	[RuleCode] [nvarchar](255) NULL,
	[RuleErrorType] [nvarchar](255) NULL,
	[RuleErrorMessage] [nvarchar](255) NULL
	)
*/
CREATE TABLE [dbo].[Table]
	(
	[TableID] [int] PRIMARY KEY,
	[TableName] [nvarchar](255) NULL
	)
-----------------------
--Shiny Screen tables
-----------------------
CREATE TABLE [dbo].[OperationOperationID]
	(
	[OperationID] [int] PRIMARY KEY,
	[Operation] [nvarchar](255) NULL
	)
CREATE TABLE [dbo].[OperationFileType]
	(
	[OperationFileTypeID] [int] PRIMARY KEY,
	[OperationID] [int],
	[FileTypeID] [int] NOT NULL
	)
CREATE TABLE [dbo].[OutputOptions]
	(
	[OutputOptionID] [int] PRIMARY KEY,
	[OutputOption] [nvarchar](255) NULL,
	[OutputTypeID] [int] NULL,
	) 
CREATE TABLE [dbo].[LogDatabaseUsage]
	(
	[LogID] [int] identity(1,1) PRIMARY KEY,
	[ProcedureName] [nvarchar](255),
	[ParameterList] [nvarchar](2500) NULL,
	[LogTimestamp] [datetime],
	[LogStatus] [nvarchar](255) NULL
	)
CREATE TABLE [dbo].[Version]
	(
	[Type] [nvarchar](100) NOT NULL,
	[Version] [nvarchar](100) NOT NULL,
	[Tag] [nvarchar](100) NOT NULL
	) 
CREATE TABLE [dbo].[ModelTransform]
	(
	[ModelID] [int] NOT NULL,
	[TransformID] [int] NOT NULL
	)
GO
-------------------------------------------------------------------------------
--Foreign Keys
-------------------------------------------------------------------------------
ALTER TABLE [dbo].[BFEUser] WITH CHECK ADD  CONSTRAINT [FK_BFEUser_CompanyID] FOREIGN KEY([CompanyID]) REFERENCES [dbo].[Company] ([CompanyID])
--ALTER TABLE [dbo].[Correlation] WITH CHECK ADD  CONSTRAINT [FK_Correlation_ProgID] FOREIGN KEY([ProgID]) REFERENCES [dbo].[Prog] ([ProgID])
--ALTER TABLE [dbo].[CorrelationItem] WITH CHECK ADD  CONSTRAINT [FK_CorrelationItem_CorrelationID] FOREIGN KEY([CorrelationID]) REFERENCES [dbo].[Correlation] ([CorrelationID])
--ALTER TABLE [dbo].[CorrelationItem] WITH CHECK ADD  CONSTRAINT [FK_CorrelationItem_InterestExposureID] FOREIGN KEY([InterestExposureID]) REFERENCES [dbo].[InterestExposure] ([InterestExposureID])
--ALTER TABLE [dbo].[CorrelationItem] WITH CHECK ADD  CONSTRAINT [FK_CorrelationItem_InterestRiskID] FOREIGN KEY([InterestRiskID]) REFERENCES [dbo].[InterestRisk] ([InterestRiskID])
--ALTER TABLE [dbo].[CorrelationItem] WITH CHECK ADD  CONSTRAINT [FK_CorrelationItem_InterestSubRiskID] FOREIGN KEY([InterestSubRiskID]) REFERENCES [dbo].[InterestSubRisk] ([InterestSubRiskID])
ALTER TABLE [dbo].[CoverageItem] WITH CHECK ADD  CONSTRAINT [FK_CoverageItem_InterestExposureID] FOREIGN KEY([InterestExposureID]) REFERENCES [dbo].[InterestExposure] ([InterestExposureID])
ALTER TABLE [dbo].[CoverageItem] WITH CHECK ADD  CONSTRAINT [FK_CoverageItem_InterestRiskID] FOREIGN KEY([InterestRiskID]) REFERENCES [dbo].[InterestRisk] ([InterestRiskID])
ALTER TABLE [dbo].[CoverageItem] WITH CHECK ADD  CONSTRAINT [FK_CoverageItem_InterestSubRiskID] FOREIGN KEY([InterestSubRiskID]) REFERENCES [dbo].[InterestSubRisk] ([InterestSubRiskID])
ALTER TABLE [dbo].[CoverageItem] WITH CHECK ADD  CONSTRAINT [FK_CoverageItem_PolicyCoverageID] FOREIGN KEY([PolicyCoverageID]) REFERENCES [dbo].[PolicyCoverage] ([PolicyCoverageID])
ALTER TABLE [dbo].[Field] WITH CHECK ADD  CONSTRAINT [FK_Field_FormatID] FOREIGN KEY([FormatID]) REFERENCES [dbo].[Format] ([FormatID])
--ALTER TABLE [dbo].[FieldRule] WITH CHECK ADD  CONSTRAINT [FK_FieldRule_FieldID] FOREIGN KEY([FieldID]) REFERENCES [dbo].[Field] ([FieldID])
--ALTER TABLE [dbo].[FieldRule] WITH CHECK ADD  CONSTRAINT [FK_FieldRule_RuleID] FOREIGN KEY([RuleID]) REFERENCES [dbo].[Rule] ([RuleID])
ALTER TABLE [dbo].[File] WITH CHECK ADD  CONSTRAINT [FK_File_FileTypeId] FOREIGN KEY([FileTypeId]) REFERENCES [dbo].[FileType] ([FileTypeId])
ALTER TABLE [dbo].[File] WITH CHECK ADD  CONSTRAINT [FK_File_LocationID] FOREIGN KEY([LocationID]) REFERENCES [dbo].[Location] ([LocationID])
ALTER TABLE [dbo].[File] WITH CHECK ADD  CONSTRAINT [FK_File_OwnerID] FOREIGN KEY([OwnerID]) REFERENCES [dbo].[Owner] ([OwnerID])
ALTER TABLE [dbo].[File] WITH CHECK ADD  CONSTRAINT [FK_File_SourceID] FOREIGN KEY([SourceID]) REFERENCES [dbo].[Source] ([SourceId])
ALTER TABLE [dbo].[FileResource] WITH CHECK ADD  CONSTRAINT [FK_FileResource_ResourceID] FOREIGN KEY([ResourceID]) REFERENCES [dbo].[Resource] ([ResourceID])
ALTER TABLE [dbo].[FileResource] WITH CHECK ADD  CONSTRAINT [FK_FileResource_FileID] FOREIGN KEY([FileID]) REFERENCES [dbo].[File] ([FileID])
ALTER TABLE [dbo].[InterestExposure] WITH CHECK ADD  CONSTRAINT [FK_InterestExposure_InterestSubRiskID] FOREIGN KEY([InterestSubRiskID]) REFERENCES [dbo].[InterestSubRisk] ([InterestSubRiskID])
ALTER TABLE [dbo].[InterestExposure] WITH CHECK ADD  CONSTRAINT [FK_InterestExposure_ProfileID] FOREIGN KEY([ProfileID]) REFERENCES [dbo].[Profile] ([ProfileID])
ALTER TABLE [dbo].[InterestExposureValues] WITH CHECK ADD  CONSTRAINT [FK_InterestExposureValues_InterestExposureID] FOREIGN KEY([InterestExposureID]) REFERENCES [dbo].[InterestExposure] ([InterestExposureID])
ALTER TABLE [dbo].[InterestGroup] WITH CHECK ADD  CONSTRAINT [FK_InterestGroup_ProfileID] FOREIGN KEY([ProfileID]) REFERENCES [dbo].[Profile] ([ProfileID])
ALTER TABLE [dbo].[InterestGroup] WITH CHECK ADD  CONSTRAINT [FK_InterestGroup_ProgID] FOREIGN KEY([ProgID]) REFERENCES [dbo].[Prog] ([ProgID])
ALTER TABLE [dbo].[InterestGroup] WITH CHECK ADD  CONSTRAINT [FK_InterestGroup_ScheduleID] FOREIGN KEY([ScheduleID]) REFERENCES [dbo].[Schedule] ([ScheduleID])
ALTER TABLE [dbo].[InterestGroupValues] WITH CHECK ADD  CONSTRAINT [FK_InterestGroupValues_InterestGroupID] FOREIGN KEY([InterestGroupID]) REFERENCES [dbo].[InterestGroup] ([InterestGroupID])
ALTER TABLE [dbo].[InterestRisk] WITH CHECK ADD  CONSTRAINT [FK_InterestRisk_InterestGroupID] FOREIGN KEY([InterestGroupID]) REFERENCES [dbo].[InterestGroup] ([InterestGroupID])
ALTER TABLE [dbo].[InterestRisk] WITH CHECK ADD  CONSTRAINT [FK_InterestRisk_ProfileID] FOREIGN KEY([ProfileID]) REFERENCES [dbo].[Profile] ([ProfileID])
ALTER TABLE [dbo].[InterestRiskValues] WITH CHECK ADD  CONSTRAINT [FK_InterestRiskValues_InterestRiskID] FOREIGN KEY([InterestRiskID]) REFERENCES [dbo].[InterestRisk] ([InterestRiskID])
ALTER TABLE [dbo].[InterestSubRisk] WITH CHECK ADD  CONSTRAINT [FK_InterestSubRisk_InterestRiskID] FOREIGN KEY([InterestRiskID]) REFERENCES [dbo].[InterestRisk] ([InterestRiskID])
ALTER TABLE [dbo].[InterestSubRisk] WITH CHECK ADD  CONSTRAINT [FK_InterestSubRisk_ProfileID] FOREIGN KEY([ProfileID]) REFERENCES [dbo].[Profile] ([ProfileID])
ALTER TABLE [dbo].[InterestSubRiskValues] WITH CHECK ADD  CONSTRAINT [FK_InterestSubRiskValues_InterestSubRiskID] FOREIGN KEY([InterestSubRiskID]) REFERENCES [dbo].[InterestSubRisk] ([InterestSubRiskID])
ALTER TABLE [dbo].[Model] WITH CHECK ADD  CONSTRAINT [FK_Model_ModelFamilyID] FOREIGN KEY([ModelFamilyID]) REFERENCES [dbo].[ModelFamily] ([ModelFamilyID])
ALTER TABLE [dbo].[Model] WITH CHECK ADD  CONSTRAINT [FK_Model_ModelTypeID] FOREIGN KEY([ModelTypeID]) REFERENCES [dbo].[ModelType] ([ModelTypeID])
ALTER TABLE [dbo].[ModelCoverageType] WITH CHECK ADD  CONSTRAINT [FK_ModelCoverageType_ModelID] FOREIGN KEY([ModelID]) REFERENCES [dbo].[Model] ([ModelID])
ALTER TABLE [dbo].[ModelCoverageType] WITH CHECK ADD  CONSTRAINT [FK_ModelCoverageType_CoverageTypeID] FOREIGN KEY([CoverageTypeID]) REFERENCES [dbo].[CoverageType] ([CoverageTypeID])
ALTER TABLE [dbo].[ModelFamily] WITH CHECK ADD  CONSTRAINT [FK_ModelFamily_SupplierID] FOREIGN KEY([SupplierID]) REFERENCES [dbo].[Supplier] ([SupplierID])
ALTER TABLE [dbo].[ModelLicense] WITH CHECK ADD  CONSTRAINT [FK_ModelLicense_CompanyID] FOREIGN KEY([CompanyID]) REFERENCES [dbo].[Company] ([CompanyID])
ALTER TABLE [dbo].[ModelLicense] WITH CHECK ADD  CONSTRAINT [FK_ModelLicense_ModelID] FOREIGN KEY([ModelID]) REFERENCES [dbo].[Model] ([ModelID])
ALTER TABLE [dbo].[ModelPeril] WITH CHECK ADD  CONSTRAINT [FK_ModelPeril_ModelID] FOREIGN KEY([ModelID]) REFERENCES [dbo].[Model] ([ModelID])
ALTER TABLE [dbo].[ModelPeril] WITH CHECK ADD  CONSTRAINT [FK_ModelPeril_PerilID] FOREIGN KEY([PerilID]) REFERENCES [dbo].[Peril] ([PerilID])
ALTER TABLE [dbo].[ModelResource] WITH CHECK ADD  CONSTRAINT [FK_ModelResource_ModelID] FOREIGN KEY([ModelID]) REFERENCES [dbo].[Model] ([ModelID])
ALTER TABLE [dbo].[ModelResource] WITH CHECK ADD  CONSTRAINT [FK_ModelResource_OasisSystemID] FOREIGN KEY([OasisSystemID]) REFERENCES [dbo].[OasisSystem] ([OasisSystemID])
ALTER TABLE [dbo].[OasisUser] WITH CHECK ADD  CONSTRAINT [FK_OasisUser_ModelLicenseID] FOREIGN KEY([ModelLicenseID]) REFERENCES [dbo].[ModelLicense] ([ModelLicenseID])
ALTER TABLE [dbo].[OasisUser] WITH CHECK ADD  CONSTRAINT [FK_OasisUser_OasisSystemID] FOREIGN KEY([OasisSystemID]) REFERENCES [dbo].[OasisSystem] ([OasisSystemID])
ALTER TABLE [dbo].[OasisSystemService] WITH CHECK ADD  CONSTRAINT [FK_OasisSystemService_OasisSystemID] FOREIGN KEY([OasisSystemID]) REFERENCES [dbo].[OasisSystem] ([OasisSystemID])
ALTER TABLE [dbo].[OasisSystemService] WITH CHECK ADD  CONSTRAINT [FK_OasisSystemService_ServiceID] FOREIGN KEY([ServiceID]) REFERENCES [dbo].[Service] ([ServiceID])
ALTER TABLE [dbo].[Owner] WITH CHECK ADD  CONSTRAINT [FK_Owner_BFEUserID] FOREIGN KEY([BFEUserID]) REFERENCES [dbo].[BFEUser] ([BFEUserID])
ALTER TABLE [dbo].[Policy] WITH CHECK ADD  CONSTRAINT [FK_Policy_PolicyTypeID] FOREIGN KEY([PolicyTypeID]) REFERENCES [dbo].[PolicyType] ([PolicyTypeID])
ALTER TABLE [dbo].[Policy] WITH CHECK ADD  CONSTRAINT [FK_Policy_ProfileID] FOREIGN KEY([ProfileID]) REFERENCES [dbo].[Profile] ([ProfileID])
ALTER TABLE [dbo].[Policy] WITH CHECK ADD  CONSTRAINT [FK_Policy_ProgID] FOREIGN KEY([ProgID]) REFERENCES [dbo].[Prog] ([ProgID])
ALTER TABLE [dbo].[PolicyCoverage] WITH CHECK ADD  CONSTRAINT [FK_PolicyCoverage_CoverageTypeID] FOREIGN KEY([CoverageTypeID]) REFERENCES [dbo].[CoverageType] ([CoverageTypeID])
ALTER TABLE [dbo].[PolicyCoverage] WITH CHECK ADD  CONSTRAINT [FK_PolicyCoverage_PolicyPerilID] FOREIGN KEY([PolicyPerilID]) REFERENCES [dbo].[PolicyPeril] ([PolicyPerilID])
ALTER TABLE [dbo].[PolicyCoverage] WITH CHECK ADD  CONSTRAINT [FK_PolicyCoverage_ProfileID] FOREIGN KEY([ProfileID]) REFERENCES [dbo].[Profile] ([ProfileID])
ALTER TABLE [dbo].[PolicyCoverageValues] WITH CHECK ADD  CONSTRAINT [FK_PolicyCoverageValues_PolicyCoverageID] FOREIGN KEY([PolicyCoverageID]) REFERENCES [dbo].[PolicyCoverage] ([PolicyCoverageID])
ALTER TABLE [dbo].[Profile] WITH CHECK ADD  CONSTRAINT [FK_Profile_ProfileTypeID] FOREIGN KEY([ProfileTypeID]) REFERENCES [dbo].[ProfileType] ([ProfileTypeID])
ALTER TABLE [dbo].[ProfileResource] WITH CHECK ADD  CONSTRAINT [FK_ProfileResource_ProfileID] FOREIGN KEY([ProfileID]) REFERENCES [dbo].[Profile] ([ProfileID])
ALTER TABLE [dbo].[ProfileResource] WITH CHECK ADD  CONSTRAINT [FK_ProfileResource_ResourceID] FOREIGN KEY([ResourceID]) REFERENCES [dbo].[Resource] ([ResourceID])
ALTER TABLE [dbo].[ProfileElement] WITH CHECK ADD  CONSTRAINT [FK_ProfileElement_FieldID] FOREIGN KEY([FieldID]) REFERENCES [dbo].[Field] ([FieldID])
ALTER TABLE [dbo].[ProfileElement] WITH CHECK ADD  CONSTRAINT [FK_ProfileElement_ProfileID] FOREIGN KEY([ProfileID]) REFERENCES [dbo].[Profile] ([ProfileID])
ALTER TABLE [dbo].[ProfileElement] WITH CHECK ADD  CONSTRAINT [FK_ProfileElement_TableID] FOREIGN KEY([TableID]) REFERENCES [dbo].[Table] ([TableID])
--ALTER TABLE [dbo].[ProfileElementRule] WITH CHECK ADD  CONSTRAINT [FK_ProfileElementRule_ProfileElementID] FOREIGN KEY([ProfileElementID]) REFERENCES [dbo].[ProfileElement] ([ProfileElementID])
--ALTER TABLE [dbo].[ProfileElementRule] WITH CHECK ADD  CONSTRAINT [FK_ProfileElementRule_RuleID] FOREIGN KEY([RuleID]) REFERENCES [dbo].[Rule] ([RuleID])
ALTER TABLE [dbo].[ProfileValueDetail] WITH CHECK ADD  CONSTRAINT [FK_ProfileValueDetail_ProfileElementID] FOREIGN KEY([ProfileElementID]) REFERENCES [dbo].[ProfileElement] ([ProfileElementID])
ALTER TABLE [dbo].[ProfileValueDetail] WITH CHECK ADD  CONSTRAINT [FK_ProfileValueDetail_PerilID] FOREIGN KEY([PerilID]) REFERENCES [dbo].[ModelPeril] ([ModelPerilID])
ALTER TABLE [dbo].[ProfileValueDetail] WITH CHECK ADD  CONSTRAINT [FK_ProfileValueDetail_CoverageTypeID] FOREIGN KEY([CoverageTypeID]) REFERENCES [dbo].[ModelCoverageType] ([ModelCoverageTypeID])
ALTER TABLE [dbo].[Prog] WITH CHECK ADD  CONSTRAINT [FK_Prog_AccountID] FOREIGN KEY([AccountID]) REFERENCES [dbo].[Account] ([AccountID])
ALTER TABLE [dbo].[Prog] WITH CHECK ADD  CONSTRAINT [FK_Prog_TransformID] FOREIGN KEY([TransformID]) REFERENCES [dbo].[Transform] ([TransformID])
ALTER TABLE [dbo].[ProgOasis] WITH CHECK ADD  CONSTRAINT [FK_ProgOasis_ModelId] FOREIGN KEY([ModelId]) REFERENCES [dbo].[Model] ([ModelID])
ALTER TABLE [dbo].[ProgOasis] WITH CHECK ADD  CONSTRAINT [FK_ProgOasis_ProgId] FOREIGN KEY([ProgId]) REFERENCES [dbo].[Prog] ([ProgID])
ALTER TABLE [dbo].[ProgOasis] WITH CHECK ADD  CONSTRAINT [FK_ProgOasis_SourceFileId] FOREIGN KEY([SourceFileId]) REFERENCES [dbo].[File] ([FileId])
ALTER TABLE [dbo].[ProgOasis] WITH CHECK ADD  CONSTRAINT [FK_ProgOasis_TransformID] FOREIGN KEY([TransformID]) REFERENCES [dbo].[Transform] ([TransformID])
ALTER TABLE [dbo].[ProgOasisKeys] WITH CHECK ADD  CONSTRAINT [FK_ProgOasisKeys_ProgOasisId] FOREIGN KEY([ProgOasisId]) REFERENCES [dbo].[ProgOasis] ([ProgOasisId])
ALTER TABLE [dbo].[Resource] WITH CHECK ADD  CONSTRAINT [FK_Resource_ResourceTypeID] FOREIGN KEY([ResourceTypeID]) REFERENCES [dbo].[ResourceType] ([ResourceTypeID])
ALTER TABLE [dbo].[Schedule] WITH CHECK ADD  CONSTRAINT [FK_Schedule_ProfileID] FOREIGN KEY([ProfileID]) REFERENCES [dbo].[Profile] ([ProfileID])
ALTER TABLE [dbo].[ScheduleValues] WITH CHECK ADD  CONSTRAINT [FK_ScheduleValues_ScheduleID] FOREIGN KEY([ScheduleID]) REFERENCES [dbo].[Schedule] ([ScheduleID])
ALTER TABLE [dbo].[SecurityGroupResource] WITH CHECK ADD  CONSTRAINT [FK_SecurityGroupResource_ResourceID] FOREIGN KEY([ResourceID]) REFERENCES [dbo].[Resource] ([ResourceID])
ALTER TABLE [dbo].[SecurityGroupResource] WITH CHECK ADD  CONSTRAINT [FK_SecurityGroupResource_SecurityGroupID] FOREIGN KEY([SecurityGroupID]) REFERENCES [dbo].[SecurityGroup] ([SecurityGroupID])
ALTER TABLE [dbo].[Service] WITH CHECK ADD  CONSTRAINT [FK_Service_ServiceTypeID] FOREIGN KEY([ServiceTypeID]) REFERENCES [dbo].[ServiceType] ([ServiceTypeID])
ALTER TABLE [dbo].[Transform] WITH CHECK ADD  CONSTRAINT [FK_TransformTypeID] FOREIGN KEY([TransformTypeID]) REFERENCES [dbo].[TransformType] ([TransformTypeID])
ALTER TABLE [dbo].[UserLicense] WITH CHECK ADD  CONSTRAINT [FK_UserLicense_BFEUserID] FOREIGN KEY([BFEUserID]) REFERENCES [dbo].[BFEUser] ([BFEUserID])
ALTER TABLE [dbo].[UserLicense] WITH CHECK ADD  CONSTRAINT [FK_UserLicense_OasisUserID] FOREIGN KEY([OasisUserID]) REFERENCES [dbo].[OasisUser] ([OasisUserID])
ALTER TABLE [dbo].[UserSecurityGroup] WITH CHECK ADD  CONSTRAINT [FK_UserSecurityGroup_BFEUserID] FOREIGN KEY([BFEUserID]) REFERENCES [dbo].[BFEUser] ([BFEUserID])
ALTER TABLE [dbo].[UserSecurityGroup] WITH CHECK ADD  CONSTRAINT [FK_UserSecurityGroup_Security Group ID] FOREIGN KEY([Security Group ID]) REFERENCES [dbo].[SecurityGroup] ([SecurityGroupID])
ALTER TABLE [dbo].[Parameter] WITH CHECK ADD  CONSTRAINT [FK_Parameter_ActionID] FOREIGN KEY([ActionID]) REFERENCES [dbo].[Action] ([ActionID])
ALTER TABLE [dbo].[WorkflowElement] WITH CHECK ADD  CONSTRAINT [FK_WorkflowElement_ActionID] FOREIGN KEY([ActionID]) REFERENCES [dbo].[Action] ([ActionID])
ALTER TABLE [dbo].[WorkflowElement] WITH CHECK ADD  CONSTRAINT [FK_WorkflowElement_WorkflowID] FOREIGN KEY([WorkflowID]) REFERENCES [dbo].[Workflow] ([WorkflowID])
ALTER TABLE [dbo].[WorkflowParameter] WITH CHECK ADD  CONSTRAINT [FK_WorkflowParameter_ParameterID] FOREIGN KEY([ParameterID]) REFERENCES [dbo].[Parameter] ([ParameterID])
ALTER TABLE [dbo].[WorkflowParameter] WITH CHECK ADD  CONSTRAINT [FK_WorkflowParameter_WorkflowElementID] FOREIGN KEY([WorkflowElementID]) REFERENCES [dbo].[WorkflowElement] ([WorkflowElementID])
ALTER TABLE [dbo].[ParameterRun] WITH CHECK ADD  CONSTRAINT [FK_ParameterRun_ParameterID] FOREIGN KEY([WorkflowParameterID]) REFERENCES [dbo].[WorkflowParameter] ([WorkflowParameterID])
ALTER TABLE [dbo].[ParameterRun] WITH CHECK ADD  CONSTRAINT [FK_ParameterRun_ElementRunID] FOREIGN KEY([ElementRunID]) REFERENCES [dbo].[ElementRun] ([ElementRunID])
ALTER TABLE [dbo].[ElementRun] WITH CHECK ADD  CONSTRAINT [FK_ElementRun_WorkflowElementID] FOREIGN KEY([WorkflowElementID]) REFERENCES [dbo].[WorkflowElement] ([WorkflowElementID])
ALTER TABLE [dbo].[ElementRun] WITH CHECK ADD  CONSTRAINT [FK_ElementRun_ProcessRunID] FOREIGN KEY([ProcessRunID]) REFERENCES [dbo].[ProcessRun] ([ProcessRunID])
ALTER TABLE [dbo].[ProcessRun] WITH CHECK ADD  CONSTRAINT [FK_ProcessRun_ProgOasisID] FOREIGN KEY([ProgOasisID]) REFERENCES [dbo].[ProgOasis] ([ProgOasisID])
ALTER TABLE [dbo].[OutputRun] WITH CHECK ADD  CONSTRAINT [FK_OutputRun_ElementRunID] FOREIGN KEY([ElementRunID]) REFERENCES [dbo].[ElementRun] ([ElementRunID])
ALTER TABLE [dbo].[OutputRun] WITH CHECK ADD  CONSTRAINT [FK_OutputRun_SummaryLevelRunID] FOREIGN KEY([SummaryLevelRunID]) REFERENCES [dbo].[SummaryLevelRun] ([SummaryLevelRunID])
ALTER TABLE [dbo].[OutputRun] WITH CHECK ADD  CONSTRAINT [FK_OutputRun_OutputTypeID] FOREIGN KEY([OutputTypeID]) REFERENCES [dbo].[OutputType] ([OutputTypeID])
ALTER TABLE [dbo].[SummaryLevelRun] WITH CHECK ADD  CONSTRAINT [FK_SummaryLevelRun_ProcessRunID] FOREIGN KEY([ProcessRunID]) REFERENCES [dbo].[ProcessRun] ([ProcessRunID])
ALTER TABLE [dbo].[SummaryLevelRun] WITH CHECK ADD  CONSTRAINT [FK_SummaryLevelRun_SummaryLevelID] FOREIGN KEY([SummaryLevelID]) REFERENCES [dbo].[SummaryLevel] ([SummaryLevelID])
ALTER TABLE [dbo].[OutputType] WITH CHECK ADD  CONSTRAINT [FK_OutputType_PerspectiveID] FOREIGN KEY([PerspectiveID]) REFERENCES [dbo].[Perspective] ([PerspectiveID])
ALTER TABLE [dbo].[OutputType] WITH CHECK ADD  CONSTRAINT [FK_OutputType_SummaryLevelID] FOREIGN KEY([SummaryLevelID]) REFERENCES [dbo].[SummaryLevel] ([SummaryLevelID])
ALTER TABLE [dbo].[OutputType] WITH CHECK ADD  CONSTRAINT [FK_OutputType_AnalysisTypeID] FOREIGN KEY([AnalysisTypeID]) REFERENCES [dbo].[AnalysisType] ([AnalysisTypeID])
ALTER TABLE [dbo].[ElementRunLog] WITH CHECK ADD  CONSTRAINT [FK_ElementRunLog_ElementRunID] FOREIGN KEY([ElementRunID]) REFERENCES [dbo].[ElementRun] ([ElementRunID])
ALTER TABLE [dbo].[ModelTransform] WITH CHECK ADD  CONSTRAINT [FK_ModelTransform_ModelID] FOREIGN KEY([ModelID]) REFERENCES [dbo].[Model] ([ModelID])
ALTER TABLE [dbo].[ModelTransform] WITH CHECK ADD  CONSTRAINT [FK_ModelTransform_TransformID] FOREIGN KEY([TransformID]) REFERENCES [dbo].[Transform] ([TransformID])
GO
-------------------------------------------------------------------------------
--Indexes
-------------------------------------------------------------------------------
CREATE INDEX Idx_InterestGroup_ScheduleID ON InterestGroup (ScheduleID);
CREATE INDEX Idx_InterestRisk_InterestGroupID ON InterestRisk (InterestGroupID);
CREATE INDEX Idx_InterestSubRisk_InterestRiskID ON InterestSubRisk (InterestRiskID);
CREATE INDEX Idx_InterestExposure_InterestSubRiskID ON InterestExposure (InterestSubRiskID);
CREATE INDEX Idx_ScheduleValues_ScheduleID ON ScheduleValues (ScheduleID);
CREATE INDEX Idx_InterestGroupValues_InterestGroupID ON InterestGroupValues (InterestGroupID);
CREATE INDEX Idx_InterestRiskValues_InterestRiskID ON InterestRiskValues (InterestRiskID);
CREATE INDEX Idx_InterestSubRiskValues_InterestSubRiskID ON InterestSubRiskValues (InterestSubRiskID);
CREATE INDEX Idx_InterestExposureValues_InterestExposureID ON InterestExposureValues (InterestExposureID);
CREATE INDEX Idx_PolicyCoverage_PolicyPerilID ON PolicyCoverage (PolicyPerilID);
CREATE INDEX Idx_PolicyPeril_PolicyID ON PolicyPeril (PolicyID);
CREATE INDEX Idx_PolicyValues_PolicyID ON PolicyValues (PolicyID);
CREATE INDEX Idx_PolicyCoverageValues_PolicyCoverageID ON PolicyCoverageValues (PolicyCoverageID);
CREATE INDEX Idx_CoverageItem_InterestRiskID ON CoverageItem (InterestRiskID);
CREATE INDEX Idx_CoverageItem_InterestSubRiskID ON CoverageItem (InterestSubRiskID);
CREATE INDEX Idx_CoverageItem_InterestExposureID ON CoverageItem (InterestExposureID);
CREATE INDEX Idx_ProgOasisKeys_ProgOasisID ON ProgOasisKeys (ProgOasisID);
CREATE INDEX Idx_ProgOasis_ProgID ON ProgOasis (ProgID);
CREATE INDEX Idx_ProgOasisKeys_LocID ON ProgOasisKeys (LocID);
CREATE INDEX Idx_ProgOasisKeys_PerilID ON ProgOasisKeys (PerilID);
CREATE INDEX Idx_ProgOasisKeys_CoverageID ON ProgOasisKeys (CoverageID);
--CREATE INDEX Idx_CorrelationItem_InterestRiskID ON CorrelationItem (InterestRiskID);
--CREATE INDEX Idx_CorrelationItem_InterestSubRiskID ON CorrelationItem (InterestSubRiskID);
--CREATE INDEX Idx_CorrelationItem_InterestExposureID ON CorrelationItem (InterestExposureID);
--CREATE INDEX Idx_CorrelationItem_CorrelationID ON CorrelationItem (CorrelationID);
CREATE INDEX Idx_Policy_ProgID ON Policy (ProgID);
CREATE INDEX Idx_InterestGroup_ProgID ON InterestGroup (ProgID);
CREATE CLUSTERED INDEX Idx_ItemOrder ON OasisItems (Item_ID)
CREATE CLUSTERED INDEX Idx_CoverageOrder ON OasisCoverages (Coverage_ID)
CREATE CLUSTERED INDEX Idx_GulSummaryXrefOrder ON OasisGulSummaryXref (SummarySet_ID, Coverage_ID)
CREATE CLUSTERED INDEX Idx_FMProgrammeOrder ON OasisFM_Programme (Level_ID, From_Agg_ID)
CREATE CLUSTERED INDEX Idx_FMPolicyTC ON OasisFM_PolicyTC (Level_ID, Agg_ID, Layer_ID)
CREATE CLUSTERED INDEX Idx_FMProfile ON OasisFM_Profile (PolicyTC_ID)
CREATE CLUSTERED INDEX Idx_FMXref ON OasisFM_Xref (Output_ID)
CREATE CLUSTERED INDEX Idx_FMSummaryXref ON OasisFMSummaryXref (SummarySet_ID, Output_ID)
GO
-------------------------------------------------------------------------------
--Stored Procedures
-------------------------------------------------------------------------------


/****** Object:  StoredProcedure [dbo].[updateLogDatabaseUsage]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[updateLogDatabaseUsage] 
	(
	@ProcedureName nvarchar(255),
	@ParameterList nvarchar(2500) = NULL,
	@LogTimestamp datetime
	)
AS
SET NOCOUNT ON;
--declare @LogID int = (Select isnull(max(LogID),0) From LogDatabaseUsage) +1
insert into LogDatabaseUsage (--LogID,
				ProcedureName,ParameterList,LogTimestamp)
select	--@logid,
	@ProcedureName,@ParameterList,@LogTimestamp
--select @LogID
GO



/****** Object:  StoredProcedure [dbo].[addSecurityGroup]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE procedure [dbo].[addSecurityGroup] @BFEUserID int, @SecurityGroupID int
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()
exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------
DECLARE	@UserSecurityGroupID int = (select isnull(max(UserSecurityGroupID),0)+1 from UserSecurityGroup)
IF	@SecurityGroupID = 0
	BEGIN
		DELETE FROM [dbo].[UserSecurityGroup]
		WHERE		BFEUserID = @BFEUserID
		INSERT INTO [dbo].[UserSecurityGroup]
		SELECT		@UserSecurityGroupID + ROW_NUMBER() over (Order By SecurityGroupID) AS UserSecurityGroupID,
					@BFEUserID,
					SecurityGroupID
		FROM		SecurityGroup
	END
ELSE
	BEGIN
		INSERT INTO		[dbo].[UserSecurityGroup]
		SELECT			@UserSecurityGroupID,@BFEUserID, @SecurityGroupID
	END
GO

/****** Object:  StoredProcedure [dbo].[BFELogin]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[BFELogin] @username	nvarchar(255), @pwd nvarchar(255)
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = '@username = ' + @username 
declare	@LogTimestamp datetime = getdate()
exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------
BEGIN
	DECLARE	@userid	int = -1
	SELECT	@userid = BFEUserID 
	FROM	dbo.BFEUser
	WHERE	BFEUserLogin = @username 
	AND		BFEUserPassword = HASHBYTES('SHA2_256',@pwd)
	
	SELECT	@userid
END
GO

/****** Object:  StoredProcedure [dbo].[createAccount]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[createAccount] @AccountName NVARCHAR(255)
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = '@AccountName = ' + @AccountName
declare	@LogTimestamp datetime = getdate()
exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------
DECLARE		@AccountID int= (select isnull(max(AccountID),0) + 1 FROM Account)

--check for existing name
if exists (select AccountName from Account where AccountName = @AccountName)
begin
	set @AccountName = @AccountName + ': version ' + convert(nvarchar,getdate(),113)
end

INSERT INTO Account(AccountID,AccountName,Deleted)
SELECT		@AccountID, @AccountName, 0
GO

/****** Object:  StoredProcedure [dbo].[createAPIErrorFileRecord]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[createAPIErrorFileRecord] @FileName nvarchar(255), @ProgOasisId int
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()
exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------
--get next incremental fileid, resourceid and identify resourceid for return file for progoasis
Declare	@FileId int				= (Select isnull(max(FileId)+1,1) From [File])
Declare	@LegacyResourceId int	= (Select isnull(max(ResourceId)+1,1) From [Resource])
Declare	@ResourceId int			= (Select ResourceId From [Resource] Where ResourceTable = 'ProgOasis' And ResourceKey = @ProgOasisId And ResourceTypeId = 108)
Declare	@FileResourceId int		= (Select isnull(max(FileResourceId)+1,1) From [FileResource])
--generate legacy resource record
Insert Into  [Resource] (ResourceID,ResourceTable,ResourceKey,ResourceQualifier,ResourceTypeID)
Select		@LegacyResourceId,
			'ProgOasis',
			@ProgOasisId,
			NULL,
			208 --Legacy Lookup Error File
--insert file record for new file
Insert Into [File] (FileId, [FileName],FileDesc,SourceID,OwnerID,LocationID,DateTimeCreated,
					DateTimeUpdated,DateTimeDeleted,OwnerNameCreated,OwnerNameUpdated,OwnerNameDeleted,
					FileTypeId)
Select		@FileId,
			@FileName,
			'Model Lookup Error File',
			1,
			1,
			102, --Output Files for API
			getdate(),
			getdate(),
			null,
			'API1b',
			'API1b',
			NULL,
			108 -- file type Lookup Return Key File
--set existing file record to legacy resource id
Update [FileResource] Set ResourceID = @LegacyResourceId Where ResourceID = @ResourceId
Insert Into [FileResource] Select @FileResourceId,@FileId,@ResourceId
--update progoasisrecord
Update ProgOasis Set API1bDateTime = getdate() Where ProgOasisID = @ProgOasisID
GO
/****** Object:  StoredProcedure [dbo].[createCompany]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[createCompany] 
	@CompanyName nvarchar(255), 
	@CompanyDomicile nvarchar(255),
	@CompanyLegalName nvarchar(255),
	@CompanyRegistrationNo nvarchar(255)
AS
BEGIN
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()
exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------
DECLARE @CompanyID int   =(SELECT isnull(Max(CompanyID),0) + 1 From dbo.Company)
INSERT INTO [dbo].[Company]
           ([CompanyID],[CompanyName],[CompanyDomicile],[CompanyLegalName],[CompanyRegistrationNo], [Deleted])
     VALUES
           (@CompanyID, @CompanyName, @CompanyDomicile, @CompanyLegalName,@CompanyRegistrationNo, 0)
SELECT @CompanyID
END
GO

/****** Object:  StoredProcedure [dbo].[createFileRecord]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[createFileRecord] @FileName nvarchar(255), @FileDesc nvarchar(255), @FileTypeID int, @LocationPathUnix nvarchar(255), @BFEUserID int, @ResourceTable nvarchar(255), @ResourceKey int
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()
exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------
DECLARE	@FileId int					= (SELECT isnull(Max(FileId),0) + 1 From [File])
DECLARE	@FileResourceId int			= (SELECT isnull(Max(FileResourceId),0) + 1 From [FileResource])
DECLARE @LegacyResourceId int       = (SELECT isnull(MAX(ResourceId),0) + 1 FROM [Resource])
DECLARE @ResourceId int				= (SELECT ResourceId FROM [Resource] WHERE ResourceTable = @ResourceTable And ResourceKey = @ResourceKey And ResourceTypeId = 
																	Case 
																		When @FileTypeID = 101 THEN 101
																		When @FileTypeID = 102 THEN 102
																		End)
DECLARE @LocationID int             = (SELECT LocationID from dbo.Location where LocationPathUnix = @LocationPathUnix)
DECLARE @BFEUserName nvarchar(255)	= (SELECT BFEUserName from dbo.BFEUser where BFEUserID = @BFEUserID)
DECLARE	@LegacyResourceTypeID int	= (SELECT Case 
												When @FileTypeID = 101 THEN 201
												When @FileTypeID = 102 THEN 202
												END
									  )
--NewResourceRecord
IF exists (select FileID From [FileResource] Where ResourceId = @ResourceId)
	begin
		Insert Into [Resource] (ResourceId, ResourceTable,ResourceKey, ResourceQualifier, ResourceTypeId)
				Values (@LegacyResourceId,@ResourceTable,@ResourceKey,NULL,@LegacyResourceTypeID) 
		Update	[FileResource] SET ResourceId = @LegacyResourceId WHERE ResourceId = @ResourceId
	end
Insert Into [File] (FileId, [FileName],FileDesc,SourceID,OwnerID,LocationID,DateTimeCreated,DateTimeUpdated,DateTimeDeleted,OwnerNameCreated,OwnerNameUpdated,OwnerNameDeleted,FileTypeId)
		Values(@FileId, @FileName, @FileDesc , 1, 1, @LocationID, getdate(), getdate(), NULL, @BFEUserName, @BFEUserName, NULL, @FileTypeID)
Insert Into [FileResource] (FileResourceId, FileId, ResourceId) Select @FileResourceId, @FileId, @ResourceId
SELECT @FileId
GO

/****** Object:  StoredProcedure [dbo].[createModelResource]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[createModelResource] 
	@ModelResourceName  nvarchar(255),  
	@ResourceTypeID int,
	@OasisSystemID int,
	@ModelID int,
	@ModelResourceValue nvarchar(255)
AS
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()
exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------
BEGIN
	SET NOCOUNT ON;
	DECLARE	@ModelResourceID int					= (SELECT isnull(Max(ModelResourceID),0) + 1 From dbo.ModelResource)
	INSERT INTO [dbo].[ModelResource]
           ([ModelResourceID],[ModelResourceName],[ResourceTypeID],[OasisSystemID],[ModelID],[ModelResourceValue])
     VALUES
           (@ModelResourceID,@ModelResourceName,@ResourceTypeID,@OasisSystemID,@ModelID,@ModelResourceValue)
	SELECT @ModelResourceID
END
GO
/****** Object:  StoredProcedure [dbo].[createNewUser]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE procedure [dbo].[createNewUser] @BFEUserName NVARCHAR(255), @CompanyID int, @BFEUserLogin NVARCHAR(255), @BFEUserPassword NVARCHAR(255), @BFEUserDept NVARCHAR(255)
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()
exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------
DECLARE @UserId int = (SELECT isnull(MAX(BFEUSERID),0) + 1 FROM dbo.BFEUSER)
INSERT INTO BFEUser (BFEUserID, BFEUserName, CompanyID, BFEUserLogin, BFEUserPassword, BFEUserDept, Deleted)
SELECT	@UserId, @BFEUserName, @CompanyID, @BFEUserLogin, HASHBYTES('SHA2_256',@BFEUserPassword), @BFEUserDept, 0
SELECT	@UserId
GO

/****** Object:  StoredProcedure [dbo].[createProg]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[createProg] @ProgName nvarchar(255), @AccountID int, @TransformID int
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = '@ProgName = ' + @ProgName + ', @AccountID = ' + convert(nvarchar,@AccountID) + ', @TransformID = ' + convert(nvarchar,@TransformID)
declare	@LogTimestamp datetime = getdate()
exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------
DECLARE @ProgID int		= (SELECT isnull(MAX(ProgID)	,0) + 1 FROM [Prog])
DECLARE @ResourceId int = (SELECT isnull(MAX(ResourceId),0) + 1 FROM [Resource])
DECLARE @FileId int		= (SELECT isnull(MAX(FileId)	,0) + 1 FROM [File])

--blank prog name
if @ProgName = ''
begin
	set @ProgName = 'Prog ' + convert(nvarchar(255),@ProgID)
end

--check for existing name
if exists (select ProgName from Prog where ProgName = @ProgName)
begin
	set @ProgName = @ProgName + ': version ' + convert(nvarchar,getdate(),113)
end

INSERT INTO Prog(ProgID, ProgName, AccountID, TransformID, Deleted, [Status])
SELECT	@ProgID, @ProgName, @AccountID, @TransformID, 0, 'Created'
--Source Location File
Insert Into Resource Values (@ResourceId,'Prog',@ProgID,NULL,101) 
Set	@ResourceId =  @ResourceId+1
--Source Account File
Insert Into Resource Values (@ResourceId,'Prog',@ProgID,NULL,102) 
Set	@ResourceId =  @ResourceId+1
--Canonical Location File
Insert Into Resource Values (@ResourceId,'Prog',@ProgID,NULL,103) 
Set	@ResourceId =  @ResourceId+1
--Canonical Account File
Insert Into Resource Values (@ResourceId,'Prog',@ProgID,NULL,104) 
Set	@ResourceId =  @ResourceId+1
GO
/****** Object:  StoredProcedure [dbo].[createProgOasis]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[createProgOasis] @ProgID int, @ModelID int, @TransformID int
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()
exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------
DECLARE @ProgOasisID int = (SELECT isnull(MAX(ProgOasisID)	,0) + 1 FROM [ProgOasis])
DECLARE @ResourceID int = (SELECT isnull(MAX(ResourceID)	,0) + 1 FROM [Resource])
INSERT INTO ProgOasis(ProgOasisID, ProgId, ModelId, TransformID,[Status])
	VALUES (@ProgOasisID,@ProgID,@ModelID, @TransformID,'Created')
--Model Lookup File
Insert Into Resource Values (@ResourceId,'ProgOasis',@ProgOasisID,NULL,105) 
Set	@ResourceId =  @ResourceId+1
--Lookup Return File
Insert Into Resource Values (@ResourceId,'ProgOasis',@ProgOasisID,NULL,106) 
Set	@ResourceId =  @ResourceId+1
--Lookup Return Non Match File
Insert Into Resource Values (@ResourceId,'ProgOasis',@ProgOasisID,NULL,107) 
Set	@ResourceId =  @ResourceId+1
--Lookup Return Error File
Insert Into Resource Values (@ResourceId,'ProgOasis',@ProgOasisID,NULL,108) 
Set	@ResourceId =  @ResourceId+1
--Oasis Items File
Insert Into Resource Values (@ResourceId,'ProgOasis',@ProgOasisID,NULL,111) 
Set	@ResourceId =  @ResourceId+1
--Oasis Coverages File
Insert Into Resource Values (@ResourceId,'ProgOasis',@ProgOasisID,NULL,112) 
Set	@ResourceId =  @ResourceId+1
--Oasis Item Dictionary File
Insert Into Resource Values (@ResourceId,'ProgOasis',@ProgOasisID,NULL,113) 
Set	@ResourceId =  @ResourceId+1
--Oasis FM Programme File
Insert Into Resource Values (@ResourceId,'ProgOasis',@ProgOasisID,NULL,114) 
Set	@ResourceId =  @ResourceId+1
--Oasis FM Policy TC File
Insert Into Resource Values (@ResourceId,'ProgOasis',@ProgOasisID,NULL,115) 
Set	@ResourceId =  @ResourceId+1
--Oasis FM Policy Profile File
Insert Into Resource Values (@ResourceId,'ProgOasis',@ProgOasisID,NULL,116) 
Set	@ResourceId =  @ResourceId+1
--Oasis FM XRef File
Insert Into Resource Values (@ResourceId,'ProgOasis',@ProgOasisID,NULL,117) 
Set	@ResourceId =  @ResourceId+1
--Oasis FM Dict File
Insert Into Resource Values (@ResourceId,'ProgOasis',@ProgOasisID,NULL,127) 
Set	@ResourceId =  @ResourceId+1
select @progoasisid
GO
/****** Object:  StoredProcedure [dbo].[createSecurityGroup]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/****** Object:  StoredProcedure [dbo].[deleteAccount]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[deleteAccount] @AccountID int
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()
exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------
UPDATE	dbo.Account
SET		Deleted = 1,
		AccountName = AccountName + ': deleted ' + convert(nvarchar,getdate(),113)
WHERE	@AccountID=AccountID

UPDATE	dbo.Prog
SET		Deleted = 1,
		ProgName = ProgName + ': deleted ' + convert(nvarchar,getdate(),113)
WHERE	@AccountID=AccountID
And		Deleted = 0

SELECT	@AccountID 

GO
/****** Object:  StoredProcedure [dbo].[deleteCompany]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[deleteCompany] @companyid int
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()
exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------
UPDATE	dbo.Company
SET		Deleted = 1 
WHERE	CompanyID = @companyid
GO

/****** Object:  StoredProcedure [dbo].[deleteModelResource]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[deleteModelResource]
@ModResID int
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()
exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------
DELETE FROM dbo.ModelResource
WHERE ModelResourceID = @ModResID
Select @ModResID
GO

/****** Object:  StoredProcedure [dbo].[deleteProg]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[deleteProg] @ProgID int
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()
exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------
UPDATE	dbo.Prog
SET		Deleted = 1,
		ProgName = ProgName + ': deleted ' + convert(nvarchar,getdate(),113)
WHERE	ProgID=@ProgID
GO

/****** Object:  StoredProcedure [dbo].[deleteUser]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE procedure [dbo].[deleteUser] @BFEUserID int
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()
exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------
DELETE FROM [dbo].[UserSecurityGroup]
WHERE	BFEUserID = @BFEUserID
DELETE FROM UserLicense
WHERE	BFEUserID = @BFEUserID
UPDATE	dbo.BFEUser
SET		Deleted = 1 
WHERE	BFEUserID = @BFEUserID
SELECT	@BFEUserID
GO
/****** Object:  StoredProcedure [dbo].[generateCanonicalModel]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE Procedure [dbo].[generateCanonicalModel] @ProgId int
AS
SET NOCOUNT ON;
SET ANSI_NULLS ON;
SET ANSI_WARNINGS ON;
--declare @ProgId int = 1
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = '@ProgID = ' + convert(nvarchar,@ProgID)
declare	@LogTimestamp datetime = getdate()
exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------
--Declare Variables
Declare @int int
Declare @Rows int
Declare	@SQL nvarchar(2500)
Declare	@SQL0 nvarchar(2500)
Declare	@SQL1 nvarchar(2500)
Declare	@SQL2 nvarchar(2500)
Declare	@SQL3 nvarchar(2500)
Declare	@SQL4 nvarchar(2500)
Declare	@SQL5 nvarchar(2500)
Declare	@SQL6 nvarchar(2500)
Declare	@Location nvarchar(2500)
Declare	@File nvarchar(2500)
Declare @DisaggregationFlag bit = 1
Declare	@DimensionId int
Declare	@TransformID int = (Select TransformID From Prog Where ProgID = @ProgId)
Declare	@CanLocProfileID int = (Select ProfileID From ProfileResource AS PR Join [Resource] AS R on PR.ResourceID = R.ResourceID Where R.ResourceTable = 'Transform' And R.ResourceKey = @TransformID and R.ResourceTypeID = 118) --118 = CanLoc Profile Type
Declare	@CanAccProfileID int = (Select ProfileID From ProfileResource AS PR Join [Resource] AS R on PR.ResourceID = R.ResourceID Where R.ResourceTable = 'Transform' And R.ResourceKey = @TransformID and R.ResourceTypeID = 119) --119 = CanAcc Profile Type
Declare	@RowId int = 1
Declare	@NumberRows int
Declare	@PrevCumulativeNumRisks int
Declare	@CumulativeNumRisks int
Declare @ProfileElementFormat nvarchar(255)
Declare @AccountFieldID int = 6
Declare @AccountFieldName nvarchar(255)
Declare @LayerFieldID int = 21
Declare @LayerFieldName nvarchar(255)
--update prog status
update prog set [status] = 'Loading Canonical Model' where ProgID = @ProgId
--Clear out canonical model for prog 
Select	S.ScheduleID,
		IG.InterestGroupId,
		IR.InterestRiskID,
		ISR.InterestSubRiskID,
		IE.InterestExposureID
Into	#ProgCan
From	Prog as P
Left Join	
		InterestGroup AS IG on IG.ProgID = P.ProgId
Left Join	
		Schedule AS S on IG.ScheduleID = S.ScheduleId
Left Join	
		InterestRisk AS IR on IG.InterestGroupID = IR.InterestGroupID
Left Join	
		InterestSubRisk AS ISR on IR.InterestRiskID = ISR.InterestRiskID
Left Join	
		InterestExposure AS IE on ISR.InterestSubRiskID = IE.InterestSubRiskID
Where	P.ProgID = @ProgId
Select	PO.PolicyID,
		PP.PolicyPerilID,
		PC.PolicyCoverageID,
		PL.PolicyLayerID
Into	#ProgAcc
From	Prog as P
Join	Policy AS PO on PO.ProgID = P.ProgId
Left Join	
		PolicyPeril AS PP on PO.PolicyID = PP.PolicyId
Left Join	
		PolicyLayer AS PL on PO.PolicyID = PL.PolicyID
Left Join	
		PolicyCoverage AS PC on PP.PolicyPerilID = PC.PolicyPerilID
Where	P.ProgID = @ProgId
if (select count(*) from #ProgAcc) > 0
begin
	Delete From	CoverageItem Where PolicyCoverageID IN (Select PolicyCoverageID From #ProgAcc)
end
if (select count(*) from #ProgCan) > 0
begin
	Delete From	CoverageItem Where InterestRiskID IN (Select InterestRiskID From #ProgCan)
	Delete From	CoverageItem Where InterestSubRiskID IN (Select InterestSubRiskID From #ProgCan)
	Delete From	CoverageItem Where InterestExposureID IN (Select InterestExposureID From #ProgCan)
	Delete From	InterestExposureValues Where InterestExposureID IN (Select InterestExposureID From #ProgCan)
	Delete From	InterestExposure Where InterestExposureID IN (Select InterestExposureID From #ProgCan)
	Delete From	InterestSubRiskValues Where InterestSubRiskID IN (Select InterestSubRiskID From #ProgCan)
	Delete From	InterestSubRisk Where InterestSubRiskID IN (Select InterestSubRiskID From #ProgCan)
	Delete From	InterestRiskValues Where InterestRiskID IN (Select InterestRiskID From #ProgCan)
	Delete From	InterestRisk Where InterestRiskID IN (Select InterestRiskID From #ProgCan)
	Delete From	InterestGroupValues Where InterestGroupID IN (Select InterestGroupID From #ProgCan)
	Delete From	InterestGroup Where InterestGroupID IN (Select InterestGroupID From #ProgCan)
	Delete From	ScheduleValues Where ScheduleID IN (Select Distinct ScheduleID From #ProgCan)
	Delete From	Schedule Where ScheduleID IN (Select Distinct ScheduleID From #ProgCan)
end
if (select count(*) from #ProgAcc) > 0
begin
	Delete From PolicyCoverageValues Where PolicyCoverageID IN (Select PolicyCoverageID From #ProgAcc)
	Delete From PolicyCoverage Where PolicyCoverageID IN (Select PolicyCoverageID From #ProgAcc)
	Delete From PolicyLayerValues Where PolicyLayerID IN (Select PolicyLayerID From #ProgAcc)
	Delete From PolicyLayer Where PolicyLayerID IN (Select PolicyLayerID From #ProgAcc)
	Delete From PolicyPeril Where PolicyPerilID IN (Select PolicyPerilID From #ProgAcc)
	Delete From PolicyValues Where PolicyID IN (Select PolicyID From #ProgAcc)
	Delete From Policy Where PolicyID IN (Select PolicyID From #ProgAcc)
end
--if (select count(*) from Correlation Where ProgId = @ProgID) > 0
--begin
--	Delete From	CorrelationItem Where CorrelationID IN (Select CorrelationID From Correlation Where ProgId = @ProgID)
--	Delete From	Correlation Where ProgId = @ProgID
--end
--GET NEXT IDS IN CANONICAL MODEL TABLES	
Declare	@InterestGroupId int	= (Select ISNULL(Max(InterestGroupID),0) From InterestGroup)
Declare	@InterestRiskId int		= (Select ISNULL(Max(InterestRiskID),0) From InterestRisk)
Declare	@InterestSubRiskId int	= (Select ISNULL(Max(InterestSubRiskID),0) From InterestSubRisk)
Declare	@InterestExposureId int = (Select ISNULL(Max(InterestExposureID),0) From InterestExposure)
Declare	@PolicyId int			= (Select ISNULL(Max(PolicyId),0) From Policy)
Declare	@PolicyPerilId int		= (Select ISNULL(Max(PolicyPerilId),0) From PolicyPeril)
Declare	@PolicyCoverageId int	= (Select ISNULL(Max(PolicyCoverageId),0) From PolicyCoverage)
Declare	@PolicyLayerId int		= (Select ISNULL(Max(PolicyLayerId),0) From PolicyLayer)
Declare	@CoverageItemId int		= (Select ISNULL(Max(CoverageItemId),0) From CoverageItem)
Declare	@ScheduleId int			= (Select ISNULL(Max(ScheduleId),0) + 1 From Schedule)
--Location Profile Table
CREATE TABLE #LocProfile (RowNumber int identity(1,1), ProfileElementName nvarchar(255), FormatName nvarchar(255), TableName nvarchar(255), FieldID int, DimensionID int, ProfileElementID int)
INSERT INTO #LocProfile
Select	PE.ProfileElementName,
		FO.FormatName,
		T.TableName,
		F.FieldID,
		DENSE_RANK() OVER (PARTITION BY T.TableName ORDER BY PE.ProfileElementID) AS DimensionID,
		PE.ProfileElementID
From	ProfileElement AS PE
Join	[Table] AS T on PE.TableID = T.TableID
Join	[Field] AS F on PE.FieldID = F.FieldID
Join	[Format] AS FO on F.FormatID = FO.FormatID
Where	ProfileID = @CanLocProfileID
order by pe.ProfileElementID 
--remove temp tables if exist
if exists (select [name] from sys.tables where name = 'TempCanLoc') begin drop table TempCanLoc end
if exists (select [name] from sys.tables where name = 'TempCanAcc') begin drop table TempCanAcc end
--Create Script for CanLoc Table
Set @int = 1
Set @Rows = (select count(*) from #LocProfile)
Set @SQL = 'CREATE TABLE TempCanLoc ('
Set @SQL0 = ''
Set @SQL1 = ''
Set @SQL2 = ''
Set @SQL3 = ''
Set @SQL4 = ''
Set @SQL5 = ''
Set @SQL6 = ''
while @int <= @Rows
begin
	Set @SQL = @SQL + (Select ProfileElementName + ' ' + FormatName + ' NULL' From #LocProfile Where RowNumber = @int)
	Set @SQL = @SQL + (Select Case When @int < @Rows Then ', ' Else ', InterestGroupID int NULL, InterestRiskID int NULL, InterestSubRiskID int NULL, InterestExposureID int NULL, PolicyID int NULL, PolicyCoverageID int NULL)' End)
	Set @SQL0 = @SQL0 + (Select ProfileElementName From #LocProfile Where RowNumber = @int)
	Set @SQL0 = @SQL0 + (Select Case When @int < @Rows Then ', ' Else '' End From #LocProfile Where RowNumber = @int)
	Set @SQL1 = @SQL1 + (Select Case When TableName = 'InterestGroupValues' Then ProfileElementName Else '' End From #LocProfile Where RowNumber = @int)
	Set @SQL1 = @SQL1 + (Select Case When @int < @Rows And TableName = 'InterestGroupValues' Then ', ' Else '' End From #LocProfile Where RowNumber = @int)
	Set @SQL2 = @SQL2 + (Select Case When TableName = 'InterestRiskValues' Then ProfileElementName Else '' End From #LocProfile Where RowNumber = @int)
	Set @SQL2 = @SQL2 + (Select Case When @int < @Rows And TableName = 'InterestRiskValues' Then ', ' Else '' End From #LocProfile Where RowNumber = @int)
	Set @SQL3 = @SQL3 + (Select Case When TableName = 'InterestSubRiskValues' Then ProfileElementName Else '' End From #LocProfile Where RowNumber = @int)
	Set @SQL3 = @SQL3 + (Select Case When @int < @Rows And TableName = 'InterestSubRiskValues' Then ', ' Else '' End From #LocProfile Where RowNumber = @int)
	Set @SQL4 = @SQL4 + (Select Case When TableName = 'InterestExposureValues' Then ProfileElementName Else '' End From #LocProfile Where RowNumber = @int)
	Set @SQL4 = @SQL4 + (Select Case When @int < @Rows And TableName = 'InterestExposureValues' Then ', ' Else '' End From #LocProfile Where RowNumber = @int)
	Set @SQL5 = @SQL5 + (Select Case When TableName = 'PolicyValues' Then ProfileElementName Else '' End From #LocProfile Where RowNumber = @int)
	Set @SQL5 = @SQL5 + (Select Case When @int < @Rows And TableName = 'PolicyValues' Then ', ' Else '' End From #LocProfile Where RowNumber = @int)
	Set @SQL6 = @SQL6 + (Select Case When TableName = 'PolicyCoverageValues' Then ProfileElementName Else '' End From #LocProfile Where RowNumber = @int)
	Set @SQL6 = @SQL6 + (Select Case When @int < @Rows And TableName = 'PolicyCoverageValues' Then ', ' Else '' End From #LocProfile Where RowNumber = @int)
	Set @int = @int+1
end
--remove trailing commas
if right(@SQL1,2) = ',' begin set @SQL1 = left(@SQL1,LEN(@SQL1)-1) end
if right(@SQL2,2) = ',' begin set @SQL2 = left(@SQL2,LEN(@SQL2)-1) end
if right(@SQL3,2) = ',' begin set @SQL3 = left(@SQL3,LEN(@SQL3)-1) end
if right(@SQL4,2) = ',' begin set @SQL4 = left(@SQL4,LEN(@SQL4)-1) end
if right(@SQL5,2) = ',' begin set @SQL5 = left(@SQL5,LEN(@SQL5)-1) end
if right(@SQL6,2) = ',' begin set @SQL6 = left(@SQL6,LEN(@SQL6)-1) end
--run SQL to create tempcanloc table
exec sp_executeSQL @SQL
--get Canonical Location File Details
Select	@File = F.[FileName],
		@Location = L.LocationName
From	[File] AS F
Join	Location AS L on F.LocationId = L.LocationId
Join	[FileResource] AS FR on F.FileId = FR.FileId
Join	[Resource] AS R on FR.ResourceId = R.ResourceId
Join	Prog AS P
			on	R.ResourceTable = 'Prog'
			and R.ResourceKey = P.ProgId
Where	P.ProgId = @ProgId
And		R.ResourceTypeId = 103 --Canonical Loc File
--get Canonical File Data into TempCanLoc
Set		@SQL = 'Select	' + @SQL0 + ',
		' + CASE WHEN @SQL1							= '' THEN '1' ELSE 'DENSE_RANK() OVER (ORDER BY ' + @SQL1 + ')' END + ' AS InterestGroupID,
		' + CASE WHEN @SQL1 + @SQL2					= '' THEN '1' ELSE 'DENSE_RANK() OVER (ORDER BY ' + @SQL1 + ',' + @SQL2 + ')' END + ' AS InterestRiskID,
		' + CASE WHEN @SQL1 + @SQL2 + @SQL3			= '' THEN '1' ELSE 'DENSE_RANK() OVER (ORDER BY ' + @SQL1 + ',' + @SQL2 + ',' + @SQL3 + ')' END + ' AS InterestSubRiskID,
		' + CASE WHEN @SQL1 + @SQL2 + @SQL3 + @SQL4 = '' THEN '1' ELSE 'DENSE_RANK() OVER (ORDER BY ' + @SQL1 + ',' + @SQL2 + ',' + @SQL3 + ',' + @SQL4 + ')' END + ' AS InterestExposureID,
		' + CASE WHEN @SQL5							= '' THEN '1' ELSE 'DENSE_RANK() OVER (ORDER BY ' + @SQL5 + ')' END + '  AS PolicyID,
		' + CASE WHEN @SQL5	+ @SQL6					= '' THEN '1' ELSE 'DENSE_RANK() OVER (ORDER BY ' + @SQL5 + ',' + @SQL6 + ')' END + '  AS PolicyCoverageID
From	OpenRowset(''MSDASQL'',''Driver={Microsoft Access Text Driver (*.txt, *.csv)};
          DefaultDir='+@Location+';Extended Properties="text;HDR=YES;FMT=Delimited'',
          ''select * from '+@File+''')'
--remove trailing commas from @SQL
Set		@int = 1
while	@int <= 6
begin
	Set		@SQL = replace(@SQL,',,',',')
	Set		@int = @int+1
end
Set		@SQL = replace(@SQL,'ORDER BY ,','ORDER BY ')
Set		@SQL = replace(@SQL,',)',')')
--populate TempCanLoc
Insert Into TempCanLoc
exec sp_executesql @SQL
Update TempCanLoc Set InterestGroupID = InterestGroupID + @InterestGroupId
Update TempCanLoc Set InterestRiskID = InterestRiskID + @InterestRiskID
Update TempCanLoc Set InterestSubRiskID = InterestSubRiskID + @InterestSubRiskID
Update TempCanLoc Set InterestExposureID = InterestExposureID + @InterestExposureID
Update TempCanLoc Set PolicyID = PolicyID + @PolicyID
Update TempCanLoc Set PolicyCoverageID = PolicyCoverageID + @PolicyCoverageID
--select * from TempCanLoc
--populate canonical tables
 --schedule
insert into schedule (scheduleid, ScheduleName, scheduledesc, profileid)
	select	@ScheduleId, null, null, null
 --InterestGroup
insert into InterestGroup (InterestGroupId, InterestGroupName, InterestGroupDesc, ScheduleID, ProgID, ProfileID)
	select	Distinct InterestGroupId, null, null, @ScheduleId, @ProgId, @CanLocProfileID From TempCanLoc
 --InterestRisk
insert into InterestRisk (InterestRiskId, InterestName, InterestSeqNo, InterestGroupId, ProfileID)
	select	Distinct InterestRiskId, null, 1, InterestGroupId, @CanLocProfileID From TempCanLoc
 --InterestSubRisk
insert into InterestSubRisk (InterestSubRiskId, InterestSubRiskName, InterestSubRiskSeqNo, InterestRiskId, ProfileID)
	select	Distinct InterestSubRiskId, null, 1, InterestRiskId, @CanLocProfileID From TempCanLoc
 --InterestExposure
insert into InterestExposure (InterestExposureId, InterestSubRiskId, TIVCcy, TIV, CoverageTypeID, ProfileID)
	select	Distinct InterestExposureId, InterestSubRiskId, 1, NULL, NULL, @CanLocProfileID From TempCanLoc
 --policy
insert into policy (policyid, PolicyTypeID, profileid, progid)
	select	distinct PolicyId, 0, @CanLocProfileID, @progid From TempCanLoc
 --policyperil
insert into PolicyPeril (PolicyPerilId, PolicyID, PerilID)
	select	Distinct PolicyId, PolicyId, 0 From TempCanLoc --note: works only for single peril policies at the moment
 --policycoverage
insert into policycoverage (PolicyCoverageId, PolicyPerilId, ProfileID, CoverageTypeID)
	select	Distinct PolicyCoverageId, PolicyId, @CanLocProfileID, 0 From TempCanLoc
--Value Tables
Set @int = 1
while @int <= @Rows
begin
	Select	@ProfileElementFormat = FO.FormatName
	From	[Format] AS FO
	Join	[Field] AS FI on FO.FormatID = FI.FormatID
	Join	ProfileElement AS PE on FI.FieldID = PE.FieldID
	Join	#LocProfile AS LP on PE.ProfileElementID = LP.ProfileElementID
	Where	LP.RowNumber = @int
	
	Select	@SQL =	'Insert Into ' 
					+ TableName 
					+ ' Select Distinct ' 
					+ Replace(TableName,'Values','ID') 
					+ ', ' 
					+ convert(nvarchar,DimensionID) 
					+ ', ' 
					+ convert(nvarchar,ProfileElementID) 
					+ ', ' 
					+ ProfileElementName 
					+ ', NULL From TempCanLoc'
	From	#LocProfile 
	Where	RowNumber = @int
	-- print @sql
	exec sp_executeSQL @SQL
	set @int = @int+1
end
--Account Profile Table
CREATE TABLE #AccProfile (RowNumber int identity(1,1), ProfileElementName nvarchar(255), FormatName nvarchar(255), TableName nvarchar(255), FieldID int, DimensionID int, ProfileElementID int)
INSERT INTO #AccProfile
Select	PE.ProfileElementName,
		FO.FormatName,
		T.TableName,
		F.FieldID,
		DENSE_RANK() OVER (PARTITION BY T.TableName ORDER BY PE.ProfileElementID) AS DimensionID,
		PE.ProfileElementID
From	ProfileElement AS PE
Join	[Table] AS T on PE.TableID = T.TableID
Join	[Field] AS F on PE.FieldID = F.FieldID
Join	[Format] AS FO on F.FormatID = FO.FormatID
Where	ProfileID = @CanAccProfileID
order by pe.ProfileElementID 
--Create Script for CanAcc Table
Set @int = 1
Set @Rows = (select count(*) from #AccProfile)
Set @SQL = 'CREATE TABLE TempCanAcc ('
Set @SQL0 = ''
Set @SQL1 = ''
Set @SQL2 = ''
while @int <= @Rows
begin
	Set @SQL = @SQL + (Select ProfileElementName + ' ' + FormatName + ' NULL' From #AccProfile Where RowNumber = @int)
	Set @SQL = @SQL + (Select Case When @int < @Rows Then ', ' Else ', PolicyID int NULL, PolicyLayerID int NULL, PolicyCoverageID int NULL)' End)
	Set @SQL0 = @SQL0 + (Select ProfileElementName From #AccProfile Where RowNumber = @int)
	Set @SQL0 = @SQL0 + (Select Case When @int < @Rows Then ', ' Else '' End From #AccProfile Where RowNumber = @int)
	Set @SQL1 = @SQL1 + (Select Case When TableName = 'PolicyValues' Then ProfileElementName Else '' End From #AccProfile Where RowNumber = @int)
	Set @SQL1 = @SQL1 + (Select Case When @int < @Rows And TableName = 'PolicyValues' Then ', ' Else '' End From #AccProfile Where RowNumber = @int)
	Set @SQL2 = @SQL2 + (Select Case When TableName = 'PolicyCoverageValues' Then ProfileElementName Else '' End From #AccProfile Where RowNumber = @int)
	Set @SQL2 = @SQL2 + (Select Case When @int < @Rows And TableName = 'PolicyCoverageValues' Then ', ' Else '' End From #AccProfile Where RowNumber = @int)
	Set @SQL3 = @SQL3 + (Select Case When TableName = 'PolicyLayerValues' Then ProfileElementName Else '' End From #AccProfile Where RowNumber = @int)
	Set @SQL3 = @SQL3 + (Select Case When @int < @Rows And TableName = 'PolicyLayerValues' Then ', ' Else '' End From #AccProfile Where RowNumber = @int)
	Set @int = @int+1
end
--remove trailing commas
if right(@SQL1,2) = ',' begin set @SQL1 = left(@SQL1,LEN(@SQL1)-1) end
if right(@SQL2,2) = ',' begin set @SQL2 = left(@SQL2,LEN(@SQL2)-1) end
if right(@SQL3,2) = ',' begin set @SQL3 = left(@SQL3,LEN(@SQL3)-1) end
--run SQL to create tempcanloc table
exec sp_executeSQL @SQL
--get Canonical Location File Details
Select	@File = F.[FileName],
		@Location = L.LocationName
From	[File] AS F
Join	Location AS L on F.LocationId = L.LocationId
Join	[FileResource] AS FR on F.FileId = FR.FileId
Join	[Resource] AS R on FR.ResourceId = R.ResourceId
Join	Prog AS P
			on	R.ResourceTable = 'Prog'
			and R.ResourceKey = P.ProgId
Where	P.ProgId = @ProgId
And		R.ResourceTypeId = 104 --Canonical Acc File
--get Canonical File Data into TempCanLoc
Set		@SQL = 'Select ' + @SQL0 + ', 
		NULL AS PolicyID,
		' + CASE WHEN @SQL1 + @SQL3	= '' THEN '1' ELSE 'DENSE_RANK() OVER (ORDER BY ' + @SQL1 + ',' + @SQL3 + ')' END + ' AS PolicyLayerID,
		' + CASE WHEN @SQL1 + @SQL2	= '' THEN '1' ELSE 'DENSE_RANK() OVER (ORDER BY ' + @SQL1 + ',' + @SQL2 + ')' END + ' AS PolicyCoverageID
From	OpenRowset(''MSDASQL'',''Driver={Microsoft Access Text Driver (*.txt, *.csv)};
          DefaultDir='+@Location+';Extended Properties="text;HDR=YES;FMT=Delimited'',
          ''select * from '+@File+''')'
--remove trailing commas from @SQL
Set		@int = 1
while	@int <= 2
begin
	Set		@SQL = replace(@SQL,',,',',')
	Set		@int = @int+1
end
Set		@SQL = replace(@SQL,'ORDER BY ,','ORDER BY ')
Set		@SQL = replace(@SQL,',)',')')
Insert Into TempCanAcc
exec sp_executesql @SQL
--set policy id
Set @SQL = 'Update	TempCanAcc 
Set		PolicyID = PV.PolicyID 
From	PolicyValues AS PV 
Join	Policy AS P on P.PolicyID = PV.PolicyID 
Join	ProfileElement AS PE on PV.ProfileElementID = PE.ProfileElementID
Where	P.ProgID = ' + convert(nvarchar,@ProgID) + '
and		PE.FieldID = 6
and		PE.ProfileID = ' + convert(nvarchar,@CanLocProfileID) + '
and		PV.FieldValue = TempCanAcc.' + (Select ProfileElementName From ProfileElement Where ProfileID = @CanLocProfileID And FieldID = 6) --ACCNTNUM 
exec sp_executesql @SQL
--set policy coverage id
Set		@PolicyCoverageId = (Select ISNULL(Max(PolicyCoverageId),0) From PolicyCoverage)
Update	TempCanAcc 
Set		PolicyCoverageID = PolicyCoverageID + @PolicyCoverageID
--set policy layer id
Set		@PolicyLayerId = (Select ISNULL(Max(PolicyLayerId),0) From PolicyLayer)
Update	TempCanAcc 
Set		PolicyLayerID = PolicyLayerID + @PolicyLayerID
--populate canonical tables for policy terms
----policycoverage
--insert into policycoverage (PolicyCoverageId, PolicyPerilId, ProfileID, CoverageTypeID)
--	select	Distinct PolicyCoverageId, PolicyId, @CanAccProfileID, 0 From TempCanAcc where PolicyId is not null
--policylayer
Select @LayerFieldName = ProfileElementName From ProfileElement Where ProfileID = @CanAccProfileID And FieldID = @LayerFieldID

if @LayerFieldName is not null
begin
	Set @SQL = '
	insert into policylayer (PolicyLayerId, PolicyLayerName, PolicyID, LayerNumber)
		select	Distinct PolicyLayerId, '+@LayerFieldName+', PolicyID, DENSE_RANK() OVER (PARTITION BY PolicyId ORDER BY PolicyLayerId) as LayerNumber From TempCanAcc where PolicyId is not null'

	exec sp_executesql @SQL
end

--Value Tables
Set @int = 1
while @int <= @Rows
begin
	Select @SQL = 'Insert Into ' + TableName + ' Select Distinct ' + Replace(TableName,'Values','ID') + ', ' + convert(nvarchar,DimensionID) + ', ' + convert(nvarchar,ProfileElementID) + ', ' + ProfileElementName + ', NULL From TempCanAcc where PolicyId is not null'
	From #AccProfile Where RowNumber = @int
	If (Select TableName From #AccProfile Where RowNumber = @int) in ('PolicyLayerValues', 'PolicyCoverageValues')
	begin
		exec sp_executeSQL @SQL
	end
	set @int = @int+1
end
--policy number
Select @AccountFieldName = ProfileElementName From ProfileElement Where ProfileID = @CanAccProfileID And FieldID = @AccountFieldID

Set @SQL = '
update	policy
set		policyname = a.policyname
from	(
		select	distinct policyid,
				'+@AccountFieldName+' as policyname
		from	tempcanacc
		) as a
where	policy.policyid = a.policyid'

exec sp_executesql @SQL
--Link Interests to Coverages
-- 1 -- coverage level
Set		@CoverageItemId = (Select ISNULL(Max(CoverageItemId),0) From CoverageItem)
Insert  Into CoverageItem (CoverageItemID, PolicyCoverageId, InterestExposureID)
Select	Distinct @CoverageItemId + ROW_NUMBER() OVER (ORDER BY InterestExposureId) AS CoverageItemID,
		PolicyCoverageId,
		InterestExposureId
From	TempCanLoc


Drop Table #ProgCan
Drop Table #ProgAcc
Drop Table TempCanLoc
Drop Table TempCanAcc
Drop Table #LocProfile
Drop Table #AccProfile
--update prog status
if @@error = 0
	begin
		update prog set [status] = 'Loaded' where ProgID = @ProgId
	end
else
	begin
		update prog set [status] = 'Failed' where ProgID = @ProgId
	end
Select 'Done' AS ReturnMessage
GO

/****** Object:  StoredProcedure [dbo].[generateLocationRecord]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE Procedure [dbo].[generateLocationRecord] (@LocationPathUnix nvarchar(255), @SubDirectory nvarchar(255), @LocationDesc nvarchar(255))
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = '@LocationPathUnix = ' + @LocationPathUnix + ', @SubDirectory = ' + @SubDirectory + ', @LocationDesc = ' + @LocationDesc
declare	@LogTimestamp datetime = getdate()
exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------
Declare @ParentLocationID int = (Select locationid From Location where LocationPathUnix = @LocationPathUnix)
Declare @ParentLocationName nvarchar(255) = (Select LocationName From Location where LocationID = @ParentLocationID)
Declare @NewLocationID int 
IF EXISTS (SELECT 1 FROM Location where LocationPathUnix = (@LocationPathUnix + '/' + @SubDirectory) )
begin
	set @NewLocationID = (SELECT LocationID FROM Location where LocationPathUnix = @LocationPathUnix + '/' + @SubDirectory)
end
else
begin
	set @NewLocationID = (Select isnull(max(locationid),0)+1 From Location)
	Insert Into Location (LocationID,LocationName,LocationDesc,LocationPathUnix)
	Select	@NewLocationID AS LocationID,
			@ParentLocationName + '\' + @SubDirectory AS LocationName,
			@LocationDesc AS LocationDesc,
			@LocationPathUnix + '/' + @SubDirectory AS LocationPathUnix
end
--Return
Select @NewLocationID AS LocationID
GO


/****** Object:  StoredProcedure [dbo].[generateOasisFiles2]    Script Date: 2/24/2017 4:46:48 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE procedure [dbo].[generateOasisFiles2] @ProgOasisId int
AS
SET ANSI_NULLS ON;
SET ANSI_WARNINGS ON;
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = '@ProgOasisId = ' + convert(nvarchar,@ProgOasisId)
declare	@LogTimestamp datetime = getdate()
exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

--declare @ProgOasisId int = 5
--Declare Variables
Declare @ProgId int = (Select ProgID From ProgOasis Where ProgOasisId = @ProgOasisID)
Declare @ModelId int = (Select ModelID From ProgOasis Where ProgOasisId = @ProgOasisID)
Declare @TransformID int = (Select TransformID From Prog Where ProgID = @ProgId)
Declare @CanLocProfileID int = (Select ProfileID From ProfileResource AS PR Join [Resource] AS R on PR.ResourceID = R.ResourceID Where R.ResourceTable = 'Transform' And ResourceKey = @TransformID And ResourceTypeID = 118)
Declare @CanAccProfileID int = (Select ProfileID From ProfileResource AS PR Join [Resource] AS R on PR.ResourceID = R.ResourceID Where R.ResourceTable = 'Transform' And ResourceKey = @TransformID And ResourceTypeID = 119)
Declare @TIVFieldID int = 2
Declare @LocFieldID int = 3
Declare @StateCodeID int = 29
Declare @CountyCode int = 30
Declare @PostCode int = 7
Declare @SourceLocNumber int = 10
Declare @OccScheme int = 13
Declare @OccCode int = 14
Declare @LineOfBusinessId  int = 61
Declare @AccountNumber int = 6
Declare @SubLimitRef int = 54
Declare @ModelGroupField int = 300
Declare @SQL nvarchar(2500)
Declare @ProfileElementID int
Declare @Level int
Declare @MaxLevel int

insert into LogDatabaseUsage values ('generateOasisFiles2','started',getdate(),null)

--update status
update progoasis set [status] = 'Generating Oasis Files' where progoasisid = @ProgOasisId
--Item Staging File
Create Table #Item (ItemId int identity(1,1) primary key, InterestGroupId int, InterestRiskID int, InterestSubRiskID int, InterestExposureID int,
					GroupId int NULL, LocId int NULL, PerilId nvarchar(255) NULL, CoverageTypeId nvarchar(255) NULL, TIV float NULL, 
					AreaPerilId int NULL, VulnerabilityId int NULL, PolicyID int NULL, AltItemID int NULL, CoverageId int NULL, StateCode nvarchar(255) NULL,
					CountyCode nvarchar(255) NULL, PostCode nvarchar(255) NULL, SourceLocNumber nvarchar(255) NULL, OccScheme nvarchar(255) NULL, 
					OccCode nvarchar(255) NULL, LineOfBusiness nvarchar(255) NULL, AccountNumber nvarchar(255) NULL, SubLimitRef nvarchar(255) NULL, ElementDimensionID int NULL, ProgID int NULL,
					IsValid bit null)
CREATE INDEX Idx_Item_InterestRiskID	 ON #Item (InterestRiskID)
CREATE INDEX Idx_Item_InterestSubRiskID  ON #Item (InterestSubRiskID)
CREATE INDEX Idx_Item_InterestExposureID ON #Item (InterestExposureID)
Insert Into	#Item (InterestGroupId, InterestRiskID, InterestSubRiskID, InterestExposureID,GroupId, LocId, PerilId, CoverageTypeId, TIV, 
					AreaPerilId, VulnerabilityId, PolicyID, AltItemID, CoverageId, StateCode, CountyCode, PostCode, SourceLocNumber, 
					OccScheme, OccCode, LineOfBusiness, AccountNumber,SubLimitRef,ElementDimensionID,ProgID,IsValid)
Select	IG.InterestGroupId,
		IR.InterestRiskID,
		ISR.InterestSubRiskID,
		IE.InterestExposureID,
		NULL AS GroupId,
		NULL AS LocId,
		PVD.PerilID,
		PVD.CoverageTypeId,
		IEV.FieldValue AS TIV,
		NULL AS AreaPerilId,
		NULL AS VulnerabilityId,
		NULL AS PolicyID,
		NULL AS AltItemID,
		NULL AS CoverageId,
		cast(NULL as nvarchar(255)) AS StateCode,
		cast(NULL as nvarchar(255)) AS CountyCode,
		cast(NULL as nvarchar(255)) AS PostCode,
		cast(NULL as nvarchar(255)) AS SourceLocNumber,
		cast(NULL as nvarchar(255)) AS OccScheme,
		cast(NULL as nvarchar(255)) AS OccCode,
		cast(NULL as nvarchar(255)) AS LineOfBusiness,
		cast(NULL as nvarchar(255)) AS AccountNumber,
		cast(NULL as nvarchar(255)) AS SubLimitRef,
		PVD.ElementDimensionID,
		@ProgId AS ProgID,
		1 AS IsValid
From	InterestGroup AS IG
Join	InterestRisk AS IR on IG.InterestGroupID = IR.InterestGroupID
Join	InterestSubRisk AS ISR on IR.InterestRiskID = ISR.InterestRiskID
Join	InterestExposure AS IE on ISR.InterestSubRiskID = IE.InterestSubRiskID
Join	InterestExposureValues AS IEV on IE.InterestExposureID = IEV.InterestExposureID
Join	ProfileElement AS PE on IEV.ProfileElementID = PE.ProfileElementID
Join	ProfileValueDetail AS PVD on PE.ProfileElementID = PVD.ProfileElementID
Where	IG.ProgID = @ProgId
And		PE.FieldID = 2 --TIV
And		PE.ProfileID = @CanLocProfileID

insert into LogDatabaseUsage values ('generateOasisFiles2','created #item',getdate(),null)

--LocId
Set @ProfileElementID = (Select ProfileElementID From ProfileElement Where ProfileID = @CanLocProfileID and FieldID = @LocFieldID)
Select	@SQL = 'Update #Item Set LocID = FieldValue From	' + TableName + ' Where ProfileElementId = ' + convert(nvarchar,@ProfileElementID) + ' And #Item.' + Replace(TableName,'Values','ID') + ' = ' + TableName + '.' + Replace(TableName,'Values','ID') 
From	ProfileElement AS PE
Join	[Table] AS T on PE.TableID = T.TableID
Where	ProfileID = @CanLocProfileID 
And		PE.FieldID = @LocFieldID
exec sp_ExecuteSQL @SQL

--StateCode
Set @ProfileElementID = (Select ProfileElementID From ProfileElement Where ProfileID = @CanLocProfileID and FieldID = @StateCodeID)
if @ProfileElementID is not null
	begin
		Select	@SQL = 'Update #Item Set StateCode = FieldValue From	' + TableName + ' Where ProfileElementId = ' + convert(nvarchar,@ProfileElementID) + ' And #Item.' + Replace(TableName,'Values','ID') + ' = ' + TableName + '.' + Replace(TableName,'Values','ID') 
		From	ProfileElement AS PE
		Join	[Table] AS T on PE.TableID = T.TableID
		Where	ProfileID = @CanLocProfileID 
		And		PE.FieldID = @LocFieldID
		exec sp_ExecuteSQL @SQL
	end
else
	begin
		Update #Item Set StateCode = 'XX'
	end

--CountyCode
Set @ProfileElementID = (Select ProfileElementID From ProfileElement Where ProfileID = @CanLocProfileID and FieldID = @CountyCode)
if @ProfileElementID is not null
	begin
		Select	@SQL = 'Update #Item Set CountyCode = FieldValue From	' + TableName + ' Where ProfileElementId = ' + convert(nvarchar,@ProfileElementID) + ' And #Item.' + Replace(TableName,'Values','ID') + ' = ' + TableName + '.' + Replace(TableName,'Values','ID') 
		From	ProfileElement AS PE
		Join	[Table] AS T on PE.TableID = T.TableID
		Where	ProfileID = @CanLocProfileID 
		And		PE.FieldID = @LocFieldID
		exec sp_ExecuteSQL @SQL
	end
else
	begin
		Update #Item Set CountyCode = 'XX'
	end

--PostCode
Set @ProfileElementID = (Select ProfileElementID From ProfileElement Where ProfileID = @CanLocProfileID and FieldID = @PostCode)
if @ProfileElementID is not null
	begin
		Select	@SQL = 'Update #Item Set PostCode = FieldValue From	' + TableName + ' Where ProfileElementId = ' + convert(nvarchar,@ProfileElementID) + ' And #Item.' + Replace(TableName,'Values','ID') + ' = ' + TableName + '.' + Replace(TableName,'Values','ID') 
		From	ProfileElement AS PE
		Join	[Table] AS T on PE.TableID = T.TableID
		Where	ProfileID = @CanLocProfileID 
		And		PE.FieldID = @LocFieldID
		exec sp_ExecuteSQL @SQL
	end
else
	begin
		Update #Item Set PostCode = 'XXXXX'
	end

--SourceLocNumber
Set @ProfileElementID = (Select ProfileElementID From ProfileElement Where ProfileID = @CanLocProfileID and FieldID = @SourceLocNumber)
if @ProfileElementID is not null
	begin
		Select	@SQL = 'Update #Item Set SourceLocNumber = FieldValue From	' + TableName + ' Where ProfileElementId = ' + convert(nvarchar,@ProfileElementID) + ' And #Item.' + Replace(TableName,'Values','ID') + ' = ' + TableName + '.' + Replace(TableName,'Values','ID') 
		From	ProfileElement AS PE
		Join	[Table] AS T on PE.TableID = T.TableID
		Where	ProfileID = @CanLocProfileID 
		And		PE.FieldID = @LocFieldID
		exec sp_ExecuteSQL @SQL
	end
else
	begin
		Update #Item Set SourceLocNumber = 'XXX'
	end

--OccScheme
Set @ProfileElementID = (Select ProfileElementID From ProfileElement Where ProfileID = @CanLocProfileID and FieldID = @OccScheme)
if @ProfileElementID is not null
	begin
		Select	@SQL = 'Update #Item Set OccScheme = FieldValue From	' + TableName + ' Where ProfileElementId = ' + convert(nvarchar,@ProfileElementID) + ' And #Item.' + Replace(TableName,'Values','ID') + ' = ' + TableName + '.' + Replace(TableName,'Values','ID') 
		From	ProfileElement AS PE
		Join	[Table] AS T on PE.TableID = T.TableID
		Where	ProfileID = @CanLocProfileID 
		And		PE.FieldID = @LocFieldID
		exec sp_ExecuteSQL @SQL
	end
else
	begin
		Update #Item Set OccScheme = 'XXX'
	end
		

--OccCode
Set @ProfileElementID = (Select ProfileElementID From ProfileElement Where ProfileID = @CanLocProfileID and FieldID = @OccCode)
if @ProfileElementID is not null
	begin
		Select	@SQL = 'Update #Item Set OccCode = FieldValue From	' + TableName + ' Where ProfileElementId = ' + convert(nvarchar,@ProfileElementID) + ' And #Item.' + Replace(TableName,'Values','ID') + ' = ' + TableName + '.' + Replace(TableName,'Values','ID') 
		From	ProfileElement AS PE
		Join	[Table] AS T on PE.TableID = T.TableID
		Where	ProfileID = @CanLocProfileID 
		And		PE.FieldID = @LocFieldID
		exec sp_ExecuteSQL @SQL
	end
else
	begin
		Update #Item Set OccCode = 'XXX'
	end

--LineOfBusiness
Set @ProfileElementID = (Select ProfileElementID From ProfileElement Where ProfileID = @CanLocProfileID and FieldID = @LineOfBusinessId)
if @ProfileElementID is not null
	begin
		Select	@SQL = 'Update #Item Set LineOfBusiness = FieldValue From	' + TableName + ' Where ProfileElementId = ' + convert(nvarchar,@ProfileElementID) + ' And #Item.' + Replace(TableName,'Values','ID') + ' = ' + TableName + '.' + Replace(TableName,'Values','ID') 
		From	ProfileElement AS PE
		Join	[Table] AS T on PE.TableID = T.TableID
		Where	ProfileID = @CanLocProfileID 
		And		PE.FieldID = @LocFieldID
		exec sp_ExecuteSQL @SQL
	end
else
	begin
		Update #Item Set LineOfBusiness = 'XXX'
	end

--Oasis Keys
Update	#Item
Set		AreaPerilId = POK.AreaPerilId,
		VulnerabilityId = POK.VulnerabilityId
From	ProgOasisKeys AS POK
Where	#Item.LocId = POK.LocId
And		#Item.CoverageTypeId = POK.CoverageId
And		#Item.PerilId = POK.PerilId
And		POK.ProgOasisID = @ProgOasisID


--GroupID
Select	@SQL = 'Update #Item Set GroupID = A.GroupID From (Select ItemId, DENSE_RANK() OVER (ORDER BY ' + ModelResourceValue + ') AS GroupID From #Item) AS A Where #Item.ItemId = A.ItemID'
From	ModelResource AS PE
Where	ModelID = @ModelId
And		ResourceTypeID = @ModelGroupField
exec sp_ExecuteSQL @SQL


insert into LogDatabaseUsage values ('generateOasisFiles2','updated #item',getdate(),null)

--update #item set AreaPerilId = 0 where AreaPerilId is null
--update #item set VulnerabilityId = 0 where VulnerabilityId is null


declare @modeltypeid int = (Select modeltypeid from model where modelid = @modelid)

if @modeltypeid = 1
	begin

		update	#item 
		set		IsValid = 0
		where	AreaPerilId is null
		or		VulnerabilityId is null
		or		TIV = 0

		delete from #item where IsValid = 0

		update	#item 
		set AltItemID =A.AltItemID
		From	(
				select	itemid,
						DENSE_RANK() OVER (ORDER BY areaperilid, vulnerabilityid, itemid) AS AltItemID
				from	#item
				) AS A
		where	#item.itemid = A.itemid
	end

if @modeltypeid = 2 
	begin
		update	#item
		set		AltItemID = A.AltItemID
		From	(
				select	itemid,
						DENSE_RANK() OVER (ORDER BY locid, perilid, CoverageTypeId) AS AltItemID
				from	#item
				) AS A
		where	#item.itemid = A.itemid

		--remove zero items from non-zero alt items
		delete from #item
		where	TIV = 0
		and		AltItemID in (select altitemid from #item where TIV > 0)

		--remove duplicate zero items from zero value alt items
		delete from #Item
		where	TIV = 0
		and		itemid in (select itemid from (select AltItemID, max(ItemId) as itemid from #item group by AltItemID having sum(TIV) = 0) as a)

		update	#item 
		set		IsValid = 0
		where	AreaPerilId is null
		or		VulnerabilityId is null

		update	#item 
		set		IsValid = 0
		from	#item as i
		join	(
				select	LocId,
						PerilId,
						Sum(TIV) AS TIV
				From	#item
				group by
						LocId,
						PerilId
				having	Sum(TIV) = 0
				) as n
					on	i.LocId = n.LocId
					and	i.PerilId = n.PerilId

		
		update #item set AreaPerilId = 0, VulnerabilityId = 0 where IsValid = 0

		delete from #item where locid in (select locid from #item group by LocId having sum(convert(int,isvalid)) = 0)

		--reindex
		update	#item
		set		AltItemID = A.AltItemID
		From	(
				select	itemid,
						DENSE_RANK() OVER (ORDER BY AltItemID) AS AltItemID
				from	#item
				) AS A
		where	#item.itemid = A.itemid
	
		update #item set AreaPerilId = 0 where AreaPerilId is null
		update #item set VulnerabilityId = 0 where VulnerabilityId is null
		
	end



insert into LogDatabaseUsage values ('generateOasisFiles2','deleted from #item',getdate(),null)

--coverage id
Update #Item Set CoverageId = A.CoverageId From (Select ItemID, DENSE_RANK() OVER (ORDER BY LocId,CoverageTypeId) AS CoverageId From #Item) AS A Where #Item.ItemID = A.ItemID


--PolicyID
Create Table #TempPolicy (PolicyId int null, InterestExposureID int null)

Insert Into #TempPolicy (PolicyId,InterestExposureID)
Select	Distinct P.PolicyID, CI.InterestExposureID
From	Policy AS P
Join	PolicyPeril as PP on P.PolicyID = PP.PolicyID
Join	PolicyCoverage AS PC on PP.PolicyPerilID = PC.PolicyPerilID
Join	CoverageItem AS CI on PC.PolicyCoverageID = CI.PolicyCoverageID
Join	#Item as I on CI.InterestExposureID = I.InterestExposureID
Where	P.ProgID = @ProgID

Update	#Item Set PolicyID = A.PolicyId 
From	#TempPolicy AS A
Where	#Item.InterestExposureID = A.InterestExposureID



insert into LogDatabaseUsage values ('generateOasisFiles2','temp policy',getdate(),null)


--AccountNumber
Set @ProfileElementID = (Select ProfileElementID From ProfileElement Where ProfileID = @CanLocProfileID and FieldID = @AccountNumber)
Select	@SQL = 'Update #Item Set AccountNumber = FieldValue From	' + TableName + ' Where ProfileElementId = ' + convert(nvarchar,@ProfileElementID) + ' And #Item.' + Replace(TableName,'Values','ID') + ' = ' + TableName + '.' + Replace(TableName,'Values','ID') 
From	ProfileElement AS PE
Join	[Table] AS T on PE.TableID = T.TableID
Where	ProfileID = @CanLocProfileID 
And		PE.FieldID = @AccountNumber
exec sp_ExecuteSQL @SQL


Update	#Item 
Set		SubLimitRef = PCV.FieldValue 
From	CoverageItem AS CI
Join	PolicyCoverage AS PC on CI.PolicyCoverageID = PC.PolicyCoverageID
Join	PolicyCoverageValues AS PCV on PC.PolicyCoverageID = PCV.PolicyCoverageID
Where	PCV.ProfileElementID = (Select ProfileElementID From ProfileElement Where ProfileID = @CanLocProfileID and FieldID = @SubLimitRef)
And		#Item.InterestExposureID = CI.InterestExposureID


Truncate Table OasisITEMS
Truncate Table OasisCOVERAGES
Truncate Table OasisITEMDICT
Truncate Table OasisFM_PROGRAMME
Truncate Table OasisFM_POLICYTC
Truncate Table OasisFM_PROFILE
Truncate Table OasisFM_XREF
Truncate Table OasisFMDICT

--OasisITEMS
Insert Into OasisITEMS
Select	ItemID,
		CoverageID,
		AreaPerilId,
		VulnerabilityId,
		GroupId
From	(
		Select	AltItemID AS ItemID,
				CoverageID,
				AreaPerilId,
				VulnerabilityId,
				Sum(TIV) AS TIV,
				GroupId
		From	#Item
		--Where	AreaPerilId <> 0
		Group By
				AltItemID,
				CoverageID,
				AreaPerilId,
				VulnerabilityId,
				GroupId
		) AS A

--OasisCOVERAGES
Insert Into  OasisCOVERAGES
Select	CoverageID,
		Max(TIV) AS TIV
From	(
		Select	AltItemID AS ItemID,
				CoverageID,
				AreaPerilId,
				VulnerabilityId,
				Sum(TIV) AS TIV,
				GroupId
		From	#Item
		--Where	AreaPerilId <> 0
		Group By
				AltItemID,
				CoverageID,
				AreaPerilId,
				VulnerabilityId,
				GroupId
		) AS A
Group By 
		CoverageId

--OasisITEMDICT
Insert Into OasisITEMDICT
Select	Distinct AltItemID AS item_id,
		CoverageID AS coverage_id,
		dense_rank() over (order by AccountNumber + '-' + SourceLocNumber) AS location_id,
		AccountNumber + '-' + SourceLocNumber AS location_desc,
		dense_rank() over (order by LineOfBusiness) AS lob_id,
		LineOfBusiness AS lob_desc,
		dense_rank() over (order by CountyCode) AS county_id,
		CountyCode as county_desc,
		dense_rank() over (order by StateCode) AS state_id,
		StateCode AS state_desc
From	#Item


insert into LogDatabaseUsage values ('generateOasisFiles2','done item tables',getdate(),null)



--FM Files Calc
Declare @InterestTable nvarchar(255)
Declare @ValueTable nvarchar(255)
Declare	@InterestFieldId int
Declare	@ValueFieldDedId int
Declare	@ValueFieldLimId int
Declare @LevelID int 

CREATE TABLE #FM (ITEM_ID int NULL, ALTITEM_ID int NULL, LEVEL_ID int NULL, LAYER_ID int NULL, AGG_ID int NULL, POLICYTC_ID int NULL, DEDUCTIBLE FLOAT NULL, 
					LIMIT FLOAT NULL, SHARE_PROP_OF_LIM FLOAT NULL, DEDUCTIBLE_TYPE NVARCHAR(2) NULL, CALCRULE_ID INT NULL, TIV FLOAT NULL)
--CREATE TABLE #STAGINGFM (INTEREST_ID INT NULL, LAYER_ID int NULL, DEDUCTIBLE FLOAT NULL, LIMIT FLOAT NULL, ElementDimensionID int NULL)
--CREATE TABLE #STAGINGFM2 (INTEREST_ID INT NULL, LAYER_ID int NULL, DEDUCTIBLE FLOAT NULL, LIMIT FLOAT NULL, LAYER_NAME NVARCHAR(255) NULL)

CREATE INDEX Idx_FM_ITEM_ID ON #FM (ITEM_ID)
CREATE INDEX Idx_FM_LEVEL_ID ON #FM (LEVEL_ID)
--CREATE INDEX Idx_STAGINGFM_InterestRiskID ON #STAGINGFM (Interest_ID)

--level 1, coverage level
insert into #FM (ITEM_ID,ALTITEM_ID,LEVEL_ID,LAYER_ID,AGG_ID,POLICYTC_ID,DEDUCTIBLE,LIMIT,SHARE_PROP_OF_LIM,DEDUCTIBLE_TYPE,CALCRULE_ID,TIV)
	select ItemId,AltItemID,1,1,DENSE_RANK() OVER (ORDER BY coverageid) AS AGG_ID,NULL,NULL,NULL,NULL,'B',NULL,TIV from #Item

--level 2, combined level
insert into #FM (ITEM_ID,ALTITEM_ID,LEVEL_ID,LAYER_ID,AGG_ID,POLICYTC_ID,DEDUCTIBLE,LIMIT,SHARE_PROP_OF_LIM,DEDUCTIBLE_TYPE,CALCRULE_ID,TIV)
	select ItemId,AltItemID,2,1,DENSE_RANK() OVER (ORDER BY case when coveragetypeid =4 then 2 else 1 end, locid) AS AGG_ID,NULL,NULL,NULL,NULL,'B',NULL,TIV from #Item
	
--level 3, location level
insert into #FM (ITEM_ID,ALTITEM_ID,LEVEL_ID,LAYER_ID,AGG_ID,POLICYTC_ID,DEDUCTIBLE,LIMIT,SHARE_PROP_OF_LIM,DEDUCTIBLE_TYPE,CALCRULE_ID,TIV)
	select ItemId,AltItemID,3,1,DENSE_RANK() OVER (ORDER BY locid) AS AGG_ID,NULL,NULL,NULL,NULL,'B',NULL,TIV from #Item
	
--level 4, sublimit level
insert into #FM (ITEM_ID,ALTITEM_ID,LEVEL_ID,LAYER_ID,AGG_ID,POLICYTC_ID,DEDUCTIBLE,LIMIT,SHARE_PROP_OF_LIM,DEDUCTIBLE_TYPE,CALCRULE_ID,TIV)
	select ItemId,AltItemID,4,1,DENSE_RANK() OVER (ORDER BY PolicyID,sublimitref) AS AGG_ID,NULL,NULL,NULL,NULL,'B',NULL,TIV from #Item
	
--level 5, policy level
insert into #FM (ITEM_ID,ALTITEM_ID,LEVEL_ID,LAYER_ID,AGG_ID,POLICYTC_ID,DEDUCTIBLE,LIMIT,SHARE_PROP_OF_LIM,DEDUCTIBLE_TYPE,CALCRULE_ID,TIV)
	select ItemId,AltItemID,5,1,DENSE_RANK() OVER (ORDER BY PolicyID) AS AGG_ID,NULL,NULL,NULL,NULL,'B',NULL,TIV from #Item
	
--level 6, layer level
insert into #FM (ITEM_ID,ALTITEM_ID,LEVEL_ID,LAYER_ID,AGG_ID,POLICYTC_ID,DEDUCTIBLE,LIMIT,SHARE_PROP_OF_LIM,DEDUCTIBLE_TYPE,CALCRULE_ID,TIV)
	select ItemId,AltItemID,6,PL.LayerNumber,DENSE_RANK() OVER (ORDER BY I.PolicyID) AS AGG_ID,NULL,NULL,NULL,NULL,'B',NULL,TIV from #Item as i
		join PolicyLayer AS PL ON I.PolicyID = PL.PolicyID 
	

insert into LogDatabaseUsage values ('generateOasisFiles2','created #FM',getdate(),null)

create table #exposures (InterestExposureID int null, CoverageItemID int null, ElementDimensionID int null,
					CoverageTypeID int null, PerilID int null)

insert into #exposures
select	ie.InterestExposureID,
		ci.CoverageItemID,
		pvd.ElementDimensionID,
		pvd.CoverageTypeID,
		pvd.PerilID
from	interestexposure as ie
join	interestexposurevalues as iev on ie.InterestExposureID = iev.InterestExposureID
join	ProfileElement as pe on iev.ProfileElementID = pe.ProfileElementID
join	ProfileValueDetail as pvd on pe.ProfileElementID = pvd.ProfileElementID
join	CoverageItem as ci on ie.InterestExposureID = ci.InterestExposureID
Join	#Item as I on CI.InterestExposureID = I.InterestExposureID
where	pe.FieldID = @TIVFieldID



create table #coverages (PolicyCoverageID int null, CoverageItemID int null, ElementDimensionID int null,
					CoverageTypeID int null, PerilID int null, FieldID int null, FieldValue float null)

insert into #coverages
select	distinct pc.PolicyCoverageID,
		ci.CoverageItemID,
		pvd.ElementDimensionID,
		pvd.CoverageTypeID,
		pvd.PerilID,
		pe.FieldID,
		case when pe.FieldID in (15,16,17,18,19,20,55,56) then pcv.FieldValue else cast(0 as float) end as FieldValue
from	PolicyCoverage as pc
join	PolicyCoverageValues as pcv on pc.PolicyCoverageID = pcv.PolicyCoverageID
join	ProfileElement as pe on pcv.ProfileElementID = pe.ProfileElementID
left join	
		ProfileValueDetail as pvd on pe.ProfileElementID = pvd.ProfileElementID
join	CoverageItem as ci on pc.PolicyCoverageID = ci.PolicyCoverageID
Join	#Item as I on CI.InterestExposureID = I.InterestExposureID
where	FieldID in (15, --coverage limit
					16, --coverage deductible
					17, --site limit
					18, --site deductible
					19, --combined limit
					20, --combined deductible
					--,23, --layer attachment point
					--24, --layer limit
					--27, --blanket deductible
					--28  --blanket limit
					55,
					56
					)



--level 1
Set @Level = 1
update #fm set LIMIT = FieldValue from (
				select	i.ItemId,
						c.FieldValue 
				from	#exposures as e 
				join	#coverages as c 
							on	e.CoverageItemID = c.CoverageItemID 
							and e.ElementDimensionID = c.ElementDimensionID
				join	#item as i
							on	i.InterestExposureID = e.InterestExposureID
							and	i.ElementDimensionID = e.ElementDimensionID
				where  c.FieldID = 15) As A
where	#fm.ITEM_ID = A.ItemId
and		#fm.LEVEL_ID = @Level



update #fm set DEDUCTIBLE = FieldValue from (
				select	i.ItemId,
						c.FieldValue 
				from	#exposures as e 
				join	#coverages as c 
							on	e.CoverageItemID = c.CoverageItemID 
							and e.ElementDimensionID = c.ElementDimensionID
				join	#item as i
							on	i.InterestExposureID = e.InterestExposureID
							and	i.ElementDimensionID = e.ElementDimensionID
				where  c.FieldID = 16) As A
where	#fm.ITEM_ID = A.ItemId
and		#fm.LEVEL_ID = @Level



--level 2
set @level = @level +1
update #fm set LIMIT = FieldValue from (
				select	i.ItemId,
						case when i.coveragetypeid = 4 then 0 else c.FieldValue end as FieldValue
				from	#exposures as e 
				join	#coverages as c 
							on	e.CoverageItemID = c.CoverageItemID 
				join	#item as i
							on	i.InterestExposureID = e.InterestExposureID
				where  c.FieldID = 19) As A
where	#fm.ITEM_ID = A.ItemId
and		#fm.LEVEL_ID = @level



update #fm set DEDUCTIBLE = FieldValue from (
				select	i.ItemId,
						case when i.coveragetypeid = 4 then 0 else c.FieldValue end as FieldValue
				from	#exposures as e 
				join	#coverages as c 
							on	e.CoverageItemID = c.CoverageItemID 
				join	#item as i
							on	i.InterestExposureID = e.InterestExposureID
				where  c.FieldID = 20) As A
where	#fm.ITEM_ID = A.ItemId
and		#fm.LEVEL_ID = @level



--level 3
set @level = @level +1
update #fm set LIMIT = FieldValue from (
				select	i.ItemId,
						FieldValue
				from	#exposures as e 
				join	#coverages as c 
							on	e.CoverageItemID = c.CoverageItemID 
				join	#item as i
							on	i.InterestExposureID = e.InterestExposureID
				where  c.FieldID = 17) As A
where	#fm.ITEM_ID = A.ItemId
and		#fm.LEVEL_ID = @level



update #fm set DEDUCTIBLE = FieldValue from (
				select	i.ItemId,
						FieldValue
				from	#exposures as e 
				join	#coverages as c 
							on	e.CoverageItemID = c.CoverageItemID 
				join	#item as i
							on	i.InterestExposureID = e.InterestExposureID
				where  c.FieldID = 18) As A
where	#fm.ITEM_ID = A.ItemId
and		#fm.LEVEL_ID = @level



--level 4
set @level = @level +1
update #fm set LIMIT = FieldValue from (
				select	i.ItemId,
						FieldValue
				from	#exposures as e 
				join	#coverages as c 
							on	e.CoverageItemID = c.CoverageItemID 
				join	#item as i
							on	i.InterestExposureID = e.InterestExposureID
				where  c.FieldID = 55) As A
where	#fm.ITEM_ID = A.ItemId
and		#fm.LEVEL_ID = @level



update #fm set DEDUCTIBLE = FieldValue from (
				select	i.ItemId,
						FieldValue
				from	#exposures as e 
				join	#coverages as c 
							on	e.CoverageItemID = c.CoverageItemID 
				join	#item as i
							on	i.InterestExposureID = e.InterestExposureID
				where  c.FieldID = 56) As A
where	#fm.ITEM_ID = A.ItemId
and		#fm.LEVEL_ID = @level



--level 5
set @level = @level +1
update #fm set LIMIT = FieldValue from (
				select	distinct i.ItemId,
						plv.FieldValue
				from	#item as i
				join	[Policy] as p on i.PolicyID = p.PolicyID
				join	PolicyLayer as pl on p.PolicyID = pl.PolicyID
				join	PolicyLayerValues as plv on pl.PolicyLayerID = plv.PolicyLayerID
				join	ProfileElement as pe on plv.ProfileElementID = pe.ProfileElementID
				where	pe.FieldID = 28
				) As A
where	#fm.ITEM_ID = A.ItemId
and		#fm.LEVEL_ID = @level


--MIN ded
update #fm set DEDUCTIBLE = FieldValue, DEDUCTIBLE_TYPE = 'MI' from (
				select	distinct i.ItemId,
						plv.FieldValue
				from	#item as i
				join	[Policy] as p on i.PolicyID = p.PolicyID
				join	PolicyLayer as pl on p.PolicyID = pl.PolicyID
				join	PolicyLayerValues as plv on pl.PolicyLayerID = plv.PolicyLayerID
				join	ProfileElement as pe on plv.ProfileElementID = pe.ProfileElementID
				where	pe.FieldID = 25
				and		plv.FieldValue > '0'
				) As A
where	#fm.ITEM_ID = A.ItemId
and		#fm.LEVEL_ID = @level

--MAX ded
update #fm set DEDUCTIBLE = FieldValue, DEDUCTIBLE_TYPE = 'MA' from (
				select	distinct i.ItemId,
						plv.FieldValue
				from	#item as i
				join	[Policy] as p on i.PolicyID = p.PolicyID
				join	PolicyLayer as pl on p.PolicyID = pl.PolicyID
				join	PolicyLayerValues as plv on pl.PolicyLayerID = plv.PolicyLayerID
				join	ProfileElement as pe on plv.ProfileElementID = pe.ProfileElementID
				where	pe.FieldID = 26
				and		plv.FieldValue > '0'
				) As A
where	#fm.ITEM_ID = A.ItemId
and		#fm.LEVEL_ID = @level

--BLANKET ded
update #fm set DEDUCTIBLE = FieldValue, DEDUCTIBLE_TYPE = 'B' from (
				select	distinct i.ItemId,
						plv.FieldValue
				from	#item as i
				join	[Policy] as p on i.PolicyID = p.PolicyID
				join	PolicyLayer as pl on p.PolicyID = pl.PolicyID
				join	PolicyLayerValues as plv on pl.PolicyLayerID = plv.PolicyLayerID
				join	ProfileElement as pe on plv.ProfileElementID = pe.ProfileElementID
				where	pe.FieldID = 27
				and		plv.FieldValue > '0'
				) As A
where	#fm.ITEM_ID = A.ItemId
and		#fm.LEVEL_ID = @level

insert into LogDatabaseUsage values ('generateOasisFiles2','done #FM level 5',getdate(),null)

--level 6
set @level = @level +1

select	i.ItemId,
		pl.LayerNumber,
		plv.FieldValue
into	tempA
from	#item as i
join	[Policy] as p on i.PolicyID = p.PolicyID
join	PolicyLayer as pl on p.PolicyID = pl.PolicyID
join	PolicyLayerValues as plv on pl.PolicyLayerID = plv.PolicyLayerID
join	ProfileElement as pe on plv.ProfileElementID = pe.ProfileElementID
where	pe.FieldID = 24

update	#fm 
set		LIMIT = FieldValue 
from	tempA As A
where	#fm.ITEM_ID = A.ItemId
and		#fm.LAYER_ID = A.LayerNumber
and		#fm.LEVEL_ID = @level

insert into LogDatabaseUsage values ('generateOasisFiles2','done #FM level 6 Limit',getdate(),null)


select	i.ItemId,
		pl.LayerNumber,
		plv.FieldValue
into	tempB
from	#item as i
join	[Policy] as p on i.PolicyID = p.PolicyID
join	PolicyLayer as pl on p.PolicyID = pl.PolicyID
join	PolicyLayerValues as plv on pl.PolicyLayerID = plv.PolicyLayerID
join	ProfileElement as pe on plv.ProfileElementID = pe.ProfileElementID
where	pe.FieldID = 23

update	#fm 
set		DEDUCTIBLE = FieldValue 
from	tempB As A
where	#fm.ITEM_ID = A.ItemId
and		#fm.LAYER_ID = A.LayerNumber
and		#fm.LEVEL_ID = @level

insert into LogDatabaseUsage values ('generateOasisFiles2','done #FM level 6 Ded',getdate(),null)

select	i.ItemId,		
		pl.LayerNumber,		
		plv.FieldValue		
into	tempC		
from	#item as i		
join	[Policy] as p on i.PolicyID = p.PolicyID		
join	PolicyLayer as pl on p.PolicyID = pl.PolicyID		
join	PolicyLayerValues as plv on pl.PolicyLayerID = plv.PolicyLayerID		
join	ProfileElement as pe on plv.ProfileElementID = pe.ProfileElementID		
where	pe.FieldID = 57		
			
update	#fm 
set		SHARE_PROP_OF_LIM = FieldValue 
from 	tempC As A		
where	#fm.ITEM_ID = A.ItemId		
and		#fm.LAYER_ID = A.LayerNumber		
and		#fm.LEVEL_ID = @level		

insert into LogDatabaseUsage values ('generateOasisFiles2','done #FM level 6 Share',getdate(),null)

--sum financials across coverages to altitem_id level
update	#fm 
set		deductible = a.deductible, 
		limit = a.limit
from	(select ALTITEM_ID, sum(deductible) as deductible, sum(limit) as limit from #fm where LEVEL_ID = 1 group by ALTITEM_ID) as a
where	#fm.ALTITEM_ID = a.ALTITEM_ID
and		#fm.LEVEL_ID = 1

--fixes
--update	#FM 
--set		SHARE_PROP_OF_LIM = a.limit 
--from	#FM
--join	#FM as a on #FM.item_id = a.item_id
--where	a.LEVEL_ID = 5
--and		#FM.LEVEL_ID = 6

--update	#FM set limit = 0 where LEVEL_ID = 5
update	#FM set SHARE_PROP_OF_LIM = 0 where SHARE_PROP_OF_LIM is null
update	#FM set limit = 0 where limit is null
update	#FM set DEDUCTIBLE = 0 where DEDUCTIBLE is null
update  #FM set SHARE_PROP_OF_LIM = 
				case when SHARE_PROP_OF_LIM >= 1 and LIMIT > 1 then SHARE_PROP_OF_LIM/LIMIT
				else SHARE_PROP_OF_LIM end
update  #FM set LIMIT = SHARE_PROP_OF_LIM,
				SHARE_PROP_OF_LIM = 0
			where SHARE_PROP_OF_LIM > 1
			and	 LIMIT = 0

---------------------
update	#FM
set		CALCRULE_ID = CASE	
			WHEN LIMIT = 0 AND SHARE_PROP_OF_LIM = 0 AND DEDUCTIBLE_TYPE = 'B' Then 12
			WHEN LIMIT = 0 AND SHARE_PROP_OF_LIM > 0 AND DEDUCTIBLE_TYPE = 'B' Then 15
			WHEN LIMIT > 0 AND SHARE_PROP_OF_LIM = 0 AND DEDUCTIBLE_TYPE = 'B' Then 1
			WHEN DEDUCTIBLE_TYPE = 'MI' Then 11
			WHEN DEDUCTIBLE_TYPE = 'MA' Then 10
			ELSE 2 END
			
--update % values
Select	LEVEL_ID, LAYER_ID, AGG_ID, sum(TIV) AS TOTAL_TIV
into	#tivtotals
From	#FM
Group By
		LEVEL_ID, LAYER_ID, AGG_ID

update	#FM
set		LIMIT = LIMIT * TOTAL_TIV
from	#FM AS F
join	#tivtotals as t
			on	f.LEVEL_ID = t.LEVEL_ID
			and	f.LAYER_ID = t.LAYER_ID
			and	f.AGG_ID = t.AGG_ID
where	LIMIT > 0 
and		LIMIT <= 1

update	#FM
set		DEDUCTIBLE = DEDUCTIBLE * TOTAL_TIV
from	#FM AS F
join	#tivtotals as t
			on	f.LEVEL_ID = t.LEVEL_ID
			and	f.LAYER_ID = t.LAYER_ID
			and	f.AGG_ID = t.AGG_ID
where	DEDUCTIBLE > 0 
and		DEDUCTIBLE < 1

update	#FM
set		SHARE_PROP_OF_LIM = SHARE_PROP_OF_LIM/LIMIT 
where	SHARE_PROP_OF_LIM > 1 
and		LIMIT > 0


--set policytc ids
create table #PolTC (POLICYTC_ID int identity(1,1), CALCRULE_ID int, deductible float null, limit float null, SHARE_PROP_OF_LIM float null)		
insert into #PolTC		
select 12,0,0,0		
union		
select distinct CALCRULE_ID, deductible, limit, SHARE_PROP_OF_LIM from #FM		
		
update	#FM set POLICYTC_ID = A.POLICYTC_ID 		
from	#PolTC AS A		
where	#FM.CALCRULE_ID = a.CALCRULE_ID		
and		#FM.deductible = a.deductible		
and		#FM.limit = a.limit		
and		#FM.SHARE_PROP_OF_LIM = a.SHARE_PROP_OF_LIM


--correct (index) levels to run in oasis new FM
declare @zeropoltcid int = (select PolicyTC_ID from #PolTC where CALCRULE_ID=12 and deductible=0 and limit=0 and SHARE_PROP_OF_LIM=0)
Create Table #EmptyLevels (Level_ID int null)
Insert Into #EmptyLevels
Select	Level_ID
From	(
		Select	Level_ID,
				Sum(CASE When PolicyTC_ID = @zeropoltcid Then 1 Else 0 End) AS StubPolTC,
				Count(PolicyTC_ID) AS PolTC
		From	#FM
		Where	Level_ID not in (1,6)
		Group By
				Level_ID
		) AS A
Where	StubPolTC = PolTC

select	distinct LEVEL_ID,
		dense_rank() over (order by LEVEL_ID) as newLEVEL_ID
into	#level
from	#FM
where	LEVEL_ID not in (select LEVEL_ID from #EmptyLevels)

delete from #FM where LEVEL_ID in (select LEVEL_ID from #EmptyLevels)
update #FM set LEVEL_ID = newLEVEL_ID from #level where #FM.LEVEL_ID = #level.LEVEL_ID

--select * from #FM

insert into LogDatabaseUsage values ('generateOasisFiles2','updated #FM',getdate(),null)

----FM Tables

--FM_Programme
--level1
Insert into OasisFM_PROGRAMME (FROM_AGG_ID,LEVEL_ID,TO_AGG_ID)
Select	Distinct ALTITEM_ID,
		LEVEL_ID,
		AGG_ID
From	#FM
where	LEVEL_ID = 1

Set	@Level = 1
Select @MaxLevel = max(LEVEL_ID) From #FM

--upper levels
while @Level <= @MaxLevel
begin
	Insert into OasisFM_PROGRAMME (FROM_AGG_ID,LEVEL_ID,TO_AGG_ID)
	Select	Distinct A.AGG_ID,
			B.LEVEL_ID,
			B.AGG_ID
	From	#FM AS A
	Join	#FM AS B
				on	A.ITEM_ID = B.ITEM_ID
	where	A.LEVEL_ID = @Level
	And		B.LEVEL_ID = @Level+1

	Set		@Level = @Level+1
end

--FM_PolicyTC
Insert into OasisFM_POLICYTC (LAYER_ID,LEVEL_ID,AGG_ID,POLICYTC_ID)
Select	Distinct LAYER_ID,
		LEVEL_ID,
		AGG_ID,
		POLICYTC_ID
From	#FM

--FM_Profile
Insert into OasisFM_PROFILE ([policytc_id]
      ,[calcrule_id]
      ,[allocrule_id]
      ,[ccy_id]
      ,[deductible]
      ,[limit]
      ,[share_prop_of_lim]
      ,[deductible_prop_of_loss]
      ,[limit_prop_of_loss]
      ,[deductible_prop_of_tiv]
      ,[limit_prop_of_tiv]
      ,[deductible_prop_of_limit])
Select	Distinct POLICYTC_ID,
		CALCRULE_ID, --0 AS CALCRULE_ID,
		1 AS ALLOCRULE_ID, --Case When LEVEL_ID in (1,2,3) then 1 else 0 end as ALLOCRULE_ID,
		1 AS CCY_ID,
		DEDUCTIBLE,
		LIMIT,
		SHARE_PROP_OF_LIM,
		0 AS [deductible_prop_of_loss],
		0 AS [limit_prop_of_loss],
		0 AS [deductible_prop_of_tiv],
		0 AS [limit_prop_of_tiv],
		0 AS [deductible_prop_of_limit]
From	#FM



update	OasisFM_PROFILE
set		limit_prop_of_loss = SHARE_PROP_OF_LIM,
		SHARE_PROP_OF_LIM = 0
where	CALCRULE_ID = 15

--FM_XRef
Create Table #FMXRef (Item_Id int null, OUTPUT_ID int null,AGG_ID int null,LAYER_ID int null,PolicyID int null, policy_name nvarchar(255) null,layer_name nvarchar(255) null)
insert into #FMXRef (Item_ID,OUTPUT_ID,AGG_ID,LAYER_ID,PolicyID)
Select	Distinct ALTITEM_ID, --Item_Id,
		DENSE_RANK() OVER(ORDER BY ALTITEM_ID,LAYER_ID) AS OUTPUT_ID,
		AGG_ID,
		LAYER_ID,
		PolicyID
From	(
		Select	A.Item_Id,
				ALTITEM_ID,
				AGG_ID,
				LAYER_ID,
				PolicyID
		From	(
				Select	Distinct Item_Id,
						ALTITEM_ID,
						--ALTITEM_ID AS AGG_ID,
						--CoverageId AS AGG_ID,
						i.PolicyID
				From	#FM as f
				join	#item as i on i.ItemId = f.ITEM_ID
				Where	LEVEL_ID = 1
				) AS A
		Join	(
				Select	Distinct Item_Id,
						AGG_ID,
						LAYER_ID
				From	#FM
				Where	LEVEL_ID = @MaxLevel
				) AS B
				on A.Item_Id = B.Item_Id
		) AS C

Update	#FMXRef 
set		policy_name = accountnumber
from	#item
where	#FMXRef.item_id = #item.altitemid

Update	#FMXRef 
set		layer_name = PolicyLayerName
from	PolicyLayer
where	#FMXRef.PolicyID = PolicyLayer.PolicyID
and		#FMXRef.LAYER_ID = PolicyLayer.LayerNumber

Insert into OasisFM_XRef (OUTPUT_ID,AGG_ID,LAYER_ID)
Select Distinct OUTPUT_ID,Item_Id --AGG_ID
					,LAYER_ID From #FMXRef order by Item_Id --AGG_ID

--FM Dict
insert into OasisFMDICT (output_id,item_id,agg_id,layer_id,policy_name,layer_name)
Select Distinct OUTPUT_ID,ITEM_ID, AGG_ID,LAYER_ID,policy_name,layer_name From #FMXRef


insert into LogDatabaseUsage values ('generateOasisFiles2','done FM Tables',getdate(),null)

--select * from #EmptyLevels
--select * from OasisFM_PROGRAMME
--select * from OasisFM_POLICYTC
--select * from OasisFM_PROFILE
--select * from OasisFM_XREF

--select * from #item
--select * from #fm

----drop temp tables
Drop Table #Item
Drop Table #TempPolicy

Drop Table #FM
Drop table #level
Drop Table #PolTC
Drop Table #EmptyLevels
Drop Table #FMXRef
Drop table #exposures
Drop table #coverages
Drop table #tivtotals

Drop table tempA
Drop table tempB
Drop table tempC

--update status
if @@error = 0
	begin
		update progoasis set [status] = 'Loaded' where progoasisid = @ProgOasisId
	end
else
	begin
		update progoasis set [status] = 'Oasis File Generation Error' where progoasisid = @ProgOasisId
	end

Select 'Done'




GO


/****** Object:  StoredProcedure [dbo].[generateOasisFilesOutputs]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


CREATE PROCEDURE [dbo].[generateOasisFilesOutputs] (@processrunid int)
AS

SET ANSI_NULLS ON;
SET ANSI_WARNINGS ON;
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

--declare @processrunid int = 24

declare @progoasisid int = (select progoasisid from processrun where processrunid = @processrunid)

declare @dictresourceid int = (select resourceid from [Resource] where resourcetable = 'ProgOasis' and ResourceKey = @progoasisid and ResourceTypeId = 113)
declare @dictfileid int = (select fileid from fileresource where resourceid = @dictresourceid)
declare @dictlocartionid int = (select locationid from [file] where fileid = @dictfileid)
declare @dictfilename nvarchar(255) = (select [FileName] From [File] where fileid = @dictfileid)
declare @dictlocationname nvarchar(255) = (select LocationName From Location where LocationID = @dictlocartionid)

declare @fmxrefresourceid int = (select resourceid from [Resource] where resourcetable = 'ProgOasis' and ResourceKey = @progoasisid and ResourceTypeId = 127)
declare @fmxreffileid int = (select fileid from fileresource where resourceid = @fmxrefresourceid)
declare @fmxreflocartionid int = (select locationid from [file] where fileid = @fmxreffileid)
declare @fmxreffilename nvarchar(255) = (select [FileName] From [File] where fileid = @fmxreffileid)
declare @fmxreflocationname nvarchar(255) = (select LocationName From Location where LocationID = @fmxreflocartionid)

declare @sql nvarchar(2500)
declare @int int
declare @perspectiveid int
declare @summarysetid int
declare @SummaryLevelID int
declare @SummaryLevelName nvarchar(255)

if (exists(select * from sys.tables where name = 'TempItemDict')) drop table TempItemDict
if (exists(select * from sys.tables where name = 'TempFMDict'))   drop table TempFMDict

Set		@SQL = 'Select item_id,coverage_id,location_id,lob_id,county_id,state_id
From	OpenRowset(''MSDASQL'',''Driver={Microsoft Access Text Driver (*.txt, *.csv)};
          DefaultDir='+@dictlocationname+';Extended Properties="text;HDR=YES;FMT=Delimited'',
          ''select * from '+@dictfilename+''')'

Create Table TempItemDict (item_id int NULL, coverage_id int NULL, location_id int null, lob_id int null, county_id int null, state_id int null)

insert into TempItemDict
exec sp_executesql @SQL

Set		@SQL = 'Select output_id,item_id,agg_id,layer_id
From	OpenRowset(''MSDASQL'',''Driver={Microsoft Access Text Driver (*.txt, *.csv)};
          DefaultDir='+@fmxreflocationname+';Extended Properties="text;HDR=YES;FMT=Delimited'',
          ''select * from '+@fmxreffilename+''')'

Create Table TempFMDict (output_id int NULL, item_id int NULL, agg_id int NULL, layer_id int null)

insert into TempFMDict
exec sp_executesql @SQL


Create Table #outputs (SummaryLevelId int null, SummarySetId int null, PerspectiveId int null, OutputId int null)

--gul
set	@perspectiveid = 1

Insert Into #outputs
select	distinct s.SummaryLevelId, SummarySetId, PerspectiveId, o.OutputID
from	SummaryLevelRun as s
join	outputrun as o on s.SummaryLevelRunID = o.SummaryLevelRunID
where	ProcessRunID = @processrunid 
and		PerspectiveId = @perspectiveid
order by 
		PerspectiveId,SummarySetID

Truncate Table OasisGULSUMMARYXREF

set @summarysetid = 1
while @summarysetid <= (select max(summarysetid) from #outputs where perspectiveid = @perspectiveid)
begin
	set @SummaryLevelID = (select SummaryLevelID from #outputs where summarysetid = @summarysetid)
	set @SummaryLevelName = case when @SummaryLevelID = 1 then '1' else (select SummaryLevelName from SummaryLevel where SummaryLevelID = @SummaryLevelID) + '_id' end

	set @sql = 'select distinct coverage_id, ' + @SummaryLevelName + ' as summary_id, '+ convert(nvarchar,@summarysetid) +' as summaryset_id from TempItemDict'
	insert into OasisGULSUMMARYXREF (coverage_id, summary_id, summaryset_id)
	exec sp_executesql @SQL
	
	set @summarysetid = @summarysetid + 1
end

--fm
set	@perspectiveid = 2
Truncate Table #outputs

Insert Into #outputs
select	distinct s.SummaryLevelId, SummarySetId, PerspectiveId, o.OutputID
from	SummaryLevelRun as s
join	outputrun as o on s.SummaryLevelRunID = o.SummaryLevelRunID
where	ProcessRunID = @processrunid 
and		PerspectiveId = @perspectiveid
order by 
		PerspectiveId,SummarySetID

Truncate Table OasisFMSUMMARYXREF

set @summarysetid = 1
while @summarysetid <= (select max(summarysetid) from #outputs where perspectiveid = @perspectiveid)
begin
	set @SummaryLevelID = (select SummaryLevelID from #outputs where summarysetid = @summarysetid)
	set @SummaryLevelName = case when @SummaryLevelID = 1 then '1' else (select SummaryLevelName from SummaryLevel where SummaryLevelID = @SummaryLevelID) + '_id' end

	if @SummaryLevelName = 'Policy_Id'
		begin
			set @sql = 'select distinct output_id, agg_id as summary_id, '+ convert(nvarchar,@summarysetid) +' as summaryset_id from TempItemDict 
						join TempFMDict on TempItemDict.item_id = TempFMDict.agg_id'
		end
	else
		begin
			set @sql = 'select distinct TempFMDict.output_id, ' + @SummaryLevelName + ' as summary_id, '+ convert(nvarchar,@summarysetid) +' as summaryset_id from TempItemDict 
						join TempFMDict on TempItemDict.item_id = TempFMDict.item_id'
		end
		--print @sql
	insert into OasisFMSUMMARYXREF (output_id, summary_id, summaryset_id)
	exec sp_executesql @SQL

	set @summarysetid = @summarysetid + 1
end

drop table TempItemDict
drop table TempFMDict
drop table #outputs

GO

/****** Object:  StoredProcedure [dbo].[generateOasisFilesRecords]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [dbo].[generateOasisFilesRecords] @ProgOasisId int, @LocationID int, @ItemsFileName nvarchar(255), @CoveragesFileName nvarchar(255), @ItemDictFileName nvarchar(255),
													@FMProgrammeFileName nvarchar(255), @FMPolicyTCFileName nvarchar(255), @FMProfileFileName nvarchar(255),
													@FMXRefFileName nvarchar(255), @FMDictFileName nvarchar(255)
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = '@ProgOasisId = ' + convert(nvarchar,@ProgOasisId) + ', @LocationID = ' + convert(nvarchar,@LocationID)
											+ ', @ItemsFileName = ' + @ItemsFileName
											+ ', @CoveragesFileName = ' + @CoveragesFileName
											+ ', @ItemDictFileName = ' + @ItemDictFileName
											+ ', @FMProgrammeFileName = ' + @FMProgrammeFileName
											+ ', @FMPolicyTCFileName = ' + @FMPolicyTCFileName
											+ ', @FMProfileFileName = ' + @FMProfileFileName
											+ ', @FMXRefFileName = ' + @FMXRefFileName
											+ ', @FMDictFileName = ' + @FMDictFileName
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------
BEGIN

	Declare	@FileId int							= (Select ISNULL(Max(FileId),0) + 1 From [File])
	Declare	@ResourceId int
	Declare	@NextResourceId int					= (Select  ISNULL(Max(ResourceId),0) + 1 From [Resource])
	Declare	@FileResourceId int					= (Select  ISNULL(Max(FileResourceId),0) + 1 From [FileResource])
	Declare	@ProgId int							= (select ProgId from ProgOasis where progoasisid = @progoasisid)
	Declare	@ModelId int						= (select ModelId from ProgOasis where progoasisid = @progoasisid)
	Declare	@SessionID int						= (select SessionId from ProgOasis where progoasisid = @progoasisid)
	
--Oasis Items File
	Set		@ResourceId = (Select ResourceId From [Resource] Where ResourceTable = 'ProgOasis' And ResourceKey = @ProgOasisId And ResourceTypeID = 111) --Oasis Items File
	insert into Resource values (@NextResourceId,'ProgOasis',@ProgOasisId,NULL,211) --legacy file resource type
	update [fileresource] set resourceid = @NextResourceId where resourceid = @ResourceId
	insert into [file] values (@FileId,@ItemsFileName,'Items File ProgOasis ' + convert(nvarchar,@ProgOasisId),1,1,@LocationID,getdate(),getdate(),null,'Sys','Sys',NULL,111) --Oasis Items File
	insert into [FileResource] values (@FileResourceId,@FileId,@ResourceId)
	set		@FileResourceId = @FileResourceId+1
	set		@FileId = @FileId+1
	set		@NextResourceId = @NextResourceId+1

--Oasis Coverages File
	Set		@ResourceId = (Select ResourceId From Resource Where ResourceTable = 'ProgOasis' And ResourceKey = @ProgOasisId And ResourceTypeID = 112) --Oasis Coverages File
	insert into Resource values (@NextResourceId,'ProgOasis',@ProgOasisId,NULL,212) --legacy file resource type
	update [fileresource] set resourceid = @NextResourceId where resourceid = @ResourceId
	insert into [file] values (@FileId,@CoveragesFileName,'Coverages File ProgOasis ' + convert(nvarchar,@ProgOasisId),1,1,@LocationID,getdate(),getdate(),null,'Sys','Sys',NULL,112) --Oasis Coverages File
	insert into [FileResource] values (@FileResourceId,@FileId,@ResourceId)
	set		@FileResourceId = @FileResourceId+1
	set		@FileId = @FileId+1
	set		@NextResourceId = @NextResourceId+1

--Oasis Item Dictionary File
	Set		@ResourceId = (Select ResourceId From Resource Where ResourceTable = 'ProgOasis' And ResourceKey = @ProgOasisId And ResourceTypeID = 113) --Oasis Item Dictionary File
	insert into Resource values (@NextResourceId,'ProgOasis',@ProgOasisId,NULL,213) --legacy file resource type
	update [fileresource] set resourceid = @NextResourceId where resourceid = @ResourceId
	insert into [file] values (@FileId,@ItemDictFileName,'Item Dictionary File ProgOasis ' + convert(nvarchar,@ProgOasisId),1,1,@LocationID,getdate(),getdate(),null,'Sys','Sys',NULL,113) --Oasis Item Dictionary File
	insert into [FileResource] values (@FileResourceId,@FileId,@ResourceId)
	set		@FileResourceId = @FileResourceId+1
	set		@FileId = @FileId+1
	set		@NextResourceId = @NextResourceId+1

--Oasis FM Programme File
	Set		@ResourceId = (Select ResourceId From Resource Where ResourceTable = 'ProgOasis' And ResourceKey = @ProgOasisId And ResourceTypeID = 114) --Oasis FM Programme File
	insert into Resource values (@NextResourceId,'ProgOasis',@ProgOasisId,NULL,214) --legacy file resource type
	update [fileresource] set resourceid = @NextResourceId where resourceid = @ResourceId
	insert into [file] values (@FileId,@FMProgrammeFileName,'FM Programme File ProgOasis ' + convert(nvarchar,@ProgOasisId),1,1,@LocationID,getdate(),getdate(),null,'Sys','Sys',NULL,114) --Oasis FM Programme File
	insert into [FileResource] values (@FileResourceId,@FileId,@ResourceId)
	set		@FileResourceId = @FileResourceId+1
	set		@FileId = @FileId+1
	set		@NextResourceId = @NextResourceId+1

--Oasis FM Policy TC File
	Set		@ResourceId = (Select ResourceId From Resource Where ResourceTable = 'ProgOasis' And ResourceKey = @ProgOasisId And ResourceTypeID = 115) --Oasis FM Policy TC File
	insert into Resource values (@NextResourceId,'ProgOasis',@ProgOasisId,NULL,215) --legacy file resource type
	update [fileresource] set resourceid = @NextResourceId where resourceid = @ResourceId
	insert into [file] values (@FileId,@FMPolicyTCFileName,'FM Policy TC File ProgOasis ' + convert(nvarchar,@ProgOasisId),1,1,@LocationID,getdate(),getdate(),null,'Sys','Sys',NULL,115) --Oasis FM Policy TC File
	insert into [FileResource] values (@FileResourceId,@FileId,@ResourceId)
	set		@FileResourceId = @FileResourceId+1
	set		@FileId = @FileId+1
	set		@NextResourceId = @NextResourceId+1

--Oasis FM Profile File
	Set		@ResourceId = (Select ResourceId From Resource Where ResourceTable = 'ProgOasis' And ResourceKey = @ProgOasisId And ResourceTypeID = 116) --Oasis FM Profile File
	insert into Resource values (@NextResourceId,'ProgOasis',@ProgOasisId,NULL,216) --legacy file resource type
	update [fileresource] set resourceid = @NextResourceId where resourceid = @ResourceId
	insert into [file] values (@FileId,@FMProfileFileName,'FM Profile File ProgOasis ' + convert(nvarchar,@ProgOasisId),1,1,@LocationID,getdate(),getdate(),null,'Sys','Sys',NULL,116) --Oasis FM Profile File
	insert into [FileResource] values (@FileResourceId,@FileId,@ResourceId)
	set		@FileResourceId = @FileResourceId+1
	set		@FileId = @FileId+1
	set		@NextResourceId = @NextResourceId+1

--Oasis FM XRef File
	Set		@ResourceId = (Select ResourceId From Resource Where ResourceTable = 'ProgOasis' And ResourceKey = @ProgOasisId And ResourceTypeID = 117) --Oasis FM XRef File
	insert into Resource values (@NextResourceId,'ProgOasis',@ProgOasisId,NULL,217) --legacy file resource type
	update [fileresource] set resourceid = @NextResourceId where resourceid = @ResourceId
	insert into [file] values (@FileId,@FMXRefFileName,'FM XRef File ProgOasis ' + convert(nvarchar,@ProgOasisId),1,1,@LocationID,getdate(),getdate(),null,'Sys','Sys',NULL,117) --Oasis FM XRef File
	insert into [FileResource] values (@FileResourceId,@FileId,@ResourceId)
	set		@FileResourceId = @FileResourceId+1
	set		@FileId = @FileId+1
	set		@NextResourceId = @NextResourceId+1

--Oasis FM Dict File
	Set		@ResourceId = (Select ResourceId From Resource Where ResourceTable = 'ProgOasis' And ResourceKey = @ProgOasisId And ResourceTypeID = 127) --Oasis FM Dict File
	insert into Resource values (@NextResourceId,'ProgOasis',@ProgOasisId,NULL,227) --legacy file resource type
	update [fileresource] set resourceid = @NextResourceId where resourceid = @ResourceId
	insert into [file] values (@FileId, @FMDictFileName,'FM Dict File ProgOasis ' + convert(nvarchar,@ProgOasisId),1,1,@LocationID,getdate(),getdate(),null,'Sys','Sys',NULL,120) --Oasis FM Dict File
	insert into [FileResource] values (@FileResourceId,@FileId,@ResourceId)
	set		@FileResourceId = @FileResourceId+1
	set		@FileId = @FileId+1
	set		@NextResourceId = @NextResourceId+1

	truncate table dbo.OasisCoverages
	truncate table dbo.OasisITEMS
	truncate table dbo.OasisITEMDict
	truncate table dbo.OasisFM_PROGRAMME
	truncate table dbo.OasisFM_POLICYTC
	truncate table dbo.OasisFM_PROFILE
	truncate table dbo.OasisFM_XREF
	truncate table dbo.OasisFMDict

END







GO
/****** Object:  StoredProcedure [dbo].[generateOasisFilesRecordsOutputs]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [dbo].[generateOasisFilesRecordsOutputs] (@ProcessRunId int, @LocationID int, @GulSummaryXrefFileName nvarchar(255), @FMSummaryXrefFileName nvarchar(255))
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------
BEGIN

	Declare	@FileId int							= (Select  ISNULL(Max(FileId),0) + 1 From [File])
	Declare	@NextResourceId int					= (Select  ISNULL(Max(ResourceId),0) + 1 From [Resource])
	Declare	@FileResourceId int					= (Select  ISNULL(Max(FileResourceId),0) + 1 From [FileResource])

SELECT * FROM resourcetype

--GulSummaryXref
	insert into Resource values (@NextResourceId,'ProcessRun',@ProcessRunId,NULL,109) 
	insert into [file] values (@FileId,@GulSummaryXrefFileName,'GUL Summary Xref File ProcessRun ' + convert(nvarchar,@ProcessRunId),1,1,@LocationID,getdate(),getdate(),null,'Sys','Sys',NULL,118) 
	insert into [FileResource] values (@FileResourceId,@FileId,@NextResourceId)

	set		@FileResourceId = @FileResourceId+1
	set		@FileId = @FileId+1
	set		@NextResourceId = @NextResourceId+1

--FMSummaryXref
	insert into Resource values (@NextResourceId,'ProcessRun',@ProcessRunId,NULL,110) 
	insert into [file] values (@FileId,@FMSummaryXrefFileName,'FM Summary Xref File ProcessRun ' + convert(nvarchar,@ProcessRunId),1,1,@LocationID,getdate(),getdate(),null,'Sys','Sys',NULL,119) 
	insert into [FileResource] values (@FileResourceId,@FileId,@NextResourceId)

	truncate table dbo.OasisGULSUMMARYXREF
	truncate table dbo.OasisFMSUMMARYXREF

	select 'Done'
END

GO
/****** Object:  StoredProcedure [dbo].[getAccount]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [dbo].[getAccount]
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

SELECT	[AccountID] as "Account ID",
		[AccountName] as "Account Name"
FROM	[dbo].[Account] where Deleted = 0
ORDER BY
		[AccountName]

GO

/****** Object:  StoredProcedure [dbo].[getAPI1aReturnData]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE Procedure [dbo].[getAPI1aReturnData] @ProgOasisId int, @FileName Nvarchar(255), @SessionId int
AS
SET ANSI_NULLS ON;
SET ANSI_WARNINGS ON;
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------



--declare @ProgOasisId int = 2, @FileName Nvarchar(255) = 'ExposureKeys_20170510151603.csv', @SessionId int = 1

--get next incremental fileid, resourceid and identify resourceid for return file for progoasis
Declare	@FileId int				= (Select isnull(max(FileId)+1,1) From [File])
Declare	@LegacyResourceId int	= (Select isnull(max(ResourceId)+1,1) From [Resource])
Declare	@FileResourceId int		= (Select isnull(max(FileResourceId)+1,1) From [FileResource])
Declare	@ResourceId int			= (Select ResourceId From [Resource] Where ResourceTable = 'ProgOasis' And ResourceKey = @ProgOasisId And ResourceTypeId = 106)
Declare @LocationId int			= 102 --Output Files for API
Declare @locationname nvarchar(255) 
								= (select locationname from location where locationid = @locationid)
Declare @ModelId int			= (Select ModelId from ProgOasis Where ProgOasisId = @ProgOasisId)
Declare @SQL nvarchar(2500)

--generate legacy resource record
Insert Into  [Resource] (ResourceID,ResourceTable,ResourceKey,ResourceQualifier,ResourceTypeID)
Select		@LegacyResourceId,
			'ProgOasis',
			@ProgOasisId,
			NULL,
			206 --Legacy Lookup Return File

--set existing file record to legacy resource id
Update [FileResource] Set ResourceID = @LegacyResourceId Where ResourceID = @ResourceId

--insert file record for new file
Insert Into [File] (FileId, [FileName],FileDesc,SourceID,OwnerID,LocationID,DateTimeCreated,
					DateTimeUpdated,DateTimeDeleted,OwnerNameCreated,OwnerNameUpdated,OwnerNameDeleted,
					FileTypeId)
Select		@FileId,
			@FileName,
			'Model Lookup Return File',
			1,
			1,
			@LocationId, 
			getdate(),
			getdate(),
			null,
			'API1a',
			'API1a',
			NULL,
			106 -- file type Lookup Return Key File

Insert Into [FileResource] (FileResourceID, FileID, ResourceID) Select @FileResourceId, @FileId, @ResourceId

--update progoasisrecord
Update ProgOasis Set API1aDateTime = getdate(), SessionId = @SessionId Where ProgOasisId = @ProgOasisId

--populate records into progoasiskeys table
Declare	@ProgOasisKeyId int = (Select isnull(max(ProgOasisKeyId),0) From ProgOasisKeys)

--clear progoasiskeys records if exists
Delete From ProgOasisKeys Where ProgOasisId = @ProgOasisId

--populate
Set @SQL = '
Select	ROW_NUMBER () OVER (Order By LocId,MP.ModelPerilName,CoverageId) + convert(int,' + convert(nvarchar,@ProgOasisKeyId) + ') AS ProgOasisKeyId,
		' + convert(nvarchar,@ProgOasisId) + ' AS ProgOasisId,
		LocId,
		MP.ModelPerilID,
		MCT.ModelCoverageTypeId,
		AreaPerilId,
		VulnerabilityId 
From   OpenRowset(''MSDASQL'',''Driver={Microsoft Access Text Driver (*.txt, *.csv)};
          DefaultDir=' + @LocationName + ';Extended Properties="text;HDR=YES;FMT=Delimited'',
          ''select * from ' + @FileName + ''') AS A
Join	ModelPeril AS MP 
				on convert(varchar,A.PerilId) = MP.ModelPerilName
				and MP.ModelId = '+convert(nvarchar,@ModelId) + '
Join	ModelCoverageType AS MCT
				on convert(varchar,A.CoverageId) = MCT.ModelCoverageTypeName
				and MCT.ModelId = '+convert(nvarchar,@ModelId) + ''


Insert Into ProgOasisKeys (ProgOasisKeyId,ProgOasisId,LocID,PerilId,CoverageId,AreaPerilId,VulnerabilityId)
exec sp_executesql @SQL

Select 'Done'




GO
/****** Object:  StoredProcedure [dbo].[getAPI1bReturnData]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE Procedure [dbo].[getAPI1bReturnData] @ProgId int, @ModelId int,  @SessionId int, @APIStatus Nvarchar(255)
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

Update	ProgOasis 
Set		[Status] = 'API1b ' + @APIStatus,
		API1bDateTime = getdate()
Where	ProgId = @ProgId
And		ModelId = @ModelId
And		SessionId = @SessionId







GO
/****** Object:  StoredProcedure [dbo].[getAPIURL]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[getAPIURL] @modelid	int
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

Select	url AS apiurl,
		OasisSystemName AS SystemName
From	OasisSystem AS OS
Join	ModelResource AS MR on OS.OasisSystemID = MR.OasisSystemID
Where	MR.ModelID = @modelid
And		MR.ResourceTypeID = 1000 --api url resource type

GO


/****** Object:  StoredProcedure [dbo].[getCompanies]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE procedure [dbo].[getCompanies]
as
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

SELECT CompanyID AS "Company ID",
CompanyName AS "Company Name",
CompanyDomicile AS "Company Domicile",
CompanyLegalName AS "Company Legal Name",
CompanyRegistrationNo AS "Company Registration Number"
FROM Company where Deleted = 0 ORDER BY CompanyID DESC

GO

/****** Object:  StoredProcedure [dbo].[getEventOccurrence]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


Create Procedure [dbo].[getEventOccurrence] @ProgOasisId int
As
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

Declare @ModelId int = (Select ModelId From ProgOasis Where ProgOasisId = @ProgOasisId)

Select	ModelResourceValue AS EventSet
From	ModelResource
Where	ModelId = @ModelId
And		ResourceTypeID = 304
Order By
		ModelResourceID

GO
/****** Object:  StoredProcedure [dbo].[getEventSet]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


Create Procedure [dbo].[getEventSet] @ProgOasisId int
As
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

Declare @ModelId int = (Select ModelId From ProgOasis Where ProgOasisId = @ProgOasisId)

Select	ModelResourceValue AS EventSet
From	ModelResource
Where	ModelId = @ModelId
And		ResourceTypeID = 303
Order By
		ModelResourceID

GO

/****** Object:  StoredProcedure [dbo].[getFileDataForFile]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE Procedure [dbo].[getFileDataForFile] @FileID int
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

Declare	@File nvarchar(255)
Declare	@Folder nvarchar(255)
Declare @SQL nvarchar(2500)

Select	@File = [FileName] from [File] where FileId = @FileID
Select	@Folder = LocationName From Location Where LocationId = (Select LocationId from [File] where FileId = @FileID)

Set @SQL = '
Select * 
From   OpenRowset(''MSDASQL'',''Driver={Microsoft Access Text Driver (*.txt, *.csv)};
          DefaultDir=' + @Folder + ';Extended Properties="text;HDR=YES;FMT=Delimited'',
          ''select * from ' + @File + ''')'

exec sp_executesql @SQL




GO

/****** Object:  StoredProcedure [dbo].[getFileLocationPath]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [dbo].[getFileLocationPath]
	@FileTypeDesc nvarchar(255)
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

BEGIN
	declare @LocationPathUnix nvarchar(255)
	SET NOCOUNT ON;
	if @FileTypeDesc = 'Exposure File'
	  begin
		   select @LocationPathUnix = LocationPathUnix from dbo.Location where LocationID = 103
		   --select @LocationPathUnix
	  end
 
	  else 
	  begin
			select @LocationPathUnix = LocationPathUnix from dbo.Location where LocationID = 101
			--select @LocationPathUnix
	   end
	 select @LocationPathUnix         
END

select * from location

GO

/****** Object:  StoredProcedure [dbo].[getFileSourceAccountFile]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE Procedure [dbo].[getFileSourceAccountFile]
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

Select	[FileName],
		[FileId]
From	[File]
Where	[FileTypeId] = 102
Order By
		[FileName]


GO
/****** Object:  StoredProcedure [dbo].[getFileSourceLocationFile]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE Procedure [dbo].[getFileSourceLocationFile]
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

Select	[FileName],
		[FileId]
From	[File]
Where	[FileTypeId] = 101
Order By
		[FileName]


GO

/****** Object:  StoredProcedure [dbo].[getFileViewerTable]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE procedure [dbo].[getFileViewerTable]
(
	@ProcessRunID int = -1
)
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

Declare	@Folder nvarchar(255)
Declare @NumberLocations int
Declare	@int int = 1

SELECT	F.FileID,
		F.[FileName] AS [File Name],
		F.FileDesc AS [Description],
		L.LocationName AS [Location],
		L.LocationPathUnix AS [Location Unix],
		IsNull(FT.FileTypeDesc,'Unknown') AS [File Type],
		O.OwnerName AS [Owner],
		S.SourceName AS [Source],
		R.ResourceTable AS [Resource Table],
		R.ResourceKey AS [Resource Key]
INTO	#FileTable
FROM	[dbo].[File] AS F
Join	[dbo].[Owner] AS O on F.OwnerID = O.OwnerID
Join	[dbo].[Source] AS S on F.SourceID = S.SourceID
Join	[dbo].[Location] AS L on F.LocationID = L.LocationID
Join	FileResource AS FR on F.FileID = FR.FileID 
Join	[Resource] AS R on FR.ResourceID = R.ResourceID 
Left Join 
		[dbo].[Prog] AS PR on R.ResourceKey = PR.ProgID
Left Join	
		[dbo].[FileType] AS FT on F.FileTypeId = FT.FileTypeId


--get files in actual folder structure
Create Table #Locations 
	(
	ID int identity, 
	LocationName Nvarchar(255)
	)

Insert Into	#Locations 
Select	LocationName 
from	Location 
--where	locationid < 1000 --to be fixed
where /*LocationDesc = case when @ProcessRunID <> -1 then 'ProcessRun_' + convert(varchar,@ProcessRunId)
	               else LocationDesc end
-- to be changed after Output file linking functionality is implemented
*/LocationDesc = case when @ProcessRunID <> -1 then 'ProcessRun_' + convert(varchar,@ProcessRunId) + '_Outputs'
	               else LocationDesc end
Order By 
		LocationName

Select	@NumberLocations = max(ID) From #Locations


Create Table #Files 
	(
	FolderName Nvarchar(255), 
	[FileName] Nvarchar(255)
	)

Create Table #DirTree 
	(
	[Name] Nvarchar(255), 
	Depth int, 
	IsFile int
	)


While @int <= @NumberLocations
Begin
	Select	@Folder = LocationName From #Locations Where ID = @int

	Insert Into #DirTree EXEC xp_dirtree @Folder, 1, 1

	Insert Into #Files Select @Folder, [Name] From #DirTree Where IsFile = 1 Order By [Name]

	Truncate Table #DirTree

	Set @int = @int+1
End

Select	FT.* 
From	#FileTable AS FT
Join	#Files AS F
			on	F.[FileName] collate DATABASE_DEFAULT = FT.[File Name] collate DATABASE_DEFAULT
			and	F.FolderName collate DATABASE_DEFAULT = FT.Location collate DATABASE_DEFAULT
order by FileID desc


Drop Table #Locations
Drop Table #Files
Drop Table #DirTree
Drop Table #FileTable


GO

/****** Object:  StoredProcedure [dbo].[getLocationNameForProcessRunId]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
Create Procedure [dbo].[getLocationNameForProcessRunId] (@ProcessRunId int)
AS

SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

Declare @ProgOasisID int =		(Select ProgOasisID from ProcessRun Where ProcessRunId = @ProcessRunId)
Declare @ItemResourceID int =	(Select ResourceID from [Resource] Where ResourceTable = 'ProgOasis' And ResourceKey = @ProgOasisID And ResourceTypeID = 111)
Declare @ItemFileID int =		(Select FileID from FileResource Where ResourceID = @ItemResourceID)
Declare @LocationID int =		(Select LocationID from [File] Where FileID = @ItemFileID)

Select	LocationPathUnix
From	Location
Where	LocationID = @LocationID


GO

/****** Object:  StoredProcedure [dbo].[getModel]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[getModel]
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

SELECT	ModelID as "Model ID", 
		ModelName as "Model Name",
		ModelDescription as "Model Description"
FROM	model
Where	Deleted = 0
order by 
		ModelName asc

GO

/****** Object:  StoredProcedure [dbo].[getModelFileExtension]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE procedure [dbo].[getModelFileExtension] @ProgOasisID int
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

Declare @modelID int = (Select modelid from progoasis where progoasisid = @ProgOasisID)
Declare @Ret nvarchar(255) = (Select ModelResourceValue from modelresource where modelid = @modelID and ResourceTypeID = 305)

Select	@Ret AS ReturnValue


GO

/****** Object:  StoredProcedure [dbo].[getModelResource]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[getModelResource] @ModelID int
As
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------
Select 
	ModelResourceID as "Model Resource ID",
	ModelResourceName as "Model Resource Name",
	ResourceTypeID as "Resource Type ID",
	OasisSystemID as "Oasis System ID",
	ModelID as "Model ID",
	ModelResourceValue as "ModelResourceValue"
From ModelResource
Where @ModelID=ModelID
order by ModelResourceID

GO

/****** Object:  StoredProcedure [dbo].[getOasisSystemID]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE procedure [dbo].[getOasisSystemID]
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

SELECT	OasisSystemID,
		OasisSystemName
FROM	OasisSystem
ORDER BY
		OasisSystemName

GO

/****** Object:  StoredProcedure [dbo].[getOasisURL]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[getOasisURL] @processrunid	int
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

declare @progoasisid int = (select progoasisid from processrun where processrunid = @processrunid)
declare @modelid int = (select modelid from progoasis where progoasisid = @progoasisid)
declare @serviceid int = (select serviceid from [service] where modelid = @modelid and servicetypeid = 1)
declare @oasissystemid int = (select oasissystemid from OasisSystemService where serviceid = @serviceid)


	declare		@url nvarchar(255) 
	set			@url =	(
						select			'http://' 
									+	os.url 
									+	case 
											when os.port is null 
											then '' 
											else ':' + convert(nvarchar,os.Port)  
											end
									--+	'/oasis/' 
						from	OasisSystem os
						where	os.oasissystemid = @oasissystemid
						)

	select @url


GO

/****** Object:  StoredProcedure [dbo].[getOutputOptionOutputs]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE procedure [dbo].[getOutputOptionOutputs] (
@OutputOption nvarchar(255) = null,
@processrunid int  = null
)
AS 
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

--declare @OutputOption nvarchar(255) = 'Test1'

create table #GroupParameter ([Group] nvarchar(255) null,[Parameter] nvarchar(255) null,GroupID int null,GroupParamID int null)

/*insert into #GroupParameter
select	lower(perspectivename) + sl.parameterstub  as [Group], 
		lower(perspectivename) + sl.parameterstub + at.ParameterStub AS [Parameter],
		dense_rank() over (order by lower(perspectivename) + sl.parameterstub) as GroupID,
		dense_rank() over (partition by lower(perspectivename) + sl.parameterstub order by lower(perspectivename) + sl.parameterstub + at.ParameterStub) as GroupParamID
from	outputtype as ot
join	summarylevel as sl on ot.SummaryLevelID = sl.SummaryLevelID
join	AnalysisType as at on ot.AnalysisTypeID =at.AnalysisTypeID
join	Perspective as p on ot.PerspectiveID = p.PerspectiveID
Where	ot.outputtypeid in (select distinct OutputTypeID from OutputOptions where OutputOption = @OutputOption)
*/

if @OutputOption is not null 
begin  
	insert into #GroupParameter
	select	lower(perspectivename) + sl.parameterstub  as [Group], 
			lower(perspectivename) + sl.parameterstub + at.ParameterStub AS [Parameter],
			dense_rank() over (order by lower(perspectivename) + sl.parameterstub) as GroupID,
			dense_rank() over (partition by lower(perspectivename) + sl.parameterstub order by lower(perspectivename) + sl.parameterstub + at.ParameterStub) as GroupParamID
	from	outputtype as ot
	join	summarylevel as sl on ot.SummaryLevelID = sl.SummaryLevelID
	join	AnalysisType as at on ot.AnalysisTypeID =at.AnalysisTypeID
	join	Perspective as p on ot.PerspectiveID = p.PerspectiveID
	Where	ot.outputtypeid in (select distinct OutputTypeID from OutputOptions where OutputOption = @OutputOption)
end

if @processrunid is not null 
begin  
	insert into #GroupParameter
	select	lower(perspectivename) + sl.parameterstub  as [Group], 
			lower(perspectivename) + sl.parameterstub + at.ParameterStub AS [Parameter],
			dense_rank() over (order by lower(perspectivename) + sl.parameterstub) as GroupID,
			dense_rank() over (partition by lower(perspectivename) + sl.parameterstub order by lower(perspectivename) + sl.parameterstub + at.ParameterStub) as GroupParamID
	from	outputtype as ot
	join	summarylevel as sl on ot.SummaryLevelID = sl.SummaryLevelID
	join	AnalysisType as at on ot.AnalysisTypeID =at.AnalysisTypeID
	join	Perspective as p on ot.PerspectiveID = p.PerspectiveID
	Where	ot.outputtypeid in (select OutputTypeID from OutputRun where ElementRunID in (select ElementRunID from ElementRun where ProcessRunID = @processrunid))
end


Declare @GroupID int = 1
Declare @GroupParamID int = 1
Declare @MaxGroupID int
Declare @Group nvarchar(255)
Declare @Parameter nvarchar(255)
Declare @ParameterStr nvarchar(2500) = ''

create table #GroupParameterReturn ([Group] nvarchar(255) null,[Parameter] nvarchar(2500))

while @GroupID <= (Select max(GroupID) From #GroupParameter)
begin
	set @Group = (select distinct [Group] From #GroupParameter where GroupID = @GroupID)
	set @MaxGroupID = (Select max(GroupParamID) From #GroupParameter Where GroupID = @GroupID)
	while @GroupParamID <= @MaxGroupID
	begin
		set @Parameter = (select Parameter From #GroupParameter where GroupID = @GroupID and GroupParamId = @GroupParamID)
		set @ParameterStr = @ParameterStr + @Parameter
		if  @GroupParamID < @MaxGroupID
		begin
			set @ParameterStr = @ParameterStr + ','
		end
		set @GroupParamID = @GroupParamID + 1
	end

	insert into #GroupParameterReturn select @Group,@ParameterStr
	set @ParameterStr = ''
	set @GroupParamID = 1
	set @GroupID = @GroupID + 1
end

--select * from #GroupParameter
select * from #GroupParameterReturn

drop table #GroupParameter
drop table #GroupParameterReturn






GO
/****** Object:  StoredProcedure [dbo].[getOutputOptions]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
create procedure [dbo].[getOutputOptions]
AS 
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

select distinct OutputOption from OutputOptions order by OutputOption







GO
/****** Object:  StoredProcedure [dbo].[getOutputSummary]    Script Date: 10/10/2016 14:00:40 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [dbo].[getOutputSummary] @ProcessRunID int
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

create table #return
	(
	SummaryType nvarchar(255),
	Value nvarchar(255)
	)

create table #Summary 
	(
	SummaryType nvarchar(255),
	Value float
	)

declare @FileID int
declare @FileName nvarchar(255)
declare @LocationName nvarchar(255)
declare @SQL nvarchar(2500)
declare @progoasisid int = (Select progoasisid from processrun where processrunid = @ProcessRunID)


--Exposure Locations
set		@FileID = null

select	@FileID = F.FileID
From	[file] as F
join	FileResource AS FR on F.FileID = FR.FileID
Join	[Resource] as R on FR.ResourceID = R.ResourceID
Join	ResourceType AS RT on R.ResourceTypeID = RT.ResourceTypeID
Where	ResourceTable = 'ProgOasis'
And		ResourceKey = @progoasisid
And		RT.ResourceTypeName = 'Oasis Item Dictionary File'

select	@FileName = [FileName], @LocationName = LocationName from [file] as f join location as l on f.locationid = l.locationid where FileID = @FileID

if @FileName is not null
begin
	Set @SQL = '
	Select	''exposure location count'' AS MeasureName, count(distinct location_id) AS MeasureValue
	From	OpenRowset(''MSDASQL'',''Driver={Microsoft Access Text Driver (*.txt, *.csv)};
			  DefaultDir=' + @LocationName + ';Extended Properties="text;HDR=YES;FMT=Delimited'',
			  ''select * from ' + @FileName + ''')'

	insert into #Summary
	exec sp_executesql @SQL
end


--Exposure TIV
set		@FileID = null
set		@FileName = null

select	@FileID = F.FileID
From	[file] as F
join	FileResource AS FR on F.FileID = FR.FileID
Join	[Resource] as R on FR.ResourceID = R.ResourceID
Join	ResourceType AS RT on R.ResourceTypeID = RT.ResourceTypeID
Where	ResourceTable = 'ProgOasis'
And		ResourceKey = @progoasisid
And		RT.ResourceTypeName = 'Oasis Coverage File'

select	@FileName = [FileName], @LocationName = LocationName from [file] as f join location as l on f.locationid = l.locationid where FileID = @FileID

if @FileName is not null
begin
	Set @SQL = '
	Select	''exposure TIV'' AS MeasureName, sum(convert(float,tiv)) AS MeasureValue
	From	OpenRowset(''MSDASQL'',''Driver={Microsoft Access Text Driver (*.txt, *.csv)};
			  DefaultDir=' + @LocationName + ';Extended Properties="text;HDR=YES;FMT=Delimited'',
			  ''select * from ' + @FileName + ''')'

	insert into #Summary
	exec sp_executesql @SQL
end

--Portfolio GU AAL (NI)
set		@FileID = null
set		@FileName = null

select	@FileID = F.FileID
From	[file] as F
join	FileResource AS FR on F.FileID = FR.FileID
Join	[Resource] as R on FR.ResourceID = R.ResourceID
Join	ResourceType AS RT on R.ResourceTypeID = RT.ResourceTypeID
Where	ResourceTable = 'ProcessRun'
And		ResourceKey = @processrunid
And		F.FileDesc = 'Output Portfolio Level GUL AAL'

select	@FileName = [FileName], @LocationName = LocationName from [file] as f join location as l on f.locationid = l.locationid where FileID = @FileID

if @FileName is not null
begin
	Set @SQL = '
	Select	''portfolio GU AAL (NI)'' AS MeasureName, mean AS MeasureValue
	From	OpenRowset(''MSDASQL'',''Driver={Microsoft Access Text Driver (*.txt, *.csv)};
			  DefaultDir=' + @LocationName + ';Extended Properties="text;HDR=YES;FMT=Delimited'',
			  ''select * from ' + @FileName + ''')
	Where	[Type] = ''1'''

	insert into #Summary
	exec sp_executesql @SQL
end

--Portfolio GU AAL (Sample)
set		@FileID = null
set		@FileName = null

select	@FileID = F.FileID
From	[file] as F
join	FileResource AS FR on F.FileID = FR.FileID
Join	[Resource] as R on FR.ResourceID = R.ResourceID
Join	ResourceType AS RT on R.ResourceTypeID = RT.ResourceTypeID
Where	ResourceTable = 'ProcessRun'
And		ResourceKey = @processrunid
And		F.FileDesc = 'Output Portfolio Level GUL AAL'

select	@FileName = [FileName], @LocationName = LocationName from [file] as f join location as l on f.locationid = l.locationid where FileID = @FileID

if @FileName is not null
begin
	Set @SQL = '
	Select	''portfolio GU AAL (Sample)'' AS MeasureName, mean AS MeasureValue
	From	OpenRowset(''MSDASQL'',''Driver={Microsoft Access Text Driver (*.txt, *.csv)};
			  DefaultDir=' + @LocationName + ';Extended Properties="text;HDR=YES;FMT=Delimited'',
			  ''select * from ' + @FileName + ''')
	Where	[Type] = ''2'''

	insert into #Summary
	exec sp_executesql @SQL
end

--Portfolio IL AAL (NI)
set		@FileID = null
set		@FileName = null

select	@FileID = F.FileID
From	[file] as F
join	FileResource AS FR on F.FileID = FR.FileID
Join	[Resource] as R on FR.ResourceID = R.ResourceID
Join	ResourceType AS RT on R.ResourceTypeID = RT.ResourceTypeID
Where	ResourceTable = 'ProcessRun'
And		ResourceKey = @processrunid
And		F.FileDesc = 'Output Portfolio Level IL AAL'

select	@FileName = [FileName], @LocationName = LocationName from [file] as f join location as l on f.locationid = l.locationid where FileID = @FileID

if @FileName is not null
begin
	Set @SQL = '
	Select	''portfolio IL AAL (NI)'' AS MeasureName, mean AS MeasureValue
	From	OpenRowset(''MSDASQL'',''Driver={Microsoft Access Text Driver (*.txt, *.csv)};
			  DefaultDir=' + @LocationName + ';Extended Properties="text;HDR=YES;FMT=Delimited'',
			  ''select * from ' + @FileName + ''')
	Where	[Type] = ''1'''

	insert into #Summary
	exec sp_executesql @SQL
end

--Portfolio IL AAL (Sample)
set		@FileID = null

select	@FileID = F.FileID
From	[file] as F
join	FileResource AS FR on F.FileID = FR.FileID
Join	[Resource] as R on FR.ResourceID = R.ResourceID
Join	ResourceType AS RT on R.ResourceTypeID = RT.ResourceTypeID
Where	ResourceTable = 'ProcessRun'
And		ResourceKey = @processrunid
And		F.FileDesc = 'Output Portfolio Level IL AAL'

select	@FileName = [FileName], @LocationName = LocationName from [file] as f join location as l on f.locationid = l.locationid where FileID = @FileID

if @FileName is not null
begin
	Set @SQL = '
	Select	''portfolio IL AAL (Sample)'' AS MeasureName, mean AS MeasureValue
	From	OpenRowset(''MSDASQL'',''Driver={Microsoft Access Text Driver (*.txt, *.csv)};
			  DefaultDir=' + @LocationName + ';Extended Properties="text;HDR=YES;FMT=Delimited'',
			  ''select * from ' + @FileName + ''')
	Where	[Type] = ''2'''

	insert into #Summary
	exec sp_executesql @SQL
end


update #Summary set Value = round(value,0)

insert into #return select SummaryType, left(convert(nvarchar,convert(money,Value,0),1),charindex('.',convert(nvarchar,convert(money,Value,0),1))-1) from #Summary

--runtime parameters
insert into #return exec [getProcessRunParams] @ProcessRunID

update #return set SummaryType = replace(SummaryType,'_',' ')

select * from #return

drop table #Summary
drop table #return


go

CREATE PROCEDURE [dbo].[getOutputSummaryEP] @ProcessRunID int
AS
SET ANSI_NULLS ON;
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

--declare @ProcessRunID int = 19

declare @FileName nvarchar(255)
declare @LocationName nvarchar(255)
declare @SQL nvarchar(2500)

Create Table #Files (FileID int null, [FileName] nvarchar(255) null, FileDesc nvarchar(255) null, LocationName nvarchar(255) null)

Insert Into #Files
Select	F.FileID,
		F.[FileName],
		F.FileDesc,
		L.LocationName
From	[File] AS F
Join	FileResource AS FR on F.FileID = FR.FileID
Join	[Resource] AS R on FR.ResourceID = R.ResourceID
Join	Location AS L on F.LocationID = L.LocationID
Where	R.ResourceTable = 'ProcessRun'
And		R.ResourceKey = @ProcessRunID

create table #EP 
	(
	ReturnPeriod int null,
	GUL_OEP float null,
	GUL_AEP float null,
	IL_OEP float null,
	IL_AEP float null
	)

update #ep set GUL_OEP = 0, GUL_AEP = 0, IL_OEP = 0, IL_AEP = 0

create table #output (Metric nvarchar(255) null, return_period float null, Loss float null)

--gul oep
set		@FileName = null
select	@FileName = [FileName], @LocationName = LocationName from #Files where FileDesc = 'Output Portfolio Level GUL LEC Full Uncertainty OEP'

if @FileName is not null
begin
	Set @SQL = '
	Select	''GUL_OEP'', return_period, Loss
	From	OpenRowset(''MSDASQL'',''Driver={Microsoft Access Text Driver (*.txt, *.csv)};
			  DefaultDir=' + @LocationName + ';Extended Properties="text;HDR=YES;FMT=Delimited'',
			  ''select * from ' + @FileName + ''')'

	insert into #output
	exec sp_executesql @SQL

end


--gul aep
set		@FileName = null
select	@FileName = [FileName], @LocationName = LocationName from #Files where FileDesc = 'Output Portfolio Level GUL LEC Full Uncertainty AEP'

if @FileName is not null
begin
	Set @SQL = '
	Select	''GUL_AEP'', return_period, Loss
	From	OpenRowset(''MSDASQL'',''Driver={Microsoft Access Text Driver (*.txt, *.csv)};
			  DefaultDir=' + @LocationName + ';Extended Properties="text;HDR=YES;FMT=Delimited'',
			  ''select * from ' + @FileName + ''')'

	insert into #output
	exec sp_executesql @SQL

end

--il oep
set		@FileName = null
select	@FileName = [FileName], @LocationName = LocationName from #Files where FileDesc = 'Output Portfolio Level IL LEC Full Uncertainty OEP'

if @FileName is not null
begin
	Set @SQL = '
	Select	''IL_OEP'', return_period, Loss
	From	OpenRowset(''MSDASQL'',''Driver={Microsoft Access Text Driver (*.txt, *.csv)};
			  DefaultDir=' + @LocationName + ';Extended Properties="text;HDR=YES;FMT=Delimited'',
			  ''select * from ' + @FileName + ''')'

	insert into #output
	exec sp_executesql @SQL

end

--il aep
set		@FileName = null
select	@FileName = [FileName], @LocationName = LocationName from #Files where FileDesc = 'Output Portfolio Level IL LEC Full Uncertainty AEP'

if @FileName is not null
begin
	Set @SQL = '
	Select	''IL_AEP'', return_period, Loss
	From	OpenRowset(''MSDASQL'',''Driver={Microsoft Access Text Driver (*.txt, *.csv)};
			  DefaultDir=' + @LocationName + ';Extended Properties="text;HDR=YES;FMT=Delimited'',
			  ''select * from ' + @FileName + ''')'


	insert into #output
	exec sp_executesql @SQL

end


insert into #ep select distinct return_period, 0,0,0,0 from #output order by return_period
update #EP set GUL_OEP = Loss from #output where #EP.ReturnPeriod = #output.return_period and metric = 'GUL_OEP'
update #EP set GUL_AEP = Loss from #output where #EP.ReturnPeriod = #output.return_period and metric = 'GUL_AEP'
update #EP set IL_OEP = Loss from #output where #EP.ReturnPeriod = #output.return_period and metric = 'IL_OEP'
update #EP set IL_AEP = Loss from #output where #EP.ReturnPeriod = #output.return_period and metric = 'IL_AEP'

select * from #ep

drop table #ep
drop table #Files
drop table #output


/****** Object:  StoredProcedure [dbo].[getOutputSummaryEP2]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO



CREATE PROCEDURE [dbo].[getOutputSummaryEP2] @ProcessRunID int
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

--declare @ProcessRunID int = 24

declare @FileName nvarchar(255)
declare @LocationName nvarchar(255)
declare @SQL nvarchar(2500)

Create Table #Files (FileID int, [FileName] nvarchar(255), FileDesc nvarchar(255), LocationName nvarchar(255))

Insert Into #Files
Select	F.FileID,
		F.[FileName],
		F.FileDesc,
		L.LocationName
From	[File] AS F
Join	FileResource AS FR on F.FileID = FR.FileID
Join	[Resource] AS R on FR.ResourceID = R.ResourceID
Join	Location AS L on F.LocationID = L.LocationID
Where	R.ResourceTable = 'ProcessRun'
And		R.ResourceKey = @ProcessRunID

create table #EP 
	(
	ReturnPeriod int,
	GUL_OEP float default 1,
	GUL_AEP float default 1,
	IL_OEP float default 1,
	IL_AEP float default 1
	)

insert into #ep (ReturnPeriod) values (5)
insert into #ep (ReturnPeriod) values (20)
insert into #ep (ReturnPeriod) values (50)
insert into #ep (ReturnPeriod) values (200)
insert into #ep (ReturnPeriod) values (500)
insert into #ep (ReturnPeriod) values (1000)
insert into #ep (ReturnPeriod) values (5000)
insert into #ep (ReturnPeriod) values (10000)

update #ep set GUL_OEP = 0, GUL_AEP = 0, IL_OEP = 0, IL_AEP = 0

create table #output (Summary_id int, return_period float, Loss float)

--gul oep
truncate table #output
set @FileName = null
select @FileName = [FileName], @LocationName = LocationName from #Files where FileDesc = 'Output Portfolio Level GUL LEC Full Uncertainty OEP'

Set @SQL = '
Select	Summary_id, return_period, Loss
From	OpenRowset(''MSDASQL'',''Driver={Microsoft Access Text Driver (*.txt, *.csv)};
          DefaultDir=' + @LocationName + ';Extended Properties="text;HDR=YES;FMT=Delimited'',
          ''select * from ' + @FileName + ''')'

insert into #output
exec sp_executesql @SQL

update #EP set GUL_OEP = Loss from #output where #EP.ReturnPeriod = #output.return_period


--gul aep
truncate table #output
set @FileName = null
select @FileName = [FileName], @LocationName = LocationName from #Files where FileDesc = 'Output Portfolio Level GUL LEC Full Uncertainty AEP'

Set @SQL = '
Select	Summary_id, return_period, Loss
From	OpenRowset(''MSDASQL'',''Driver={Microsoft Access Text Driver (*.txt, *.csv)};
          DefaultDir=' + @LocationName + ';Extended Properties="text;HDR=YES;FMT=Delimited'',
          ''select * from ' + @FileName + ''')'

insert into #output
exec sp_executesql @SQL

update #EP set GUL_AEP = Loss from #output where #EP.ReturnPeriod = #output.return_period

--il oep
truncate table #output
set @FileName = null
select @FileName = [FileName], @LocationName = LocationName from #Files where FileDesc = 'Output Portfolio Level IL LEC Full Uncertainty OEP'

Set @SQL = '
Select	Summary_id, return_period, Loss
From	OpenRowset(''MSDASQL'',''Driver={Microsoft Access Text Driver (*.txt, *.csv)};
          DefaultDir=' + @LocationName + ';Extended Properties="text;HDR=YES;FMT=Delimited'',
          ''select * from ' + @FileName + ''')'

insert into #output
exec sp_executesql @SQL

update #EP set IL_OEP = Loss from #output where #EP.ReturnPeriod = #output.return_period

--il aep
truncate table #output
set @FileName = null
select @FileName = [FileName], @LocationName = LocationName from #Files where FileDesc = 'Output Portfolio Level IL LEC Full Uncertainty AEP'

Set @SQL = '
Select	Summary_id, return_period, Loss
From	OpenRowset(''MSDASQL'',''Driver={Microsoft Access Text Driver (*.txt, *.csv)};
          DefaultDir=' + @LocationName + ';Extended Properties="text;HDR=YES;FMT=Delimited'',
          ''select * from ' + @FileName + ''')'

insert into #output
exec sp_executesql @SQL

update #EP set IL_AEP = Loss from #output where #EP.ReturnPeriod = #output.return_period


select * from #ep order by ReturnPeriod

drop table #ep
drop table #Files
drop table #output

GO

/****** Object:  StoredProcedure [dbo].[getParametersJSONGeneral]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[getParametersJSONGeneral] (@ProcessRunId int)
AS 
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

Create Table #General ([Name] nvarchar(255) NULL, [Value] nvarchar(255) NULL, [Type] nvarchar(255) NULL)

Insert Into #General
Select	PR.ParameterRunName  AS [Name],
		PR.ParameterRunValue AS [Value],
		PR.ParameterRunFormat AS [Type]
From	ElementRun AS ER
Join	ParameterRun AS PR on ER.ElementRunID = PR.ElementRunID
Join	WorkflowParameter AS WP on PR.WorkflowParameterID = WP.WorkflowParameterID
Join	Parameter AS P on WP.ParameterID = P.ParameterID
Where	ER.ProcessRunID = @ProcessRunID
And		P.JSONLevel = 2

Insert Into #General (Name, Value, Type)
Select 'source_tag', tag, 'str' 
From dbo.Version

Insert Into #General (Name, Value, Type) Values
('analysis_tag', @ProcessRunId, 'str')

Create Table #Outputs (SummarySetID int NULL, [Output] nvarchar(255) NULL, [Flag] nvarchar(255) NULL, LECFlag int NULL)

Insert Into #Outputs exec getParametersJSONSummariesGUL @ProcessRunId
If (Select Count(*) From #Outputs) > 0
Begin
	Insert Into #General Values ('gul_output','1','bool')
End

Truncate Table #Outputs

Insert Into #Outputs exec getParametersJSONSummariesIL @ProcessRunId
If (Select Count(*) From #Outputs) > 0
Begin
	Insert Into #General Values ('il_output','1','bool')
End

Select * From #General

Drop Table #General
Drop Table #Outputs






GO
/****** Object:  StoredProcedure [dbo].[getParametersJSONModel]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[getParametersJSONModel] (@ProcessRunId int)
AS 
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

Select	PR.ParameterRunName AS [Name],
		PR.ParameterRunValue AS [Value],
		PR.ParameterRunFormat AS [Type]
From	ElementRun AS ER
Join	ParameterRun AS PR on ER.ElementRunID = PR.ElementRunID
Join	WorkflowParameter AS WP on PR.WorkflowParameterID = WP.WorkflowParameterID
Join	Parameter AS P on WP.ParameterID = P.ParameterID
Where	ER.ProcessRunID = @ProcessRunID
And		P.JSONLevel = 3
And		PR.ParameterRunValue is not null --temp fix, to be removed






GO
/****** Object:  StoredProcedure [dbo].[getParametersJSONSummariesGUL]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[getParametersJSONSummariesGUL] (@ProcessRunId int)
AS 
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------


create table #output (SummarySetID int, [Output] nvarchar(255), [Flag] nvarchar(255), LECFlag int)

insert into #output
Select	O.OutputId AS SummarySetID,
		lower(AT.AnalysisFileNameStub) AS [Output],
		'1' as [Flag],
		AT.LECFlag
From	ElementRun AS ER
Join	OutputRun AS O on ER.ElementRunID = O.ElementRunID
Join	OutputType AS OT on OT.OutputTypeID = O.OutputTypeID
Join	Perspective AS P on OT.PerspectiveID = P.PerspectiveID
Join	AnalysisType AS AT on OT.AnalysisTypeID = AT.AnalysisTypeID
Where	ER.ProcessRunID = @ProcessRunID
And		P.PerspectiveID = 1 --GUL
Order By
		P.PerspectiveID,
		O.OutputID,
		JSONLevel

insert into #output
select	distinct SummarySetID,
		'lec_output' AS [Output],
		1 as [Flag],
		0 as LECFlag
from	#output
where	lecflag = 1

insert into #output
select	distinct SummarySetID,
		'return_period_file' AS [Output],
		1 as [Flag],
		1 as LECFlag
from	#output
where	lecflag = 1


select * from #output

drop table #output





GO
/****** Object:  StoredProcedure [dbo].[getParametersJSONSummariesIL]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[getParametersJSONSummariesIL] (@ProcessRunId int)
AS 
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------


create table #output (SummarySetID int, [Output] nvarchar(255), [Flag] nvarchar(255), LECFlag int)

insert into #output
Select	O.OutputId AS SummarySetID,
		lower(AT.AnalysisFileNameStub) AS [Output],
		'1' as [Flag],
		AT.LECFlag
From	ElementRun AS ER
Join	OutputRun AS O on ER.ElementRunID = O.ElementRunID
Join	OutputType AS OT on OT.OutputTypeID = O.OutputTypeID
Join	Perspective AS P on OT.PerspectiveID = P.PerspectiveID
Join	AnalysisType AS AT on OT.AnalysisTypeID = AT.AnalysisTypeID
Where	ER.ProcessRunID = @ProcessRunID
And		P.PerspectiveID = 2 --IL
Order By
		P.PerspectiveID,
		O.OutputID,
		JSONLevel

insert into #output
select	distinct SummarySetID,
		'lec_output' AS [Output],
		1 as [Flag],
		0 as LECFlag
from	#output
where	lecflag = 1
		
insert into #output
select	distinct SummarySetID,
		'return_period_file' AS [Output],
		1 as [Flag],
		1 as LECFlag
from	#output
where	lecflag = 1

select * from #output

drop table #output

GO

/****** Object:  StoredProcedure [dbo].[getProcessData]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[getProcessData] @userid	int, @modelid int, @progid int, @wfid int
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

select	po.ProgOasisId,
		p.ProgName,
		m.ModelName
from	progoasis as po
join	prog as p on po.ProgId = p.ProgID
join	model as m on po.ModelId = m.ModelID
where	po.[Status] = 'Loaded'
and		p.deleted = 0
order by 		
		ProgOasisId desc

GO
/****** Object:  StoredProcedure [dbo].[getProcessRunDetails]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [dbo].[getProcessRunDetails] 
	@processrunid			int
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

	create table #temp
	(
		ProcessID				int,
		Run						nvarchar(255) null,
		ElementId				int null,
		Task					nvarchar(255) null,
		Status					nvarchar(255) null,
		Description				nvarchar(255) null,
		CompletedAt				datetime
	)
BEGIN
    -- Insert statements for procedure here
	insert into #temp 
	select	l.ElementRunID as 'ProcessID', 
			null,
			l.ElementRunID,
			null,
			l.LogStatus,
			l.LogDescription,
			l.LogTimestamp
	from ElementRunLog l  
	where l.ElementRunID in (select e.ElementRunID from ElementRun e where e.ProcessRunID = @processrunid) 
						 order by l.LogTimestamp desc, l.ElementRunID desc

	--select * from #temp
	--select count(*) from #temp

	update #temp
	set Task = er.ElementRunName
	from ElementRun er
	where ElementId = er.ElementRunID

	update #temp
	set Run = pr.ProcessRunName
	from ProcessRun pr
	where pr.ProcessRunID = #temp.ProcessID


	--select * from #temp


	select	ElementID as 'Element ID',
			--Run,
			Task as 'Element Name' ,
			Status,
			Description,
			CompletedAt
	from #temp
	order by ElementId desc

	drop table #temp
END






GO

/****** Object:  StoredProcedure [dbo].[getProcessRunParams]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
-- =============================================
-- Author:		<Author,,Name>
-- Create date: <Create Date,,>
-- Description:	<Description,,>
-- =============================================
CREATE PROCEDURE [dbo].[getProcessRunParams]
	-- Add the parameters for the stored procedure here
	@processrunid int 
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.

    -- Insert statements for procedure here
	SELECT ParameterRunName, ParameterRunValue, ParameterRunFormat into #temp
	FROM ParameterRun
	where ElementRunID in (select ElementRunID from ElementRun where ProcessRunID = @processrunid)
	and ParameterRunName in ('number_of_samples', 'gul_threshold', 'event_set' , 'event_occurrence_id',
	'peril_wind' , 'peril_surge', 'peril_quake', 'peril_flood', 'demand_surge', 'leakage_factor')


	update #temp
	set ParameterRunValue = case when ParameterRunValue = 1 then 'TRUE'
	else case when ParameterRunValue = 0 then 'FALSE' end end
	where ParameterRunFormat = 'bool'

	update #temp
	set ParameterRunValue = ''
	where ParameterRunValue is Null


	select ParameterRunName, ParameterRunValue from #temp
	drop table #temp
END




GO
/****** Object:  StoredProcedure [dbo].[getProfileDetailsForIni]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO



CREATE procedure [dbo].[getProfileDetailsForIni] @progid int
AS

SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = '@ProgId = ' + convert(nvarchar,@progid)
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

--declare @progid int = 1

declare @transformid int = (select transformid from prog where ProgID = @progid)
declare @transformLocResourceid int = (select resourceid from [resource] where resourcetable = 'Transform' and ResourceKey = @transformid and resourcetypeid = 118)
declare @transformAccResourceid int = (select resourceid from [resource] where resourcetable = 'Transform' and ResourceKey = @transformid and resourcetypeid = 119)
declare @locfileresourceid int = (select resourceid from [resource] where resourcetable = 'Prog' and ResourceKey = @progid and resourcetypeid = 103)
declare @accfileresourceid int = (select resourceid from [resource] where resourcetable = 'Prog' and ResourceKey = @progid and resourcetypeid = 104)
declare @locfileid int = (select fileid from fileresource where resourceid = @locfileresourceid)
declare @accfileid int = (select fileid from fileresource where resourceid = @accfileresourceid)
declare @locfilename nvarchar(255) = (select [filename] from [file] where fileid = @locfileid)
declare @accfilename nvarchar(255) = (select [filename] from [file] where fileid = @accfileid)
declare @locprofileid int = (select profileid from ProfileResource where resourceid = @transformLocResourceid)
declare @accprofileid int = (select profileid from ProfileResource where resourceid = @transformAccResourceid)


create table #return (returnid int identity(1,1), returnstring nvarchar(2500))

--declare @returnstring nvarchar(2500)


CREATE TABLE #elements (elementid int identity(1,1), ProfileElementName nvarchar(255), FormatName nvarchar(255), FormatNameIni nvarchar(255))


--loc profile
--set		@returnstring = '[' + @locfilename + '], ColNameHeader = True, Format = CSVDelimited, CharacterSet = ANSI'
insert into #return select '[' + @locfilename + ']'
insert into #return select 'ColNameHeader = True'
insert into #return select 'Format = CSVDelimited'
insert into #return select 'CharacterSet = ANSI'

Insert into #elements
select	ProfileElementName,
		FormatName,
		case
			when FormatName = 'int' then 'Integer'
			when FormatName = 'float' then 'Float'
			when FormatName = 'nvarchar(255)' then 'Char width 255'
			when FormatName = 'datetime' then 'Date'
			end as FormatNameIni
from	profileelement as pe
join	field as f on f.FieldID = pe.FieldID
join	[format] as fo on f.FormatID = fo.FormatID
where	profileid = @locprofileid
order by pe.profileelementid

declare @int int = 1
declare @pes int = (select max(elementid) from #elements)
declare @elementini nvarchar(255)

while @int <= @pes
begin
	select	@elementini = 'Col' + convert(nvarchar,@int) + '=' + ProfileElementName + ' ' + FormatNameIni from #elements where elementid = @int
	--set		@returnstring = @returnstring + @elementini
	insert into #return select @elementini
	set		@int = @int + 1
end

--insert into #return
--select @returnstring

insert into #return select ''

--acc profile
--set		@returnstring = '[' + @accfilename + '], ColNameHeader = True, Format = CSVDelimited, CharacterSet = ANSI'

insert into #return select '[' + @accfilename + ']'
insert into #return select 'ColNameHeader = True'
insert into #return select 'Format = CSVDelimited'
insert into #return select 'CharacterSet = ANSI'

truncate table #elements
Insert into #elements
select	ProfileElementName,
		FormatName,
		case
			when FormatName = 'int' then 'Integer'
			when FormatName = 'float' then 'Float'
			when FormatName = 'nvarchar(255)' then 'Char width 255'
			when FormatName = 'datetime' then 'Date'
			end as FormatNameIni
from	profileelement as pe
join	field as f on f.FieldID = pe.FieldID
join	[format] as fo on f.FormatID = fo.FormatID
where	profileid = @accprofileid
order by pe.profileelementid

set @int = 1
set @pes = (select max(elementid) from #elements)

while @int <= @pes
begin
	select	@elementini = 'Col' + convert(nvarchar,@int) + '=' + ProfileElementName + ' ' + FormatNameIni from #elements where elementid = @int
	--set		@returnstring = @returnstring + @elementini
	insert into #return select @elementini
	set		@int = @int + 1
end

--insert into #return
--select @returnstring



select returnstring from #return order by returnid

drop table #elements
drop table #return



GO

/****** Object:  StoredProcedure [dbo].[getProgData]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE procedure [dbo].[getProgData]
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

SELECT	P.[ProgID] as "Programme ID",
		P.[ProgName] as "Programme Name",
		A.AccountID as "Account ID",
		A.AccountName as "Account Name",
		T.TransformID as [Transform ID],
		T.TransformName as [Transform],
		P.[Status]
FROM	Prog  AS P
JOIN	Account AS A ON A.AccountID = P.AccountID
JOIN	Transform AS T on P.TransformID = T.TransformID
Where	ISNULL(P.Deleted,1) = 0
order by 
		P.ProgID desc







GO
/****** Object:  StoredProcedure [dbo].[getProgFileDetails]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [dbo].[getProgFileDetails] @ProgId int
AS

SET ANSI_NULLS ON;
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

Create Table #Staging ([ObjectId] int, [Object] nvarchar(255), [FileId] int NULL, [Status] nvarchar(255), Detail nvarchar(255) NULL)

Insert Into #Staging Values (1,'Source Location File', NULL,  'Not Loaded', NULL)
Insert Into #Staging Values (2,'Source Account File',NULL,  'Not Loaded', NULL)
Insert Into #Staging Values (3,'Canonical Location File',NULL,  'Not Loaded', NULL)
Insert Into #Staging Values (4,'Canonical Account File',NULL,  'Not Loaded', NULL)
Insert Into #Staging Values (5,'Canonical Model', NULL, 'Not Loaded', NULL)

Update #Staging Set [FileId] = A.[FileId], Detail = A.[FileName] From (
	Select F.[FileName], F.FileID From [Resource] AS R Join	FileResource AS FR on FR.ResourceID = R.ResourceID Join [File] AS F on FR.FileID = F.FileID Where R.ResourceTable = 'Prog' And R.ResourceKey = @ProgId And R.ResourceTypeID = 101) AS A --SourceLocationFile
Where	#Staging.[ObjectId] = 1

Update #Staging Set [FileId] = A.[FileId], Detail = A.[FileName] From (
	Select F.[FileName], F.FileID  From [Resource] AS R Join	FileResource AS FR on FR.ResourceID = R.ResourceID Join [File] AS F on FR.FileID = F.FileID Where R.ResourceTable = 'Prog' And R.ResourceKey = @ProgId And R.ResourceTypeID = 102) AS A --
Where	#Staging.[ObjectId] = 2

Update #Staging Set [FileId] = A.[FileId], Detail = A.[FileName] From (
	Select F.[FileName], F.FileID  From [Resource] AS R Join	FileResource AS FR on FR.ResourceID = R.ResourceID Join [File] AS F on FR.FileID = F.FileID Where R.ResourceTable = 'Prog' And R.ResourceKey = @ProgId And R.ResourceTypeID = 103) AS A --
Where	#Staging.[ObjectId] = 3

Update #Staging Set [FileId] = A.[FileId], Detail = A.[FileName] From (
	Select F.[FileName], F.FileID  From [Resource] AS R Join	FileResource AS FR on FR.ResourceID = R.ResourceID Join [File] AS F on FR.FileID = F.FileID Where R.ResourceTable = 'Prog' And R.ResourceKey = @ProgId And R.ResourceTypeID = 104) AS A --
Where	#Staging.[ObjectId] = 4

Update #Staging Set Detail = 'InterestGroupId: ' + convert(nvarchar, InterestGroupId) From (
	Select InterestGroupId From InterestGroup Where ProgID = @ProgId) AS A --SourceLocationFile
Where	#Staging.[ObjectId] = 5


Update #Staging Set [Status] = 'Loaded' Where Detail IS NOT NULL

Select [Object], [FileId], [Status], Detail From #Staging

Drop Table #Staging

GO

/****** Object:  StoredProcedure [dbo].[getProgOasisDetails]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[getProgOasisDetails] @ProgOasisId int
AS

SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

Declare @ProgId int  = (Select ProgId From ProgOasis Where ProgOasisId = @ProgOasisId)
Declare @ModelId int = (Select ModelId From ProgOasis Where ProgOasisId = @ProgOasisId)
Declare @FileId int  = (Select FileId From [FileResource] F Join [Resource] R on F.ResourceId = R.ResourceId Where R.ResourceTable = 'ProgOasis' And R.ResourceKey = @ProgOasisId And R.ResourceTypeID = 105) --105 = model lookup file 

--update status
update progoasis set [status] = 'Running Lookup' where progoasisid = @ProgOasisId

Select @ProgId AS ProgId, @ModelId AS ModelId, @FileId AS FileId








GO
/****** Object:  StoredProcedure [dbo].[getProgOasisFileDetails]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [dbo].[getProgOasisFileDetails] @ProgOasisId int
AS

SET ANSI_NULLS ON;
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

Declare @ProgID int = (Select ProgID From ProgOasis Where ProgOasisId = @ProgOasisId)

Create Table #Staging ([ObjectId] int, [Object] nvarchar(255), [FileId] int NULL, [Status] nvarchar(255), Detail nvarchar(255) NULL)

Insert Into #Staging Values (1,'Source Location File', NULL,  'Not Loaded', NULL)
Insert Into #Staging Values (2,'Source Account File',NULL,  'Not Loaded', NULL)
Insert Into #Staging Values (3,'Canonical Location File',NULL,  'Not Loaded', NULL)
Insert Into #Staging Values (4,'Canonical Account File',NULL,  'Not Loaded', NULL)
Insert Into #Staging Values (5,'Canonical Model', NULL, 'Not Loaded', NULL)

Insert Into #Staging Values (6,'Model Format Location Lookup File', NULL,  'Not Loaded', NULL)
Insert Into #Staging Values (7,'Lookup Service Return File',NULL,  'Not Loaded', NULL)
Insert Into #Staging Values (8,'Lookup Service Return Error File',NULL,  'Not Loaded', NULL)
Insert Into #Staging Values (9,'Oasis Items File',NULL,  'Not Loaded', NULL)
Insert Into #Staging Values (10,'Oasis Coverages File', NULL, 'Not Loaded', NULL)
Insert Into #Staging Values (11,'Oasis Item Dictionary File', NULL, 'Not Loaded', NULL)
Insert Into #Staging Values (12,'Oasis FM Programme File', NULL, 'Not Loaded', NULL)
Insert Into #Staging Values (13,'Oasis FM Policy TC File', NULL, 'Not Loaded', NULL)
Insert Into #Staging Values (14,'Oasis FM Profile File', NULL, 'Not Loaded', NULL)
Insert Into #Staging Values (15,'Oasis FM XRef File', NULL, 'Not Loaded', NULL)
Insert Into #Staging Values (16,'Oasis FM Dict File', NULL, 'Not Loaded', NULL)

Update #Staging Set [FileId] = A.[FileId], Detail = A.[FileName] From (
	Select F.[FileName], F.FileID From [Resource] AS R Join	FileResource AS FR on FR.ResourceID = R.ResourceID Join [File] AS F on FR.FileID = F.FileID Where R.ResourceTable = 'Prog' And R.ResourceKey = @ProgId And R.ResourceTypeID = 101) AS A --SourceLocationFile
Where	#Staging.[ObjectId] = 1

Update #Staging Set [FileId] = A.[FileId], Detail = A.[FileName] From (
	Select F.[FileName], F.FileID  From [Resource] AS R Join	FileResource AS FR on FR.ResourceID = R.ResourceID Join [File] AS F on FR.FileID = F.FileID Where R.ResourceTable = 'Prog' And R.ResourceKey = @ProgId And R.ResourceTypeID = 102) AS A --
Where	#Staging.[ObjectId] = 2

Update #Staging Set [FileId] = A.[FileId], Detail = A.[FileName] From (
	Select F.[FileName], F.FileID  From [Resource] AS R Join	FileResource AS FR on FR.ResourceID = R.ResourceID Join [File] AS F on FR.FileID = F.FileID Where R.ResourceTable = 'Prog' And R.ResourceKey = @ProgId And R.ResourceTypeID = 103) AS A --
Where	#Staging.[ObjectId] = 3

Update #Staging Set [FileId] = A.[FileId], Detail = A.[FileName] From (
	Select F.[FileName], F.FileID  From [Resource] AS R Join	FileResource AS FR on FR.ResourceID = R.ResourceID Join [File] AS F on FR.FileID = F.FileID Where R.ResourceTable = 'Prog' And R.ResourceKey = @ProgId And R.ResourceTypeID = 104) AS A --
Where	#Staging.[ObjectId] = 4

Update #Staging Set Detail = 'InterestGroupId: ' + convert(nvarchar, InterestGroupId) From (
	Select InterestGroupId From InterestGroup Where ProgID = @ProgId) AS A --SourceLocationFile
Where	#Staging.[ObjectId] = 5

Update #Staging Set [FileId] = A.[FileId], Detail = A.[FileName] From (
	Select F.[FileName], F.FileID From [Resource] AS R Join [FileResource] AS FR on R.ResourceID = FR.ResourceID Join [File] AS F on F.FileID = FR.FileID Where R.ResourceTable = 'ProgOasis' And R.ResourceKey = @ProgOasisId And R.ResourceTypeID = 105) AS A --SourceLocationFile
Where	#Staging.[ObjectId] = 6

Update #Staging Set [FileId] = A.[FileId], Detail = A.[FileName] From (
	Select F.[FileName], F.FileID From [Resource] AS R Join [FileResource] AS FR on R.ResourceID = FR.ResourceID Join [File] AS F on F.FileID = FR.FileID Where R.ResourceTable = 'ProgOasis' And R.ResourceKey = @ProgOasisId And R.ResourceTypeID = 106) AS A --SourceLocationFile
Where	#Staging.[ObjectId] = 7

Update #Staging Set [FileId] = A.[FileId], Detail = A.[FileName] From (
	Select F.[FileName], F.FileID From [Resource] AS R Join [FileResource] AS FR on R.ResourceID = FR.ResourceID Join [File] AS F on F.FileID = FR.FileID Where R.ResourceTable = 'ProgOasis' And R.ResourceKey = @ProgOasisId And R.ResourceTypeID = 108) AS A --SourceLocationFile
Where	#Staging.[ObjectId] = 8

Update #Staging Set [FileId] = A.[FileId], Detail = A.[FileName] From (
	Select F.[FileName], F.FileID From [Resource] AS R Join [FileResource] AS FR on R.ResourceID = FR.ResourceID Join [File] AS F on F.FileID = FR.FileID Where R.ResourceTable = 'ProgOasis' And R.ResourceKey = @ProgOasisId And R.ResourceTypeID = 111) AS A --Oasis Items File
Where	#Staging.[ObjectId] = 9

Update #Staging Set [FileId] = A.[FileId], Detail = A.[FileName] From (
	Select F.[FileName], F.FileID From [Resource] AS R Join [FileResource] AS FR on R.ResourceID = FR.ResourceID Join [File] AS F on F.FileID = FR.FileID Where R.ResourceTable = 'ProgOasis' And R.ResourceKey = @ProgOasisId And R.ResourceTypeID = 112) AS A --Oasis Coverages File
Where	#Staging.[ObjectId] = 10

Update #Staging Set [FileId] = A.[FileId], Detail = A.[FileName] From (
	Select F.[FileName], F.FileID From [Resource] AS R Join [FileResource] AS FR on R.ResourceID = FR.ResourceID Join [File] AS F on F.FileID = FR.FileID Where R.ResourceTable = 'ProgOasis' And R.ResourceKey = @ProgOasisId And R.ResourceTypeID = 113) AS A --Oasis Item Dictionary File
Where	#Staging.[ObjectId] = 11

Update #Staging Set [FileId] = A.[FileId], Detail = A.[FileName] From (
	Select F.[FileName], F.FileID From [Resource] AS R Join [FileResource] AS FR on R.ResourceID = FR.ResourceID Join [File] AS F on F.FileID = FR.FileID Where R.ResourceTable = 'ProgOasis' And R.ResourceKey = @ProgOasisId And R.ResourceTypeID = 114) AS A --Oasis FM Programme File
Where	#Staging.[ObjectId] = 12

Update #Staging Set [FileId] = A.[FileId], Detail = A.[FileName] From (
	Select F.[FileName], F.FileID From [Resource] AS R Join [FileResource] AS FR on R.ResourceID = FR.ResourceID Join [File] AS F on F.FileID = FR.FileID Where R.ResourceTable = 'ProgOasis' And R.ResourceKey = @ProgOasisId And R.ResourceTypeID = 115) AS A --Oasis FM Policy TC File
Where	#Staging.[ObjectId] = 13

Update #Staging Set [FileId] = A.[FileId], Detail = A.[FileName] From (
	Select F.[FileName], F.FileID From [Resource] AS R Join [FileResource] AS FR on R.ResourceID = FR.ResourceID Join [File] AS F on F.FileID = FR.FileID Where R.ResourceTable = 'ProgOasis' And R.ResourceKey = @ProgOasisId And R.ResourceTypeID = 116) AS A --Oasis FM Profile File
Where	#Staging.[ObjectId] = 14

Update #Staging Set [FileId] = A.[FileId], Detail = A.[FileName] From (
	Select F.[FileName], F.FileID From [Resource] AS R Join [FileResource] AS FR on R.ResourceID = FR.ResourceID Join [File] AS F on F.FileID = FR.FileID Where R.ResourceTable = 'ProgOasis' And R.ResourceKey = @ProgOasisId And R.ResourceTypeID = 117) AS A --Oasis FM XRef File
Where	#Staging.[ObjectId] = 15

Update #Staging Set [FileId] = A.[FileId], Detail = A.[FileName] From (
	Select F.[FileName], F.FileID From [Resource] AS R Join [FileResource] AS FR on R.ResourceID = FR.ResourceID Join [File] AS F on F.FileID = FR.FileID Where R.ResourceTable = 'ProgOasis' And R.ResourceKey = @ProgOasisId And R.ResourceTypeID = 127) AS A --Oasis FM Dict File
Where	#Staging.[ObjectId] = 16

Update #Staging Set [Status] = 'Loaded' Where Detail IS NOT NULL

Select [Object], [FileId], [Status], Detail From #Staging

Drop Table #Staging







GO
/****** Object:  StoredProcedure [dbo].[getProgOasisForProg]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE procedure [dbo].[getProgOasisForProg] @ProgID int = NULL
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

SELECT	[ProgOasisId],
      	Prog.ProgName,
      	Model.ModelName,
		Transform.TransformName,
      	[SourceFileId],
      	[OasisKeyName] AS 'FileID',
		ProgOasis.[Status],
      	[API1aDateTime],
      	[API1bDateTime],
      	[API1cDateTime],
      	[SessionId]
FROM	ProgOasis
JOIN	Prog ON Prog.ProgID=ProgOasis.ProgId
JOIN	Model ON Model.ModelID=ProgOasis.ModelId
JOIN    Transform ON Transform.TransformID=ProgOasis.TransformID
WHERE	ProgOasis.ProgId = (ISNULL(@ProgID,ProgOasis.ProgId))
order by 
		[ProgOasisId] desc

GO

/****** Object:  StoredProcedure [dbo].[getResourceModeUser]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE procedure [dbo].[getResourceModeUser] @BFEUserID int,  @ResourceID int
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

SELECT	SGR.Mode
FROM	SecurityGroupResource AS SGR
Join	SecurityGroup AS SG on SGR.SecurityGroupID = SG.SecurityGroupID
Join	UserSecurityGroup AS USG on SG.SecurityGroupID = USG.[Security Group ID]
Join	BFEUser AS B on USG.BFEUserID = B.BFEUserID
WHERE	@ResourceID = SGR.ResourceID
AND		@BFEUserID = B.BFEUserID



GO
/****** Object:  StoredProcedure [dbo].[getResourceType]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE procedure [dbo].[getResourceType]
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

SELECT	[ResourceTypeID],
		[ResourceTypeName],
		[Source]
FROM	[dbo].[ResourceType]








GO

/****** Object:  StoredProcedure [dbo].[getRuntimeParamList]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[getRuntimeParamList] @ProgOasisid int 
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------
BEGIN
	SELECT ModelResourceName, ModelResourceValue from ModelResource where ModelID = (select ModelID from ProgOasis where ProgOasisID = @ProgOasisid) and ResourceTypeID = 1001
END








GO

/****** Object:  StoredProcedure [dbo].[getSecurityGroups]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE procedure [dbo].[getSecurityGroups]
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

SELECT	SecurityGroupID AS [Security Group ID],
		SecurotyGroupName AS [Security Group Name]
FROM	SecurityGroup
Order By
		SecurotyGroupName








GO
/****** Object:  StoredProcedure [dbo].[getSecurityGroupsForUser]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE Procedure [dbo].[getSecurityGroupsForUser] @BFEUserID int
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

Select	SG.SecurityGroupID as "Security Group ID",
		SG.SecurotyGroupName as "Security Group Name"
From	SecurityGroup AS SG
Join	UserSecurityGroup AS USG on SG.SecurityGroupID = USG.[Security Group ID]
Join	BFEUser AS U on USG.BFEUserID = U.BFEUserID
Where	U.BFEUserID = ISNULL(@BFEUserId,U.BFEUserID)
Order By
		SG.SecurotyGroupName








GO

/****** Object:  StoredProcedure [dbo].[getTransformNameCanModel]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[getTransformNameCanModel]
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

SELECT	TransformName,
		TransformID
FROM	[Transform]
WHERE	TransformTypeId = 2
ORDER BY
		TransformName








GO
/****** Object:  StoredProcedure [dbo].[getTransformNameSourceCan]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[getTransformNameSourceCan]
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

SELECT	TransformName,
		TransformID
FROM	[Transform]
WHERE	TransformTypeId = 1
ORDER BY
		TransformName








GO

/****** Object:  StoredProcedure [dbo].[getUserDepartment]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE procedure [dbo].[getUserDepartment] @BFEUserID int
as
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

select	BFEUserLogin,
		BFEUserPassword,
		BFEUserDept
from	BFEUser
Where	BFEUserID = ISNULL(@BFEUserId,0)








GO

/****** Object:  StoredProcedure [dbo].[getUserLicenses]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE procedure [dbo].[getUserLicenses] @BFEUserID int
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

Select	UL.OasisUserID,
		OU.OasisUserName,
		OU.OasisSystemID,
		OU.SystemLogin,
		OU.SystemPassword,
		OU.ModelLicenseID,
		ML.ModelLicenseName,
		ML.LicenseStartDate,
		ML.LicenseEndDate,
		ML.LicenseType,
		ML.LicenseContractRef
From	BFEUser AS BU
Join	UserLicense AS UL on bu.BFEUserID = ul.BFEUserID
Join	OasisUser AS OU on ou.OasisUserID = ul.OasisUserID
Join	ModelLicense AS ML on ml.ModelLicenseID = ou.ModelLicenseID
Where	BU.BFEUserID = isnull(@BFEUserID,0)








GO

/****** Object:  StoredProcedure [dbo].[getUserProcessDetails]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[getUserProcessDetails] @userid int
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------
BEGIN
create table #temp
	(
		ProgOasisID		int NULL,
		RunID			int NULL,
		Run				nvarchar(255) NULL,
		Model			nvarchar(255) NULL,
		Status			nvarchar(255) NULL,
		Completed		datetime NULL
	)

	Create Table #ProgOasisList
		(
		ProgOasisID int,
		ProgName nvarchar(255),
		ModelName nvarchar(255)
		)

	Create Table #ProcessRunList
		(
		[ProcessRunID] int,
		[ProcessRunName] nvarchar(255),
		[ProcessRunStatus] nvarchar(255) null,
		[ProgOasisID] int
		)

	INSERT INTO #ProgOasisList
	SELECT	dbo.ProgOasis.ProgOasisID, 
			dbo.Prog.ProgName, 
			dbo.Model.ModelName
	FROM    dbo.ProgOasis 
	JOIN	dbo.Prog ON dbo.ProgOasis.ProgID = dbo.Prog.ProgID
	JOIN	dbo.Model ON dbo.ProgOasis.ModelID = dbo.Model.ModelID


	--select * from #ProgOasisList

	INSERT INTO #ProcessRunList   
	SELECT	[ProcessRun].[ProcessRunID], 
			[ProcessRun].[ProcessRunName], 
			[ProcessRun].[ProcessRunStatus], 
			[ProcessRun].[ProgOasisID]
	FROM	[ProcessRun] 
	JOIN	[ProgOasis] ON [ProgOasis].[ProgOasisID] = [ProcessRun].[ProgOasisID]
  
	--select * from #ProcessRunList

	insert into #temp (ProgOasisID, RunID, Run, [Status])
	select	pr.ProgOasisID, 
			pr.ProcessRunID, 
			pr.ProcessRunName, 
			pr.ProcessRunStatus 
	from	dbo.ProcessRun pr 

	--select * from #temp

	update	#temp 
	set		Model = pl.ModelName 
	from	#ProgOasisList pl
	join	#temp t on t.ProgOasisID = pl.ProgOasisID
				
	--select * from #temp		

	update #temp set 
	Completed = (select max(e.LogTimestamp) from dbo.ElementRunLog e where 
				e.ElementRunID = (select max(ER.ElementRunID) from ElementRun AS ER join ElementRunLog AS L on ER.ElementRunID = L.ElementRunID where ProcessRunID = RunID))

	--select * from #temp

	select
		ProgOasisID,
		RunID,
		Run as 'Run Name',
		Model,
		Status,
		convert(varchar, Completed) as 'Completed At'
	from #temp
	order by RunID desc
	--where UserID = @userid

	drop table #temp
	drop table #ProcessRunList
	drop table #ProgOasisList

END







GO
/****** Object:  StoredProcedure [dbo].[getUsersForCompany]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE procedure [dbo].[getUsersForCompany] @CompanyId int = NULL
as
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------
select	C.CompanyID AS [Company ID],
		C.CompanyName AS [Company Name],
		U.BFEUserId AS [User ID],
		U.BFEUserName AS [User Name],
		U.BFEUserDept AS [User Department]
FROM	Company AS C
Join	BFEUser AS U on C.CompanyID = U.CompanyID
Where	C.CompanyID = isnull(@CompanyId,C.CompanyID) and U.Deleted = 0
Order By
		BFEUserID desc,
		C.CompanyName,
		U.BFEUserName








GO

/****** Object:  StoredProcedure [dbo].[linkOutputFileToProcessRun]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE Procedure [dbo].[linkOutputFileToProcessRun] @ProcessRunId int, @OutputFiles nvarchar(max)
AS

SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

----debug
--declare @processrunid int = 48
--declare @OutputFiles nvarchar(2500) = 'gul_2_eltcalc.csv,gul_eltcalc.csv'

declare @fileid int = (select isnull(max(fileid),0) from [file])
declare @resourceid int = (select isnull(max(resourceid),0) from [resource])
declare @fileresourceid int = (select isnull(max(fileresourceid),0) from [fileresource])
declare @locationid int = (select isnull(max(locationid),0) +1 from [location])
declare @sql nvarchar(max)

set @OutputFiles =  replace(@OutputFiles,',',''',''') 


insert into location
select	distinct @locationid,
		l.LocationName + '\output',
		l.LocationDesc + '_Outputs',
		l.LocationPathUnix + '/output'
from	[resource] as r
join	FileResource as fr on r.ResourceID = fr.ResourceID
join	[file] as f on fr.FileID = f.FileId
join	location as l on f.LocationID = l.LocationID
where	resourcetable = 'ProcessRun' and resourcekey = @processrunid


set @sql = '
select	lower(p.perspectivename) + ''_S'' + convert(nvarchar,[or].outputid) + ''_'' + 
		case when at.LECFlag = 1 then ''leccalc_'' else '''' end +
		at.AnalysisFileNameStub + ''.csv'' as [FileName],
		''Output '' + SummaryLevelName + '' Level '' + PerspectiveName + '' '' + AnalysisTypeName
from	processrun as pr
join	elementrun as er on er.ProcessRunID = pr.ProcessRunID
join	OutputRun as [or] on er.ElementRunID = [or].ElementRunID
join	OutputType as ot on [or].OutputTypeID = ot.OutputTypeID
join	AnalysisType as at on ot.AnalysisTypeID = at.AnalysisTypeID
join	perspective as p on ot.PerspectiveID = p.PerspectiveID
join	summarylevel as sl on ot.SummaryLevelID = sl.SummaryLevelID
where	pr.ProcessRunID = ' + convert(nvarchar,@processrunid) + '
and		lower(p.perspectivename) + ''_S'' + convert(nvarchar,[or].outputid) + ''_'' + 
		case when at.LECFlag = 1 then ''leccalc_'' else '''' end + at.AnalysisFileNameStub + ''.csv'' in (''' + @OutputFiles + ''')'


create table #files (RowId int identity(1,1), [FileName] nvarchar(255), FileDesc nvarchar(255))
insert into #files
exec sp_executesql @sql

insert into [file]		select RowId+@fileid,[FileName],FileDesc,1,1,@locationid,getdate(),getdate(),NULL,'Sys','Sys,',NULL,121 from #files
insert into [resource]	select RowId+@resourceid,'ProcessRun',@processrunid,NULL,128 from #files
insert into FileResource select RowId+@fileresourceid,RowId+@fileid,RowId+@resourceid from #files

drop table #files


select 'Done'




GO
/****** Object:  StoredProcedure [dbo].[listProcessRun]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [dbo].[listProcessRun] @ProgOasisID int, @Status nvarchar(25)
AS 
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

Select	*
From	ProcessRun
Where	ProgOasisID = @ProgOasisID
order by		
		processrunid desc






GO

/****** Object:  StoredProcedure [dbo].[logElementRun]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO



CREATE PROCEDURE [dbo].[logElementRun]
	@ElementRunID		int,
	@Status				varchar(20),
	@Description		varchar(200)
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------
BEGIN

	declare @logid			int
	if exists (select * from dbo.ElementRunLog rl where rl.ElementRunID = @ElementRunID)
	begin
		update dbo.ElementRunLog 
		set LogStatus = @Status,
		    LogDescription = @Description
		where ElementRunID = @ElementRunID and LogStatus != 'Started'
	end
	else
	begin
		select @logid = case when max(ElementRunLogID) is NULL then 1
						else max(ElementRunLogID) + 1 end 
				from ElementRunLog
		INSERT INTO dbo.ElementRunLog
			  (ElementRunLogID,
			   ElementRunID,
			   LogStatus,
			   LogDescription
			   )
		 VALUES
			   (@logid,
			    @ElementRunID,
				@Status,
				@Description
			   )
	end
END









GO

/****** Object:  StoredProcedure [dbo].[removeSecurityGroup]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE procedure [dbo].[removeSecurityGroup]
@BFEUserID int,
@SecurityGroupID int

AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------
if @SecurityGroupID = 0 
	DELETE FROM [dbo].[UserSecurityGroup]
	WHERE BFEUserID = @BFEUserID


else

	DELETE FROM [dbo].[UserSecurityGroup]
	WHERE BFEUserID = @BFEUserID
	 AND [Security Group ID]= @SecurityGroupID







GO

/****** Object:  StoredProcedure [dbo].[saveoutputoption]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
-- =============================================
-- Author:		<Author,,Name>
-- Create date: <Create Date,,>
-- Description:	<Description,,>
-- =============================================
CREATE PROCEDURE [dbo].[saveoutputoption] 
	-- Add the parameters for the stored procedure here
	@OutputOptionName nvarchar(255), 
	@OutputOptionsList nvarchar(2500)

AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	SET ANSI_NULLS ON;
	SET ANSI_WARNINGS ON;
	SET NOCOUNT ON;

    -- Insert statements for procedure here
	Set @OutputOptionsList = '''' + @OutputOptionsList + ''''
	Set @OutputOptionsList = REPLACE(@OutputOptionsList,',',''',''')
	Declare @SQL nvarchar(2500)

	--select @OutputOptionsList
	Create Table #outoptids (OutputTypeID int)
	Set @SQL = '
	select ot.OutputTypeID
	from	outputtype as ot
	join	summarylevel as sl on ot.SummaryLevelID = sl.SummaryLevelID
	join	AnalysisType as at on ot.AnalysisTypeID =at.AnalysisTypeID
	join	Perspective as p on ot.PerspectiveID = p.PerspectiveID
	where lower(perspectivename) + sl.parameterstub + at.ParameterStub in (' + @OutputOptionsList + ')'

	insert into #outoptids
	Exec sp_executesql @SQL

	--select * from #outoptids

	Declare @count int = 0
	declare @loopcnt int = (select count(*) from #outoptids)
	while @count < @loopcnt
	begin
		INSERT INTO dbo.OutputOptions (OutputOptionID,OutputOption,OutputTypeID)
		VALUES ((select max(OutputOptionID)+1 from OutputOptions), @OutputOptionName, (select top(1) OutputTypeID from #outoptids))
		--select * from OutputOptions
		delete top(1) from #outoptids
		set @count = @count + 1
	end

	drop table #outoptids

END




GO
/****** Object:  StoredProcedure [dbo].[tellOperationsValidOnFileID]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [dbo].[tellOperationsValidOnFileID]
	@FileID int
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------
begin

	if	(@FileID is null )
		or
		(@FileID not in
		(
			select FileId
			from "File"
		))
	begin
		declare @ErrMsg nvarchar(max) = 
			'Error: invalid FileID  ' + 
			coalesce(cast(@FileID as nvarchar(255)), 'null') + 
			' was given to OperationIDAppliesToFileID'
		;throw 50001, @ErrMsg, 1
	end

	declare @FileTypeID int = 
	(
		select FileTypeId
		from "file"
		where FileID = @FileID
	)

	declare @res nvarchar(4000) = ' File'

	;with ValidOpStringsOnThisFileType as
	(
		(
			select distinct operation
			from OperationFileType as oft 
				join OperationOperationID as ooid
					on oft.OperationID = ooid.OperationID
			where FileTypeID = @FileTypeID
		)
		union
		(
			-- viewing a file raw is always legal
			select operation
			from OperationOperationID
			where OperationID = 1000
		)
	)
	-- ghastly hack but easier than cursors and ok here as 
	-- we don't care about order, which isn't guaranteed here
	select operation
	from ValidOpStringsOnThisFileType

end







GO


/****** Object:  StoredProcedure [dbo].[updateAccount]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[updateAccount]	@AccountID int,
									@AccountName nvarchar(255)
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()
exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------
UPDATE dbo.Account
SET AccountName  = @AccountName
WHERE 	AccountID = @AccountID
GO

/****** Object:  StoredProcedure [dbo].[updateCompany]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[updateCompany]
	@CompanyID int, 
	@CompanyName nvarchar(255), 
	@CompanyDomicile nvarchar(255),
	@CompanyLegalName nvarchar(255),
	@CompanyRegistrationNo nvarchar(255)
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()
exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------
UPDATE [dbo].[Company]
SET CompanyID = @CompanyID,
	CompanyName = @CompanyName,
	CompanyDomicile = @CompanyDomicile,
	CompanyLegalName = @CompanyLegalName,
	CompanyRegistrationNo = @CompanyRegistrationNo
WHERE CompanyID = @CompanyID
select @CompanyID
GO


/****** Object:  StoredProcedure [dbo].[updateModelResource]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE procedure [dbo].[updateModelResource]
	@ModelResourceID int,
	@ModelResourceName  nvarchar(255),  
	@ResourceTypeID int,
	@OasisSystemID int,
	@ModelID int,
	@ModelResourceValue nvarchar(255)
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()
exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------
UPDATE dbo.ModelResource
SET ModelResourceName = @ModelResourceName,
	ResourceTypeID = @ResourceTypeID,
	OasisSystemID = @OasisSystemID,
	ModelID = @ModelID,
	ModelResourceValue = @ModelResourceValue
WHERE ModelResourceID = @ModelResourceID
Select @ModelResourceID
GO

/****** Object:  StoredProcedure [dbo].[updateProg]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[updateProg] @ProgID int,
								@ProgName nvarchar(255),
								@AccountID int,
								@TransformID int
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()
exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------
if @ProgName = ''
begin
	set @ProgName = 'Prog ' + convert(nvarchar(255),@ProgId)
end

--update prog table
UPDATE dbo.Prog
SET ProgName  = @ProgName,
	AccountID = @AccountID,
	TransformID = @TransformID
WHERE ProgID=@ProgID
GO

/****** Object:  StoredProcedure [dbo].[updateProgStatus]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[updateProgStatus] @ProgID int,
								@Status nvarchar(255)
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()
exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------
UPDATE dbo.Prog
SET [Status]  = @Status
WHERE ProgID=@ProgID
GO

/****** Object:  StoredProcedure [dbo].[updateProgOasisStatus]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[updateProgOasisStatus] @ProgOasisID int,
								@Status nvarchar(255)
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()
exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------
UPDATE dbo.ProgOasis
SET [Status]  = @Status
WHERE ProgOasisID=@ProgOasisID
GO

/****** Object:  StoredProcedure [dbo].[updateSourceAccountFileForProg]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[updateSourceAccountFileForProg] @FileId int, @ProgId int
AS
SET ANSI_NULLS ON;
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()
exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------
Declare @LegacyResourceId int	= (Select ISNULL(Max(ResourceId),0) + 1 From [Resource])
Declare @ResourceId int			= (Select ResourceId From [Resource] Where ResourceTable = 'Prog' And ResourceKey = @ProgId And ResourceTypeID = 102)
Declare @ExistingFileId int		= (Select FileID From [FileResource] Where ResourceId = @ResourceId)
Declare @FileResourceId int		= (Select ISNULL(Max(FileResourceId),0) + 1 From [FileResource])
--set existing file to legacy resource if exists
IF @ExistingFileId IS NOT NULL
	BEGIN
		Insert Into [Resource] values (@LegacyResourceId,'Prog',@ProgId,NULL,202)
		Update [FileResource] Set ResourceId = @LegacyResourceId Where [FileId] = @ExistingFileId
		Update [FileResource] Set ResourceId = @ResourceId		 Where [FileId] = @FileId
	END
ELSE
	BEGIN
		Insert Into [FileResource] values (@FileResourceId,@FileId,@ResourceId)
	END
GO
/****** Object:  StoredProcedure [dbo].[updateSourceLocationFileForProg]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[updateSourceLocationFileForProg] @FileId int, @ProgId int
AS
SET ANSI_NULLS ON;
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()
exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------
Declare @ResourceId int			= (Select ResourceId From [Resource] Where ResourceTable = 'Prog' And ResourceKey = @ProgId And ResourceTypeID = 101)
Declare @ExistingFileId int		= (Select FileID From [FileResource] Where ResourceId = @ResourceId)
Declare @LegacyResourceId int	= (Select ISNULL(Max(ResourceId),0) + 1 From [Resource])
Declare @FileResourceId int		= (Select ISNULL(Max(FileResourceId),0) + 1 From [FileResource])
--set existing file to legacy resource if exists
IF @ExistingFileId IS NOT NULL
	BEGIN
		Insert Into [Resource] values (@LegacyResourceId,'Prog',@ProgId,NULL,202)
		Update [FileResource] Set ResourceId = @LegacyResourceId Where [FileId] = @ExistingFileId
		Update [FileResource] Set ResourceId = @ResourceId		 Where [FileId] = @FileId
	END
ELSE
	BEGIN
		Insert Into [FileResource] values (@FileResourceId,@FileId,@ResourceId)
	END
GO
/****** Object:  StoredProcedure [dbo].[updateUser]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE procedure [dbo].[updateUser] 
			@BFEUserID int, 
			@BFEUserName nvarchar(255), 
			@CompanyID int, 
			@BFEUserLogin nvarchar(255), 
			@BFEUserPassword nvarchar(255), 
			@BFEUserDept nvarchar(255)
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()
exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------
UPDATE [dbo].[BFEUser]
SET BFEUserName = @BFEUserName,
	CompanyID = @CompanyID,
	BFEUserLogin = @BFEUserLogin,
	BFEUserPassword = HASHBYTES('SHA2_256',@BFEUserPassword),
	BFEUserDept = @BFEUserDept
WHERE BFEUserID = @BFEUserID
select @BFEUserID
GO


/****** Object:  StoredProcedure [dbo].[workflowFlattener]    Script Date: 30/09/2016 14:24:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[workflowFlattener] 
		(
		@ProgOasisID int = null,
		@WorkflowID int = null,
		@NumberOfSamples int = null,
		@GULThreshold int = null,
		@UseRandomNumberFile int = null,
		@OutputsStringGUL nvarchar(max) = null,
		@OutputsStringIL nvarchar(max) = null,
		@EventSetID nvarchar(255)  = null,
		@EventOccurrenceID nvarchar(255)  = null,
		@DemandSurge bit  = null,
		@LeakageFactor float  = null,
		@PerilWind bit  = null,
		@PerilSurge bit  = null,
		@PerilQuake bit  = null,
		@PerilFlood bit  = null,
		@ProcessRunName nvarchar(2500)  = null,
		@SummaryReports bit  = null
		)
AS
SET ANSI_NULLS ON;
SET ANSI_WARNINGS ON;
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(max) = '@ProgOasisID = ' + convert(nvarchar,@ProgOasisID) + ', ' +
										'@WorkflowID = ' + convert(nvarchar,@WorkflowID) + ', ' +
										'@NumberOfSamples = ' + convert(nvarchar,@NumberOfSamples) + ', ' +
										'@GULThreshold = ' + convert(nvarchar,@GULThreshold) + ', ' +
										'@UseRandomNumberFile = ' + convert(nvarchar,@UseRandomNumberFile) + ', ' +
										'@OutputsStringGUL = ' + @OutputsStringGUL + ', ' +
										'@OutputsStringIL = ' + @OutputsStringIL + ', ' +
										'@EventSetID = ' + @EventSetID + ', ' +
										'@EventOccurrenceID = ' + @EventOccurrenceID + ', ' +
										'@DemandSurge = ' + convert(nvarchar,@DemandSurge) + ', ' +
										'@LeakageFactor = ' + convert(nvarchar,@LeakageFactor) + ', ' +
										'@PerilWind = ' + convert(nvarchar,@PerilWind) + ', ' +
										'@PerilSurge = ' + convert(nvarchar,@PerilSurge) + ', ' +
										'@PerilQuake = ' + convert(nvarchar,@PerilQuake) + ', ' +
										'@PerilFlood = ' + convert(nvarchar,@PerilFlood)
declare	@LogTimestamp datetime = getdate()
exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp;
----------------------------------------------------------------------------
--fix NULLS
if @WorkflowID is null begin set @WorkflowID = 1 end
if @NumberOfSamples is null begin set @NumberOfSamples = 0 end
if @GULThreshold is null begin set @GULThreshold = 0 end
if @UseRandomNumberFile is null begin set @UseRandomNumberFile = 0 end
if @PerilWind is null begin  set @PerilWind  = 0 end
if @PerilSurge is null begin set @PerilSurge = 0 end
if @PerilQuake is null begin set @PerilQuake = 0 end
if @PerilFlood is null begin set @PerilFlood = 0 end
if @ProcessRunName is null begin set @ProcessRunName = '' end
if @SummaryReports is null begin set @SummaryReports = 1 end
--Declare Variables
Declare	@ProgId int						= (Select ProgId From ProgOasis Where ProgOasisId = @ProgOasisID)
Declare	@ModelId int					= (Select ModelId From ProgOasis Where ProgOasisId = @ProgOasisID)
Declare	@SessionId int					= (Select SessionId From ProgOasis Where ProgOasisId = @ProgOasisID)
Declare	@ProcessRunId int				= (Select ISNULL(Max(ProcessRunID),0)+1 From ProcessRun)
Declare	@ElementRunId int				= (Select ISNULL(Max(ElementRunID),0) From ElementRun)
Declare	@ParameterRunId int				= (Select ISNULL(Max(ParameterRunID),0) From ParameterRun)
Declare	@SummaryLevelRunId int			= (Select ISNULL(Max(SummaryLevelRunID),0) From SummaryLevelRun)
Declare	@OutputRunId int				= (Select ISNULL(Max(OutputRunID),0) From OutputRun)
Declare @ProgName nvarchar(255)			= (Select ProgName From Prog Where ProgId = @ProgId)
Declare @ModelName nvarchar(255)		= (Select ModelName From Model Where ModelId = @ModelId)
Declare	@ModuleSupplierId nvarchar(255)	= (Select ModelResourceValue From ModelResource Where ModelID = @ModelID And ModelResourceName = 'module_supplier_id')
Declare	@ModelVersionId nvarchar(255)	= (Select ModelResourceValue From ModelResource Where ModelID = @ModelID And ModelResourceName = 'model_version_id')
Declare	@NumberOfPeriods nvarchar(255)	= (Select ModelResourceValue From ModelResource Where ModelID = @ModelID And ModelResourceName = 'number_of_periods')
Declare @SQL nvarchar(max)
--Set	@ProcessRunName = @ProgName + ' - ' + @ModelName + case when @ProcessRunName = '' then '' else ': ' + @ProcessRunName end
if @SummaryReports = 1
begin
	--Add in summary outputs where appropriate GUL
	if len(@OutputsStringGUL) > 0
	begin
		if @OutputsStringGUL not like '%gulprogFullUncAEP%'
		begin
			set @OutputsStringGUL = @OutputsStringGUL + ', gulprogFullUncAEP'
		end
		if @OutputsStringGUL not like '%gulprogFullUncOEP%'
		begin
			set @OutputsStringGUL = @OutputsStringGUL + ', gulprogFullUncOEP'
		end
		if @OutputsStringGUL not like '%gulprogAAL%'
		begin
			set @OutputsStringGUL = @OutputsStringGUL + ', gulprogAAL'
		end
	end
	--Add in summary outputs where appropriate IL
	if len(@OutputsStringIL) > 0
	begin
		if @OutputsStringIL not like '%ilprogFullUncAEP%'
		begin
			set @OutputsStringIL = @OutputsStringIL + ', ilprogFullUncAEP'
		end
		if @OutputsStringIL not like '%ilprogFullUncOEP%'
		begin
			set @OutputsStringIL = @OutputsStringIL + ', ilprogFullUncOEP'
		end
		if @OutputsStringIL not like '%ilprogAAL%'
		begin
			set @OutputsStringIL = @OutputsStringIL + ', ilprogAAL'
		end
	end
end
--Add quotation marks into Outputs String Params
Set @OutputsStringGUL = '''' + @OutputsStringGUL + ''''
Set @OutputsStringIL = '''' + @OutputsStringIL + ''''
Set @OutputsStringGUL = REPLACE(@OutputsStringGUL,' ','')
Set @OutputsStringIL = REPLACE(@OutputsStringIL,' ', '')
Set @OutputsStringGUL = REPLACE(@OutputsStringGUL,',',''',''')
Set @OutputsStringIL = REPLACE(@OutputsStringIL,',', ''',''')
--Workflow
Create Table #WorkflowElements (RowID int identity(1,1), ElementRunID int null, ElementRunName nvarchar(255) null, WorkflowElementID int null,
								ParameterRunName nvarchar(255) null, WorkflowParameterID int null, ParameterRunSource nvarchar(255) null, 
								ParameterRunValue nvarchar(255) null, ParameterId int null, ParameterRunFormat nvarchar(255) null)
Insert Into #WorkflowElements (ElementRunID, ElementRunName, WorkflowElementID, ParameterRunName, WorkflowParameterID, ParameterRunSource, ParameterId, ParameterRunFormat)
Select	WE.WorkflowElementID AS ElementRunID,
		WE.WorkflowElementName AS ElementRunName,
		WE.WorkflowElementID AS WorkflowElementID,
		P.ParameterName AS ParameterRunName,
		WP.WorkflowParameterID AS WorkflowParameterID,
		WP.WorkflowParameterSource AS ParameterRunSource,
		WP.ParameterId AS ParameterId,
		P.ParameterFormat
From	WorkflowElement AS WE
Join	WorkflowParameter AS WP on WE.WorkflowElementID = WP.WorkflowElementID
Join	Parameter AS P on WP.ParameterID =P.ParameterID
Where	WE.WorkflowID = @WorkflowID
And		P.ParameterID <> 10 -- 10 = model specific parameter id
Update #WorkflowElements Set ParameterRunValue = @ProgID				Where ParameterId = 2
Update #WorkflowElements Set ParameterRunValue = @ModuleSupplierId		Where ParameterId = 3
Update #WorkflowElements Set ParameterRunValue = @ModelVersionId		Where ParameterId = 4
Update #WorkflowElements Set ParameterRunValue = @NumberOfSamples		Where ParameterId = 5
Update #WorkflowElements Set ParameterRunValue = @GULThreshold			Where ParameterId = 6
Update #WorkflowElements Set ParameterRunValue = 'L:'					Where ParameterId = 7
Update #WorkflowElements Set ParameterRunValue = @UseRandomNumberFile	Where ParameterId = 8
--ProcessRun
Insert Into ProcessRun (ProcessRunId, ProcessRunName, ProgOasisID) 
Select	@ProcessRunId,
		@ProcessRunName,
		@ProgOasisID
--ElementRun
Insert Into ElementRun (ElementRunID, ElementRunName, WorkflowElementID, ProcessRunID)
Select	Distinct @ElementRunId + ElementRunID AS ElementRunID,
		ElementRunName,
		WorkflowElementID,
		@ProcessRunId
From	#WorkflowElements
--ParameterRun
Insert Into ParameterRun (ParameterRunID, ParameterRunName, ElementRunID, WorkflowParameterID, ParameterRunValue, ParameterRunFormat)
Select	Distinct @ParameterRunId + RowID AS ParameterRunID,
		ParameterRunName,
		@ElementRunId + ElementRunID AS ElementRunID,
		WorkflowParameterID,
		ParameterRunValue,
		ParameterRunFormat
From	#WorkflowElements
--Model Specific Parameters
Create Table #ModelParameters (ParameterRunID int identity(1,1), ParameterRunName nvarchar(255) NULL, ParameterRunValue nvarchar(255) NULL, ParameterRunFormat nvarchar(255) NULL)
Insert Into #ModelParameters (ParameterRunName,ParameterRunValue)
Select	ModelResourceName AS ParameterRunName,
		NULL AS ParameterRunValue
From	ModelResource
Where	ModelID = @ModelId
And		ResourceTypeID = 1001 --1001 = model specific parameter type
Update #ModelParameters Set ParameterRunValue = convert(nvarchar(255),@DemandSurge),		ParameterRunFormat = 'bool'		Where ParameterRunName = 'demand_surge'
Update #ModelParameters Set ParameterRunValue = convert(nvarchar(255),@LeakageFactor),	ParameterRunFormat = 'float'	Where ParameterRunName = 'leakage_factor'
Update #ModelParameters Set ParameterRunValue = convert(nvarchar(255),@PerilWind),		ParameterRunFormat = 'bool'		Where ParameterRunName = 'peril_wind'
Update #ModelParameters Set ParameterRunValue = convert(nvarchar(255),@PerilSurge),		ParameterRunFormat = 'bool'		Where ParameterRunName = 'peril_surge'
Update #ModelParameters Set ParameterRunValue = convert(nvarchar(255),@PerilQuake),		ParameterRunFormat = 'bool'		Where ParameterRunName = 'peril_quake'
Update #ModelParameters Set ParameterRunValue = convert(nvarchar(255),@PerilFlood),		ParameterRunFormat = 'bool'		Where ParameterRunName = 'peril_flood'
Update #ModelParameters Set ParameterRunValue = convert(nvarchar(255),@SessionID),		ParameterRunFormat = 'int'		Where ParameterRunName = 'session_id'
Update #ModelParameters Set ParameterRunValue = convert(nvarchar(255),@EventSetID),		ParameterRunFormat = 'str'		Where ParameterRunName = 'event_set'
Update #ModelParameters Set ParameterRunValue = convert(nvarchar(255),@EventOccurrenceID),ParameterRunFormat = 'str'		Where ParameterRunName = 'event_occurrence_id'
--select * from #ModelParameters
Declare @OutputElementRunID int = (Select ElementRunID From ElementRun Where ProcessRunID = @ProcessRunID And WorkflowElementId = 2)
Set @ParameterRunId = (Select ISNULL(Max(ParameterRunID),0) From ParameterRun)
Insert Into ParameterRun (ParameterRunID, ParameterRunName, ElementRunID, WorkflowParameterID,ParameterRunValue,ParameterRunFormat)
Select	ParameterRunID + @ParameterRunId AS ParameterRunID,
		ParameterRunName,
		@OutputElementRunID AS ElementRunID,
		10 AS WorkflowParameterID, -- 10 = model specific parameter id
		ParameterRunValue,
		ParameterRunFormat
From	#ModelParameters
--set boolean flags
--update ParameterRun set ParameterRunValue = 'true'  where ParameterRunValue = '1' and ParameterRunFormat = 'bool'
--update ParameterRun set ParameterRunValue = 'false' where ParameterRunValue = '0' and ParameterRunFormat = 'bool'
--SummaryLevelRun
Create Table #OutputRun (RowId int identity(1,1), SummaryLevelID int, PerspectiveID int, OutputTypeID int)
Set @SQL = '
Select	SL.SummaryLevelID,
		P.PerspectiveID,
		OT.OutputTypeID
From	OutputType AS OT
Join	AnalysisType AS AT on OT.AnalysisTypeID = AT.AnalysisTypeID
Join	SummaryLevel AS SL on OT.SummaryLevelID = SL.SummaryLevelID
Join	Perspective AS P on OT.PerspectiveID = P.PerspectiveID
Where	lower(P.PerspectiveName) + SL.ParameterStub + AT.ParameterStub in (' + @OutputsStringGUL + ')
Or		lower(P.PerspectiveName) + SL.ParameterStub + AT.ParameterStub in (' + @OutputsStringIL + ')'
Insert Into #OutputRun
Exec sp_executesql @SQL
Insert Into SummaryLevelRun (SummaryLevelRunID, SummaryLevelID, ProcessRunID, SummarySetID, PerspectiveID)
Select	@SummaryLevelRunID + RowId AS SummaryLevelRunID,
		SummaryLevelID,
		@ProcessRunId AS ProcessRunId,
		DENSE_RANK() Over (Partition By PerspectiveID Order By SummaryLevelID) AS SummarySetID,
		PerspectiveID
From	#OutputRun
Insert Into OutputRun (OutputRunID, OutputTypeID, ElementRunID, OutputID, SummaryLevelRunId)
Select	@OutputRunID + RowId AS OutputRunID,
		OutputTypeID,
		@OutputElementRunID AS ElementRunID,
		DENSE_RANK() Over (Partition By PerspectiveID Order By PerspectiveID,SummaryLevelID) AS OutputID,
		@SummaryLevelRunID + RowId AS SummaryLevelRunID
From	#OutputRun
Drop Table #OutputRun
Drop Table #ModelParameters
Drop Table #WorkflowElements
Select @ProcessRunId AS ReturnValue
GO
						     
						     
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[getTransformInputFilesForProg] @ProgId int
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = '@ProgId = ' + convert(nvarchar,@ProgId)
declare	@LogTimestamp datetime = getdate()
exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

--get transform
DECLARE @TransformID int = (Select TransformID From Prog Where ProgID = @ProgID)

DECLARE @SourceLoc nvarchar(255)
DECLARE @ValidationFileLoc nvarchar(255)
DECLARE @TransformationFileLoc nvarchar(255)

DECLARE @SourceAcc nvarchar(255)
DECLARE @ValidationFileAcc nvarchar(255)
DECLARE @TransformationFileAcc nvarchar(255)

--source loc file
Select @SourceLoc = L.LocationPathUnix + '/' + F.[FileName] 
From [File] AS F Join Location AS L on F.LocationID = L.LocationID Join FileResource AS FR on F.FileID = FR.FileID Join [Resource] AS R on FR.ResourceID = R.ResourceID 
Where R.ResourceTable = 'Prog' And ResourceKey = @ProgId and R.ResourceTypeID = 101 --source loc

--validation loc file
Select @ValidationFileLoc = L.LocationPathUnix + '/' + F.[FileName] 
From [File] AS F Join Location AS L on F.LocationID = L.LocationID Join FileResource AS FR on F.FileID = FR.FileID Join [Resource] AS R on FR.ResourceID = R.ResourceID 
Where R.ResourceTable = 'Transform' And ResourceKey = @TransformID and R.ResourceTypeID = 121 --To Location XSD

--transformation loc file
Select @TransformationFileLoc = L.LocationPathUnix + '/' + F.[FileName] 
From [File] AS F Join Location AS L on F.LocationID = L.LocationID Join FileResource AS FR on F.FileID = FR.FileID Join [Resource] AS R on FR.ResourceID = R.ResourceID 
Where R.ResourceTable = 'Transform' And ResourceKey = @TransformID and R.ResourceTypeID = 124 --Location XSLT

--source acc file
Select @SourceAcc = L.LocationPathUnix + '/' + F.[FileName] 
From [File] AS F Join Location AS L on F.LocationID = L.LocationID Join FileResource AS FR on F.FileID = FR.FileID Join [Resource] AS R on FR.ResourceID = R.ResourceID 
Where R.ResourceTable = 'Prog' And ResourceKey = @ProgId and R.ResourceTypeID = 102

--validation acc file
Select @ValidationFileAcc = L.LocationPathUnix + '/' + F.[FileName] 
From [File] AS F Join Location AS L on F.LocationID = L.LocationID Join FileResource AS FR on F.FileID = FR.FileID Join [Resource] AS R on FR.ResourceID = R.ResourceID 
Where R.ResourceTable = 'Transform' And ResourceKey = @TransformID and R.ResourceTypeID = 122 --To Location XSD

--transformation acc file
Select @TransformationFileAcc = L.LocationPathUnix + '/' + F.[FileName] 
From [File] AS F Join Location AS L on F.LocationID = L.LocationID Join FileResource AS FR on F.FileID = FR.FileID Join [Resource] AS R on FR.ResourceID = R.ResourceID 
Where R.ResourceTable = 'Transform' And ResourceKey = @TransformID and R.ResourceTypeID = 125 --Location XSLT


Select	@SourceLoc AS SourceLoc,
		@ValidationFileLoc AS ValidationLoc,
		@TransformationFileLoc AS TransformationLoc,
		@SourceAcc AS SourceAcc,
		@ValidationFileAcc AS ValidationAcc,
		@TransformationFileAcc AS TransformationAcc


GO




SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


CREATE PROCEDURE [dbo].[getTransformInputFilesForProgOasis] @ProgOasisId int
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = '@ProgOasisId = ' + convert(nvarchar,@ProgOasisId)
declare	@LogTimestamp datetime = getdate()
exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

--get transform
DECLARE @TransformID int = (Select TransformID From ProgOasis Where ProgOasisID = @ProgOasisId)
DECLARE @ProgID int = (Select ProgId From ProgOasis Where ProgOasisID = @ProgOasisId)

DECLARE @SourceLoc nvarchar(255)
DECLARE @ValidationFileLoc nvarchar(255)
DECLARE @TransformationFileLoc nvarchar(255)


--source loc file
Select @SourceLoc = L.LocationPathUnix + '/' + F.[FileName] 
From [File] AS F Join Location AS L on F.LocationID = L.LocationID Join FileResource AS FR on F.FileID = FR.FileID Join [Resource] AS R on FR.ResourceID = R.ResourceID 
Where R.ResourceTable = 'Prog' And ResourceKey = @ProgId and R.ResourceTypeID = 103 --can loc

--validation loc file
Select @ValidationFileLoc = L.LocationPathUnix + '/' + F.[FileName] 
From [File] AS F Join Location AS L on F.LocationID = L.LocationID Join FileResource AS FR on F.FileID = FR.FileID Join [Resource] AS R on FR.ResourceID = R.ResourceID 
Where R.ResourceTable = 'Transform' And ResourceKey = @TransformID and R.ResourceTypeID = 120 --From Location XSD

--transformation loc file
Select @TransformationFileLoc = L.LocationPathUnix + '/' + F.[FileName] 
From [File] AS F Join Location AS L on F.LocationID = L.LocationID Join FileResource AS FR on F.FileID = FR.FileID Join [Resource] AS R on FR.ResourceID = R.ResourceID 
Where R.ResourceTable = 'Transform' And ResourceKey = @TransformID and R.ResourceTypeID = 124 --Location XSLT

Select	@SourceLoc AS SourceLoc,
		@ValidationFileLoc AS ValidationLoc,
		@TransformationFileLoc AS TransformationLoc



GO



SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[generateOutputTransformFileRecordsForProg] @ProgId int, @CanLocFileName nvarchar(255), @CanAccFileName nvarchar(255)
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = '@ProgId = ' + convert(nvarchar,@ProgId) + ', @CanLocFileName = ' + @CanLocFileName + ', @CanAccFileName = ' + @CanAccFileName
declare	@LogTimestamp datetime = getdate()
exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

--loc records
	Declare @ResourceId int = (Select ResourceId From [Resource] Where ResourceTable = 'Prog' And ResourceKey = @ProgId And ResourceTypeID = 103) 
	Declare @LegacyResourceId int = (Select isnull(MAX(ResourceId),0)+1 From [Resource])
	Declare	@FileId int = (Select isnull(MAX(FileId),0) + 1 From [File])
	Declare	@FileResourceID int = (Select isnull(MAX(FileResourceID),0) + 1 From FileResource)
	Declare @FileDesc nvarchar(255) = 'Canonical Loc File: Prog ' + convert(nvarchar,@ProgId)

	--insert new resource row for legacy resource record
	Insert into [resource] (ResourceId, ResourceTable, ResourceKey, ResourceQualifier, ResourceTypeID) values (@LegacyResourceId,'Prog',@ProgId,NULL,203) 

	--set resource id for existing file (if exists) set to legacy resource id
	update [fileresource] set resourceid = @LegacyResourceId Where resourceid = @ResourceId

	--insert new file record for generated file
	insert into [file] values (@FileId, @CanLocFileName, @FileDesc, 1, 1, 103, getdate(), getdate(), null, 'Sys', 'Sys', NULL, 103) 
	insert into [fileresource] values (@FileResourceID, @FileID, @ResourceID)


--acc records
	Set @ResourceId = (Select ResourceId From [Resource] Where ResourceTable = 'Prog' And ResourceKey = @ProgId And ResourceTypeID = 104) 
	Set @LegacyResourceId = (Select isnull(MAX(ResourceId),0)+1 From [Resource])
	Set	@FileId = (Select isnull(MAX(FileId),0) + 1 From [File])
	Set	@FileResourceID = (Select isnull(MAX(FileResourceID),0) + 1 From FileResource)
	Set @FileDesc = 'Canonical Acc File: Prog ' + convert(nvarchar,@ProgId)

	--insert new resource row for legacy resource record
	Insert into [resource] (ResourceId, ResourceTable, ResourceKey, ResourceQualifier, ResourceTypeID) values (@LegacyResourceId,'Prog',@ProgId,NULL,204)

	--set resource id for existing file (if exists) set to legacy resource id
	update [fileresource] set resourceid = @LegacyResourceId Where resourceid = @ResourceId

	--insert new file record for generated file
	insert into [file] values (@FileId, @CanAccFileName, @FileDesc, 1, 1, 103, getdate(), getdate(), null, 'Sys', 'Sys', NULL, 104)
	insert into [fileresource] values (@FileResourceID, @FileID, @ResourceID)


GO


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[generateOutputTransformFileRecordsForProgOasos] @ProgOasisId int, @ModelLocFileName nvarchar(255)
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = '@ProgOasisId = ' + convert(nvarchar,@ProgOasisId) + ', @ModelLocFileName = ' + @ModelLocFileName 
declare	@LogTimestamp datetime = getdate()
exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

--loc records
	Declare @ResourceId int = (Select ResourceId From [Resource] Where ResourceTable = 'ProgOasis' And ResourceKey = @ProgOasisId And ResourceTypeID = 105) 
	Declare @LegacyResourceId int = (Select isnull(MAX(ResourceId),0)+1 From [Resource])
	Declare	@FileId int = (Select isnull(MAX(FileId),0) + 1 From [File])
	Declare	@FileResourceID int = (Select isnull(MAX(FileResourceID),0) + 1 From FileResource)
	Declare @FileDesc nvarchar(255) = 'Model Loc File: ProgOasis ' + convert(nvarchar,@ProgOasisId)

	--insert new resource row for legacy resource record
	Insert into [resource] (ResourceId, ResourceTable, ResourceKey, ResourceQualifier, ResourceTypeID) values (@LegacyResourceId,'ProgOasis',@ProgOasisId,NULL,205) 

	--set resource id for existing file (if exists) set to legacy resource id
	update [fileresource] set resourceid = @LegacyResourceId Where resourceid = @ResourceId

	--insert new file record for generated file
	insert into [file] values (@FileId, @ModelLocFileName, @FileDesc, 1, 1, 101, getdate(), getdate(), null, 'Sys', 'Sys', NULL, 105) 
	insert into [fileresource] values (@FileResourceID, @FileID, @ResourceID)

GO
/****** Object:  StoredProcedure [dbo].[getModelTransformForModel]    Script Date: 19/01/2018 14:25:28 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[getModelTransformForModel] @ModelId int
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

SELECT ModelTransform.TransformId
FROM ModelTransform
JOIN Transform ON ModelTransform.TransformID=Transform.TransformID
WHERE
    @ModelId = ModelID
AND
    TransformTypeID = 2

GO
/****** Object:  StoredProcedure [dbo].[getSourceTransformForModel]    Script Date: 19/01/2018 14:30:27 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[getSourceTransformForModel] @ModelId int
AS
SET NOCOUNT ON;
----------------------------------------------------------------------------
--log database usage
declare	@ProcedureName nvarchar(255)  = (Select OBJECT_NAME(@@PROCID))
declare	@ParameterList nvarchar(2500) = ''
declare	@LogTimestamp datetime = getdate()

exec updateLogDatabaseUsage @ProcedureName,@ParameterList,@LogTimestamp
----------------------------------------------------------------------------

SELECT ModelTransform.TransformId
FROM ModelTransform
JOIN Transform ON ModelTransform.TransformID=Transform.TransformID
WHERE
    @ModelId = ModelID
AND
    TransformTypeID = 1

GO
---------------------------------------------------------------------------------
--Table Values
---------------------------------------------------------------------------------
INSERT [dbo].[Format] ([FormatID], [FormatName], [FormatDesc]) VALUES (1, N'int', NULL)
INSERT [dbo].[Format] ([FormatID], [FormatName], [FormatDesc]) VALUES (2, N'float', NULL)
INSERT [dbo].[Format] ([FormatID], [FormatName], [FormatDesc]) VALUES (3, N'nvarchar(255)', NULL)
INSERT [dbo].[Format] ([FormatID], [FormatName], [FormatDesc]) VALUES (4, N'datetime', NULL)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (0, N'Other', 3, NULL, NULL,0)						     
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (1, N'RowId', 2, NULL, NULL,0)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (2, N'TIV', 2, NULL, NULL,1)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (3, N'LocationID', 1, NULL, NULL,0)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (4, N'PerilID', 1, NULL, NULL,0)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (5, N'CoverageTypeID', 1, NULL, NULL,0)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (6, N'AccountNumber', 3, NULL, NULL,0)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (7, N'PostCode', 3, NULL, NULL,0)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (8, N'Latitude', 2, NULL, NULL,0)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (9, N'Longitude', 2, NULL, NULL,0)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (10, N'SourceLocationNumber', 3, NULL, NULL,0)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (11, N'BuildingScheme', 3, NULL, NULL,0)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (12, N'BuildingClass', 3, NULL, NULL,0)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (13, N'OccupancyScheme', 3, NULL, NULL,0)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (14, N'OccupancyType', 3, NULL, NULL,0)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (15, N'CoverageLimit', 2, NULL, NULL,1)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (16, N'CoverageDeductible', 2, NULL, NULL,1)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (17, N'SiteLimit', 2, NULL, NULL,1)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (18, N'SiteDeductible', 2, NULL, NULL,1)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (19, N'CombinedLimit', 2, NULL, NULL,1)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (20, N'CombinedDeductible', 2, NULL, NULL,1)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (21, N'PolicyNumber', 3, NULL, NULL,0)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (22, N'PolicyType', 1, NULL, NULL,0)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (23, N'AttachmentPoint', 2, NULL, NULL,1)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (24, N'LayerLimit', 2, NULL, NULL,1)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (25, N'MinimumDeductible', 2, NULL, NULL,1)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (26, N'MaximumDeductible', 2, NULL, NULL,1)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (27, N'BlanketDeductible', 2, NULL, NULL,1)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (28, N'BlanketLimit', 2, NULL, NULL,1)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (29, N'StateCode', 3, NULL, NULL,0)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (30, N'CountyCode', 3, NULL, NULL,0)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (31, N'YearBuilt',1,NULL,NULL,0)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (32, N'YearUpgraded',1,NULL,NULL,0)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (33, N'NumberStories',1,NULL,NULL,0)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (34, N'NumberBuildings',1,NULL,NULL,0)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (35, N'FloorArea',2,NULL,NULL,0)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (36, N'RoofCovering',1,NULL,NULL,0)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (37, N'RoofGeometry',1,NULL,NULL,0)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (38, N'RoofAnchor',1,NULL,NULL,0)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (39, N'RoofAge',1,NULL,NULL,0)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (40, N'RoofFramingType',1,NULL,NULL,0)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (41, N'CladdingRating',1,NULL,NULL,0)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (42, N'OpeningProtection ',1,NULL,NULL,0)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (43, N'CrestaZone',3,NULL,NULL,0)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (44, N'CountryGeoID',3,NULL,NULL,0)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (45, N'CountryScheme',3,NULL,NULL,0)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (46, N'CountryCode',3,NULL,NULL,0)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (47, N'Currency',3,NULL,NULL,0)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (48, N'AddressMatch',3,NULL,NULL,0)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (49, N'Basement',3,NULL,NULL,0)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (50, N'County',3,NULL,NULL,0)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (51, N'State',3,NULL,NULL,0)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (52, N'Country',3,NULL,NULL,0)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (53, N'SubLimitType',3,NULL,NULL,0)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (54, N'SubLimitReference',3,NULL,NULL,0)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (55, N'SubLimitLimit',2,NULL,NULL,1)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (56, N'MinimumSubLimitDeductible',2,NULL,NULL,1)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (57, N'ProportionalShareOfLimit',2,NULL,NULL,1)
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (58, N'MaximumSubLimitDeductible',2,NULL,NULL,1)						     
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (59, N'City',3,NULL,NULL,0)						     
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (60, N'CityCode',3,NULL,NULL,0)	
INSERT [dbo].[Field] ([FieldID], [FieldName], [FormatID], [FieldlLength], [FieldRule], [IsOasisField]) VALUES (61, N'LineOfBusiness',3,NULL,NULL,0)	
INSERT [dbo].[ProfileType] ([ProfileTypeID], [ProfileTypeName]) VALUES (0, N'Dummy')
INSERT [dbo].[ProfileType] ([ProfileTypeID], [ProfileTypeName]) VALUES (1, N'Canonical Location File')
INSERT [dbo].[ProfileType] ([ProfileTypeID], [ProfileTypeName]) VALUES (2, N'Canonical Account File')
INSERT [dbo].[Profile] ([ProfileID], [ProfileName], [ProfileTypeID]) VALUES (0, N'Dummy Profile', 0)
INSERT [dbo].[Company] ([CompanyID], [CompanyName], [CompanyDomicile], [CompanyLegalName], [CompanyRegistrationNo], [Deleted]) VALUES (1, N'Admin', NULL, NULL, NULL, 0)
INSERT [dbo].[BFEUser] ([BFEUserID], [BFEUserName], [CompanyID], [BFEUserLogin], [BFEUserPassword], [BFEUserDept], [Deleted]) VALUES (1, N'Admin', 1, N'Admin', HASHBYTES('SHA2_256',N'password'), N'System', 0)
INSERT [dbo].[Owner] ([OwnerID], [OwnerName], [BFEUserID], [OwnerDesc], [Deleted]) VALUES (1, N'System', 1, N'System', 0)
INSERT [dbo].[SecurityGroup] ([SecurityGroupID], [SecurotyGroupName], [Deleted]) VALUES (1, N'User Administration', 0)
INSERT [dbo].[SecurityGroup] ([SecurityGroupID], [SecurotyGroupName], [Deleted]) VALUES (2, N'Systems Configuration', 0)
INSERT [dbo].[SecurityGroup] ([SecurityGroupID], [SecurotyGroupName], [Deleted]) VALUES (3, N'File Administration', 0)
INSERT [dbo].[SecurityGroup] ([SecurityGroupID], [SecurotyGroupName], [Deleted]) VALUES (4, N'Canonical Model', 0)
INSERT [dbo].[SecurityGroup] ([SecurityGroupID], [SecurotyGroupName], [Deleted]) VALUES (5, N'Utilities', 0)
INSERT [dbo].[SecurityGroup] ([SecurityGroupID], [SecurotyGroupName], [Deleted]) VALUES (6, N'Enquiry', 0)
INSERT [dbo].[SecurityGroup] ([SecurityGroupID], [SecurotyGroupName], [Deleted]) VALUES (7, N'Exposures and Policies', 0)
INSERT [dbo].[SecurityGroup] ([SecurityGroupID], [SecurotyGroupName], [Deleted]) VALUES (8, N'Workflow', 0)
INSERT [dbo].[SecurityGroup] ([SecurityGroupID], [SecurotyGroupName], [Deleted]) VALUES (9, N'Workflow Management Admin', 0)
INSERT [dbo].[SecurityGroup] ([SecurityGroupID], [SecurotyGroupName], [Deleted]) VALUES (10, N'User View', 0)
INSERT [dbo].[UserSecurityGroup] ([UserSecurityGroupID], [BFEUserID], [Security Group ID]) VALUES (1, 1, 1)
INSERT [dbo].[UserSecurityGroup] ([UserSecurityGroupID], [BFEUserID], [Security Group ID]) VALUES (2, 1, 2)
INSERT [dbo].[UserSecurityGroup] ([UserSecurityGroupID], [BFEUserID], [Security Group ID]) VALUES (3, 1, 3)
INSERT [dbo].[UserSecurityGroup] ([UserSecurityGroupID], [BFEUserID], [Security Group ID]) VALUES (4, 1, 4)
INSERT [dbo].[UserSecurityGroup] ([UserSecurityGroupID], [BFEUserID], [Security Group ID]) VALUES (5, 1, 5)
INSERT [dbo].[UserSecurityGroup] ([UserSecurityGroupID], [BFEUserID], [Security Group ID]) VALUES (6, 1, 6)
INSERT [dbo].[UserSecurityGroup] ([UserSecurityGroupID], [BFEUserID], [Security Group ID]) VALUES (7, 1, 7)
INSERT [dbo].[UserSecurityGroup] ([UserSecurityGroupID], [BFEUserID], [Security Group ID]) VALUES (8, 1, 8)
INSERT [dbo].[UserSecurityGroup] ([UserSecurityGroupID], [BFEUserID], [Security Group ID]) VALUES (9, 1, 9)
INSERT [dbo].[UserSecurityGroup] ([UserSecurityGroupID], [BFEUserID], [Security Group ID]) VALUES (10, 1, 10)
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (101, N'Source Location File', N'Prog')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (102, N'Source Account File', N'Prog')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (103, N'Canonical Location File', N'Prog')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (104, N'Canonical Account File', N'Prog')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (105, N'Model Lookup File', N'ProgOasis')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (106, N'Lookup Return File', N'ProgOasis')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (107, N'Lookup Return Non Match File', N'ProgOasis')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (108, N'Lookup Return Error File', N'ProgOasis')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (109, N'Oasis GUL Summary Xref File', N'Flamingo')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (110, N'Oasis FM Summary Xref File', N'Flamingo')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (111, N'Oasis Items File', N'ProgOasis')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (112, N'Oasis Coverage File', N'ProgOasis')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (113, N'Oasis Item Dictionary File', N'ProgOasis')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (114, N'Oasis FM Programme File', N'ProgOasis')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (115, N'Oasis FM Policy TC File', N'ProgOasis')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (116, N'Oasis FM Policy Profile File', N'ProgOasis')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (117, N'Oasis FM XRef File', N'ProgOasis')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (118, N'CanonicalLocationProfile', N'Transform')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (119, N'CanonicalAccountProfile', N'Transform')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (120, N'FromLocationXSD', N'Transform')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (121, N'ToLocationXSD', N'Transform')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (122, N'FromAccountXSD', N'Transform')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (123, N'ToAccountXSD', N'Transform')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (124, N'LocationXSLT', N'Transform')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (125, N'AccountXSLT', N'Transform')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (126, N'ModelLookupFileExtension', N'Model')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (127, N'Oasis FM Dict File', N'ProgOasis')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (128, N'Oasis Output File', N'ProcessRun')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (129, N'ToLocationXSD Profile', N'Transform')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (130, N'ToAccountXSD Profile', N'Transform')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (201, N'Legacy Source Location File', N'Prog')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (202, N'Legacy Source Account File', N'Prog')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (203, N'Legacy Canonical Location File', N'Prog')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (204, N'Legacy Canonical Account File', N'Prog')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (205, N'Legacy Model Lookup File', N'ProgOasis')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (206, N'Legacy Lookup Return File', N'ProgOasis')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (207, N'Legacy Lookup Return Non Match File', N'ProgOasis')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (208, N'Legacy Lookup Return Error File', N'ProgOasis')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (211, N'Legacy Oasis Exposure Version File', N'ProgOasis')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (212, N'Legacy Oasis Exposure Dictionary File', N'ProgOasis')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (213, N'Legacy Oasis Exposure Correlation File', N'ProgOasis')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (214, N'Legacy Oasis FM Prog File', N'ProgOasis')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (215, N'Legacy Oasis FM Programme File', N'ProgOasis')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (216, N'Legacy Oasis FM Policy TC File', N'ProgOasis')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (217, N'Legacy Oasis FM Policy Profile File', N'ProgOasis')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (227, N'Legacy Oasis FM Dict File', N'ProgOasis')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (300, N'Model Group Field', N'Model')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (301, N'Module Supplier ID for calc BE', N'Model')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (302, N'Model Version ID for calc BE', N'Model')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (303, N'Event Set', N'Model')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (304, N'Event Occurrence', N'Model')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (305, N'File Extension', N'Model')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (900, N'Menu', N'System')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (999, N'Dummy', N'Dummy')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (1000, N'API', N'OasisSystem')
INSERT [dbo].[ResourceType] ([ResourceTypeID], [ResourceTypeName], [Source]) VALUES (1001, N'DataRecord', N'User')
INSERT [dbo].[Resource] ([ResourceID], [ResourceTable], [ResourceKey], [ResourceQualifier], [ResourceTypeID]) VALUES (200, N'System Config', NULL, N'Landing Page', 900)
INSERT [dbo].[Resource] ([ResourceID], [ResourceTable], [ResourceKey], [ResourceQualifier], [ResourceTypeID]) VALUES (300, N'File Admin', NULL, N'Landing Page', 900)
INSERT [dbo].[Resource] ([ResourceID], [ResourceTable], [ResourceKey], [ResourceQualifier], [ResourceTypeID]) VALUES (400, N'Canonical Model', NULL, NULL, 900)
INSERT [dbo].[Resource] ([ResourceID], [ResourceTable], [ResourceKey], [ResourceQualifier], [ResourceTypeID]) VALUES (500, N'Utilities', NULL, N'Landing Page', 900)
INSERT [dbo].[Resource] ([ResourceID], [ResourceTable], [ResourceKey], [ResourceQualifier], [ResourceTypeID]) VALUES (600, N'Enquiry', NULL, N'Landing Page', 900)
INSERT [dbo].[Resource] ([ResourceID], [ResourceTable], [ResourceKey], [ResourceQualifier], [ResourceTypeID]) VALUES (700, N'Exposures & Policies', NULL, N'Landing Page', 900)
INSERT [dbo].[Resource] ([ResourceID], [ResourceTable], [ResourceKey], [ResourceQualifier], [ResourceTypeID]) VALUES (904, N'User Admin', NULL, N'Landing Page', 900)
INSERT [dbo].[Resource] ([ResourceID], [ResourceTable], [ResourceKey], [ResourceQualifier], [ResourceTypeID]) VALUES (905, N'User Admin', NULL, N'Company User Define', 900)
INSERT [dbo].[Resource] ([ResourceID], [ResourceTable], [ResourceKey], [ResourceQualifier], [ResourceTypeID]) VALUES (906, N'User Admin', NULL, N'Security Groups', 900)
INSERT [dbo].[Resource] ([ResourceID], [ResourceTable], [ResourceKey], [ResourceQualifier], [ResourceTypeID]) VALUES (907, N'User Admin', NULL, N'Resources', 900)
INSERT [dbo].[Resource] ([ResourceID], [ResourceTable], [ResourceKey], [ResourceQualifier], [ResourceTypeID]) VALUES (908, N'User Admin', NULL, N'Licenses', 900)
INSERT [dbo].[Resource] ([ResourceID], [ResourceTable], [ResourceKey], [ResourceQualifier], [ResourceTypeID]) VALUES (950, N'Workflow Managemet', NULL, N'Landing Page', 900)
INSERT [dbo].[Resource] ([ResourceID], [ResourceTable], [ResourceKey], [ResourceQualifier], [ResourceTypeID]) VALUES (1000, N'Workflow', NULL, N'Landing Page', 900)
INSERT [dbo].[SecurityGroupResource] ([SecurityGroupResourceID], [SecurityGroupID], [ResourceID], [Mode], [ResourcePasswordC], [ResourcePasswordR], [ResourcePasswordU], [ResourcePasswordD], [ResourcePasswordP]) VALUES (1, 1, 904, N'CRUD', NULL, NULL, NULL, NULL, NULL)
INSERT [dbo].[SecurityGroupResource] ([SecurityGroupResourceID], [SecurityGroupID], [ResourceID], [Mode], [ResourcePasswordC], [ResourcePasswordR], [ResourcePasswordU], [ResourcePasswordD], [ResourcePasswordP]) VALUES (2, 1, 905, N'CRUD', NULL, NULL, NULL, NULL, NULL)
INSERT [dbo].[SecurityGroupResource] ([SecurityGroupResourceID], [SecurityGroupID], [ResourceID], [Mode], [ResourcePasswordC], [ResourcePasswordR], [ResourcePasswordU], [ResourcePasswordD], [ResourcePasswordP]) VALUES (3, 1, 906, N'CRUD', NULL, NULL, NULL, NULL, NULL)
INSERT [dbo].[SecurityGroupResource] ([SecurityGroupResourceID], [SecurityGroupID], [ResourceID], [Mode], [ResourcePasswordC], [ResourcePasswordR], [ResourcePasswordU], [ResourcePasswordD], [ResourcePasswordP]) VALUES (4, 1, 907, N'CRUD', NULL, NULL, NULL, NULL, NULL)
INSERT [dbo].[SecurityGroupResource] ([SecurityGroupResourceID], [SecurityGroupID], [ResourceID], [Mode], [ResourcePasswordC], [ResourcePasswordR], [ResourcePasswordU], [ResourcePasswordD], [ResourcePasswordP]) VALUES (5, 1, 908, N'CRUD', NULL, NULL, NULL, NULL, NULL)
INSERT [dbo].[SecurityGroupResource] ([SecurityGroupResourceID], [SecurityGroupID], [ResourceID], [Mode], [ResourcePasswordC], [ResourcePasswordR], [ResourcePasswordU], [ResourcePasswordD], [ResourcePasswordP]) VALUES (6, 2, 200, N'CRUD', NULL, NULL, NULL, NULL, NULL)
INSERT [dbo].[SecurityGroupResource] ([SecurityGroupResourceID], [SecurityGroupID], [ResourceID], [Mode], [ResourcePasswordC], [ResourcePasswordR], [ResourcePasswordU], [ResourcePasswordD], [ResourcePasswordP]) VALUES (7, 3, 300, N'CRUD', NULL, NULL, NULL, NULL, NULL)
INSERT [dbo].[SecurityGroupResource] ([SecurityGroupResourceID], [SecurityGroupID], [ResourceID], [Mode], [ResourcePasswordC], [ResourcePasswordR], [ResourcePasswordU], [ResourcePasswordD], [ResourcePasswordP]) VALUES (8, 4, 400, N'CRUD', NULL, NULL, NULL, NULL, NULL)
INSERT [dbo].[SecurityGroupResource] ([SecurityGroupResourceID], [SecurityGroupID], [ResourceID], [Mode], [ResourcePasswordC], [ResourcePasswordR], [ResourcePasswordU], [ResourcePasswordD], [ResourcePasswordP]) VALUES (9, 5, 500, N'CRUD', NULL, NULL, NULL, NULL, NULL)
INSERT [dbo].[SecurityGroupResource] ([SecurityGroupResourceID], [SecurityGroupID], [ResourceID], [Mode], [ResourcePasswordC], [ResourcePasswordR], [ResourcePasswordU], [ResourcePasswordD], [ResourcePasswordP]) VALUES (10, 6, 600, N'CRUD', NULL, NULL, NULL, NULL, NULL)
INSERT [dbo].[SecurityGroupResource] ([SecurityGroupResourceID], [SecurityGroupID], [ResourceID], [Mode], [ResourcePasswordC], [ResourcePasswordR], [ResourcePasswordU], [ResourcePasswordD], [ResourcePasswordP]) VALUES (11, 7, 700, N'CRUD', NULL, NULL, NULL, NULL, NULL)
INSERT [dbo].[SecurityGroupResource] ([SecurityGroupResourceID], [SecurityGroupID], [ResourceID], [Mode], [ResourcePasswordC], [ResourcePasswordR], [ResourcePasswordU], [ResourcePasswordD], [ResourcePasswordP]) VALUES (12, 9, 950, N'CRUD', NULL, NULL, NULL, NULL, NULL)
INSERT [dbo].[SecurityGroupResource] ([SecurityGroupResourceID], [SecurityGroupID], [ResourceID], [Mode], [ResourcePasswordC], [ResourcePasswordR], [ResourcePasswordU], [ResourcePasswordD], [ResourcePasswordP]) VALUES (13, 8, 1000, N'CRUD', NULL, NULL, NULL, NULL, NULL)
INSERT [dbo].[SecurityGroupResource] ([SecurityGroupResourceID], [SecurityGroupID], [ResourceID], [Mode], [ResourcePasswordC], [ResourcePasswordR], [ResourcePasswordU], [ResourcePasswordD], [ResourcePasswordP]) VALUES (14, 10, 904, N'R', NULL, NULL, NULL, NULL, NULL)
INSERT [dbo].[SecurityGroupResource] ([SecurityGroupResourceID], [SecurityGroupID], [ResourceID], [Mode], [ResourcePasswordC], [ResourcePasswordR], [ResourcePasswordU], [ResourcePasswordD], [ResourcePasswordP]) VALUES (15, 10, 905, N'R', NULL, NULL, NULL, NULL, NULL)
INSERT [dbo].[SecurityGroupResource] ([SecurityGroupResourceID], [SecurityGroupID], [ResourceID], [Mode], [ResourcePasswordC], [ResourcePasswordR], [ResourcePasswordU], [ResourcePasswordD], [ResourcePasswordP]) VALUES (16, 10, 906, N'R', NULL, NULL, NULL, NULL, NULL)
INSERT [dbo].[SecurityGroupResource] ([SecurityGroupResourceID], [SecurityGroupID], [ResourceID], [Mode], [ResourcePasswordC], [ResourcePasswordR], [ResourcePasswordU], [ResourcePasswordD], [ResourcePasswordP]) VALUES (17, 10, 907, N'R', NULL, NULL, NULL, NULL, NULL)
INSERT [dbo].[SecurityGroupResource] ([SecurityGroupResourceID], [SecurityGroupID], [ResourceID], [Mode], [ResourcePasswordC], [ResourcePasswordR], [ResourcePasswordU], [ResourcePasswordD], [ResourcePasswordP]) VALUES (18, 10, 908, N'R', NULL, NULL, NULL, NULL, NULL)
INSERT [dbo].[Action] ([ActionID], [ActionName], [ActionDesc]) VALUES (1, N'exposure', N'POST exposure files to calc BE. Returns location of exposure data')
INSERT [dbo].[Action] ([ActionID], [ActionName], [ActionDesc]) VALUES (2, N'analysis', N'POST analysis request with analysis parameters and exposure location. Returns queue resource detail')
INSERT [dbo].[Action] ([ActionID], [ActionName], [ActionDesc]) VALUES (3, N'analysis_status', N'GET status of analysis for queue resource. Returns status of analysis and once complete location of outputs')
INSERT [dbo].[Action] ([ActionID], [ActionName], [ActionDesc]) VALUES (4, N'outputs', N'GET output files for location given. Returns files')
INSERT [dbo].[Parameter] ([ParameterID], [ParameterName], [ParameterDesc], [ActionID], [JSONLevel], [ParameterFormat]) VALUES (1, N'exposure_files', N'tarball of exposure files for a process run', 1, NULL, N'int')
INSERT [dbo].[Parameter] ([ParameterID], [ParameterName], [ParameterDesc], [ActionID], [JSONLevel], [ParameterFormat]) VALUES (2, N'prog_id', N'Prog ID of data in Flamingo', 2, 2, N'int')
INSERT [dbo].[Parameter] ([ParameterID], [ParameterName], [ParameterDesc], [ActionID], [JSONLevel], [ParameterFormat]) VALUES (3, N'module_supplier_id', N'Module supplier defines BE calc code to use', 2, 2, N'str')
INSERT [dbo].[Parameter] ([ParameterID], [ParameterName], [ParameterDesc], [ActionID], [JSONLevel], [ParameterFormat]) VALUES (4, N'model_version_id', N'model version number if multiple versions exist', 2, 2, N'str')
INSERT [dbo].[Parameter] ([ParameterID], [ParameterName], [ParameterDesc], [ActionID], [JSONLevel], [ParameterFormat]) VALUES (5, N'number_of_samples', N'number of samples to use in the process run', 2, 2, N'int')
INSERT [dbo].[Parameter] ([ParameterID], [ParameterName], [ParameterDesc], [ActionID], [JSONLevel], [ParameterFormat]) VALUES (6, N'gul_threshold', N'threshold below which ground up losses will be disregarded', 2, 2, N'int')
INSERT [dbo].[Parameter] ([ParameterID], [ParameterName], [ParameterDesc], [ActionID], [JSONLevel], [ParameterFormat]) VALUES (7, N'exposure_location', N'location of exposure files as returned from POST exposures', 2, 2, N'str')
INSERT [dbo].[Parameter] ([ParameterID], [ParameterName], [ParameterDesc], [ActionID], [JSONLevel], [ParameterFormat]) VALUES (8, N'queue_resource', N'Queue resource id of analysis status to be queried', 3, NULL, N'int')
INSERT [dbo].[Parameter] ([ParameterID], [ParameterName], [ParameterDesc], [ActionID], [JSONLevel], [ParameterFormat]) VALUES (9, N'output_files', N'location of output files to be retreived', 4, NULL, N'str')
INSERT [dbo].[Parameter] ([ParameterID], [ParameterName], [ParameterDesc], [ActionID], [JSONLevel], [ParameterFormat]) VALUES (10, N'model_specific_parameter', N'parameter that is specific to the model', 2, 3, N'var')
INSERT [dbo].[Workflow] ([WorkflowID], [WorkflowName]) VALUES (1, N'Run Analysis')
INSERT [dbo].[WorkflowElement] ([WorkflowElementID], [WorkflowElementName], [WorkflowID], [ActionID], [SequenceNumber]) VALUES (1, N'Post Exposure Files', 1, 1, 1)
INSERT [dbo].[WorkflowElement] ([WorkflowElementID], [WorkflowElementName], [WorkflowID], [ActionID], [SequenceNumber]) VALUES (2, N'Post Analysis Request', 1, 2, 2)
INSERT [dbo].[WorkflowElement] ([WorkflowElementID], [WorkflowElementName], [WorkflowID], [ActionID], [SequenceNumber]) VALUES (3, N'Get Analysis Status', 1, 3, 3)
INSERT [dbo].[WorkflowElement] ([WorkflowElementID], [WorkflowElementName], [WorkflowID], [ActionID], [SequenceNumber]) VALUES (4, N'Get Output Files', 1, 4, 4)
INSERT [dbo].[WorkflowParameter] ([WorkflowParameterID], [WorkflowElementID], [ParameterID], [WorkflowParameterValue], [WorkflowParameterSource]) VALUES (1, 1, 1, N'', N'ProgOasis')
INSERT [dbo].[WorkflowParameter] ([WorkflowParameterID], [WorkflowElementID], [ParameterID], [WorkflowParameterValue], [WorkflowParameterSource]) VALUES (2, 2, 2, N'', N'Prog')
INSERT [dbo].[WorkflowParameter] ([WorkflowParameterID], [WorkflowElementID], [ParameterID], [WorkflowParameterValue], [WorkflowParameterSource]) VALUES (3, 2, 3, N'', N'Model')
INSERT [dbo].[WorkflowParameter] ([WorkflowParameterID], [WorkflowElementID], [ParameterID], [WorkflowParameterValue], [WorkflowParameterSource]) VALUES (4, 2, 4, N'', N'User')
INSERT [dbo].[WorkflowParameter] ([WorkflowParameterID], [WorkflowElementID], [ParameterID], [WorkflowParameterValue], [WorkflowParameterSource]) VALUES (5, 2, 5, N'', N'User')
INSERT [dbo].[WorkflowParameter] ([WorkflowParameterID], [WorkflowElementID], [ParameterID], [WorkflowParameterValue], [WorkflowParameterSource]) VALUES (6, 2, 6, N'', N'User')
INSERT [dbo].[WorkflowParameter] ([WorkflowParameterID], [WorkflowElementID], [ParameterID], [WorkflowParameterValue], [WorkflowParameterSource]) VALUES (7, 2, 7, N'1', N'Lookup')
INSERT [dbo].[WorkflowParameter] ([WorkflowParameterID], [WorkflowElementID], [ParameterID], [WorkflowParameterValue], [WorkflowParameterSource]) VALUES (8, 3, 8, N'2', N'Lookup')
INSERT [dbo].[WorkflowParameter] ([WorkflowParameterID], [WorkflowElementID], [ParameterID], [WorkflowParameterValue], [WorkflowParameterSource]) VALUES (9, 4, 9, N'3', N'Lookup')
INSERT [dbo].[WorkflowParameter] ([WorkflowParameterID], [WorkflowElementID], [ParameterID], [WorkflowParameterValue], [WorkflowParameterSource]) VALUES (10, 2, 10, N'', N'User')
INSERT [dbo].[SummaryLevel] ([SummaryLevelID], [SummaryLevelName], [SummaryLevelSequence], [ParameterStub]) VALUES (1, N'Portfolio', 1, N'prog')
INSERT [dbo].[SummaryLevel] ([SummaryLevelID], [SummaryLevelName], [SummaryLevelSequence], [ParameterStub]) VALUES (2, N'Policy', 2, N'policy')
INSERT [dbo].[SummaryLevel] ([SummaryLevelID], [SummaryLevelName], [SummaryLevelSequence], [ParameterStub]) VALUES (3, N'State', 3, N'state')
INSERT [dbo].[SummaryLevel] ([SummaryLevelID], [SummaryLevelName], [SummaryLevelSequence], [ParameterStub]) VALUES (4, N'County', 4, N'county')
INSERT [dbo].[SummaryLevel] ([SummaryLevelID], [SummaryLevelName], [SummaryLevelSequence], [ParameterStub]) VALUES (5, N'Location', 5, N'loc')
INSERT [dbo].[SummaryLevel] ([SummaryLevelID], [SummaryLevelName], [SummaryLevelSequence], [ParameterStub]) VALUES (6, N'LOB', 6, N'lob')
INSERT [dbo].[SummaryLevel] ([SummaryLevelID], [SummaryLevelName], [SummaryLevelSequence], [ParameterStub]) VALUES (7, N'Zip', 7, N'zip')
INSERT [dbo].[Perspective] ([PerspectiveID], [PerspectiveName]) VALUES (1, N'GUL')
INSERT [dbo].[Perspective] ([PerspectiveID], [PerspectiveName]) VALUES (2, N'IL')
INSERT [dbo].[AnalysisType] ([AnalysisTypeID], [AnalysisTypeName], [AnalysisFileNameStub], [JSONLevel], [ParameterStub], [LECFlag]) VALUES (1, N'Samples', N'summarycalc', 2, N'Summary',0)
INSERT [dbo].[AnalysisType] ([AnalysisTypeID], [AnalysisTypeName], [AnalysisFileNameStub], [JSONLevel], [ParameterStub], [LECFlag]) VALUES (2, N'ELT', N'eltcalc', 2, N'ELT',0)
INSERT [dbo].[AnalysisType] ([AnalysisTypeID], [AnalysisTypeName], [AnalysisFileNameStub], [JSONLevel], [ParameterStub], [LECFlag]) VALUES (3, N'LEC Full Uncertainty AEP', N'full_uncertainty_aep', 3, N'FullUncAEP',1)
INSERT [dbo].[AnalysisType] ([AnalysisTypeID], [AnalysisTypeName], [AnalysisFileNameStub], [JSONLevel], [ParameterStub], [LECFlag]) VALUES (4, N'LEC Full Uncertainty OEP', N'full_uncertainty_oep', 3, N'FullUncOEP',1)
INSERT [dbo].[AnalysisType] ([AnalysisTypeID], [AnalysisTypeName], [AnalysisFileNameStub], [JSONLevel], [ParameterStub], [LECFlag]) VALUES (5, N'LEC Wheatsheaf AEP', N'wheatsheaf_aep', 3, N'AEPWheatsheaf',1)
INSERT [dbo].[AnalysisType] ([AnalysisTypeID], [AnalysisTypeName], [AnalysisFileNameStub], [JSONLevel], [ParameterStub], [LECFlag]) VALUES (6, N'LEC Wheatsheaf OEP', N'wheatsheaf_oep', 3, N'OEPWheatsheaf',1)
INSERT [dbo].[AnalysisType] ([AnalysisTypeID], [AnalysisTypeName], [AnalysisFileNameStub], [JSONLevel], [ParameterStub], [LECFlag]) VALUES (7, N'LEC Wheatsheaf Mean AEP', N'wheatsheaf_mean_aep', 3, N'MeanAEPWheatsheaf',1)
INSERT [dbo].[AnalysisType] ([AnalysisTypeID], [AnalysisTypeName], [AnalysisFileNameStub], [JSONLevel], [ParameterStub], [LECFlag]) VALUES (8, N'LEC Wheatsheaf Mean OEP', N'wheatsheaf_mean_oep', 3, N'MeanOEPWheatsheaf',1)
INSERT [dbo].[AnalysisType] ([AnalysisTypeID], [AnalysisTypeName], [AnalysisFileNameStub], [JSONLevel], [ParameterStub], [LECFlag]) VALUES (9, N'LEC Sample Mean AEP', N'sample_mean_aep', 3, N'SampleMeanAEP',1)
INSERT [dbo].[AnalysisType] ([AnalysisTypeID], [AnalysisTypeName], [AnalysisFileNameStub], [JSONLevel], [ParameterStub], [LECFlag]) VALUES (10, N'LEC Sample Mean OEP', N'sample_mean_oep', 3, N'SampleMeanOEP',1)
INSERT [dbo].[AnalysisType] ([AnalysisTypeID], [AnalysisTypeName], [AnalysisFileNameStub], [JSONLevel], [ParameterStub], [LECFlag]) VALUES (11, N'AAL', N'aalcalc', 2, N'AAL',0)
INSERT [dbo].[AnalysisType] ([AnalysisTypeID], [AnalysisTypeName], [AnalysisFileNameStub], [JSONLevel], [ParameterStub], [LECFlag]) VALUES (12, N'PLT', N'pltcalc', 2, N'PLT',0)
INSERT [dbo].[OutputOptions] ([OutputOptionID], [OutputOption], [OutputTypeID]) VALUES (1, N'MyOutputs1', 1)
INSERT [dbo].[OutputOptions] ([OutputOptionID], [OutputOption], [OutputTypeID]) VALUES (2, N'MyOutputs1', 10)
INSERT [dbo].[OutputOptions] ([OutputOptionID], [OutputOption], [OutputTypeID]) VALUES (3, N'MyOutputs1', 90)
INSERT [dbo].[OutputOptions] ([OutputOptionID], [OutputOption], [OutputTypeID]) VALUES (4, N'MyOutputs2', 31)
INSERT [dbo].[OutputOptions] ([OutputOptionID], [OutputOption], [OutputTypeID]) VALUES (5, N'MyOutputs2', 70)
INSERT [dbo].[OutputOptions] ([OutputOptionID], [OutputOption], [OutputTypeID]) VALUES (6, N'MyOutputs2', 40)
INSERT [dbo].[OutputOptions] ([OutputOptionID], [OutputOption], [OutputTypeID]) VALUES (7, N'MyOutputs2', 100)
INSERT [dbo].[OutputOptions] ([OutputOptionID], [OutputOption], [OutputTypeID]) VALUES (8, N'MyOutputs2', 75)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (1, 1, 1, 1)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (2, 1, 2, 1)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (3, 1, 3, 1)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (4, 1, 4, 1)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (5, 1, 5, 1)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (6, 1, 6, 1)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (7, 1, 7, 1)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (8, 1, 8, 1)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (9, 1, 9, 1)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (10, 1, 10, 1)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (11, 1, 11, 1)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (12, 1, 12, 1)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (13, 1, 1, 2)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (14, 1, 2, 2)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (15, 1, 3, 2)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (16, 1, 4, 2)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (17, 1, 5, 2)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (18, 1, 6, 2)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (19, 1, 7, 2)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (20, 1, 8, 2)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (21, 1, 9, 2)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (22, 1, 10, 2)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (23, 1, 11, 2)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (24, 1, 12, 2)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (25, 1, 1, 3)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (26, 1, 2, 3)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (27, 1, 3, 3)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (28, 1, 4, 3)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (29, 1, 5, 3)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (30, 1, 6, 3)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (31, 1, 7, 3)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (32, 1, 8, 3)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (33, 1, 9, 3)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (34, 1, 10, 3)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (35, 1, 11, 3)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (36, 1, 12, 3)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (37, 1, 1, 4)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (38, 1, 2, 4)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (39, 1, 3, 4)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (40, 1, 4, 4)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (41, 1, 5, 4)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (42, 1, 6, 4)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (43, 1, 7, 4)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (44, 1, 8, 4)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (45, 1, 9, 4)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (46, 1, 10, 4)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (47, 1, 11, 4)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (48, 1, 12, 4)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (49, 1, 1, 5)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (50, 1, 2, 5)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (51, 1, 3, 5)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (52, 1, 4, 5)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (53, 1, 5, 5)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (54, 1, 6, 5)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (55, 1, 7, 5)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (56, 1, 8, 5)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (57, 1, 9, 5)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (58, 1, 10, 5)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (59, 1, 11, 5)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (60, 1, 12, 5)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (61, 1, 1, 6)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (62, 1, 2, 6)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (63, 1, 3, 6)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (64, 1, 4, 6)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (65, 1, 5, 6)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (66, 1, 6, 6)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (67, 1, 7, 6)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (68, 1, 8, 6)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (69, 1, 9, 6)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (70, 1, 10, 6)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (71, 1, 11, 6)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (72, 1, 12, 6)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (73, 2, 1, 1)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (74, 2, 2, 1)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (75, 2, 3, 1)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (76, 2, 4, 1)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (77, 2, 5, 1)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (78, 2, 6, 1)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (79, 2, 7, 1)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (80, 2, 8, 1)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (81, 2, 9, 1)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (82, 2, 10, 1)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (83, 2, 11, 1)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (84, 2, 12, 1)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (85, 2, 1, 2)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (86, 2, 2, 2)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (87, 2, 3, 2)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (88, 2, 4, 2)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (89, 2, 5, 2)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (90, 2, 6, 2)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (91, 2, 7, 2)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (92, 2, 8, 2)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (93, 2, 9, 2)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (94, 2, 10, 2)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (95, 2, 11, 2)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (96, 2, 12, 2)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (97, 2, 1, 3)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (98, 2, 2, 3)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (99, 2, 3, 3)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (100, 2, 4, 3)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (101, 2, 5, 3)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (102, 2, 6, 3)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (103, 2, 7, 3)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (104, 2, 8, 3)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (105, 2, 9, 3)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (106, 2, 10, 3)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (107, 2, 11, 3)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (108, 2, 12, 3)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (109, 2, 1, 4)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (110, 2, 2, 4)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (111, 2, 3, 4)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (112, 2, 4, 4)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (113, 2, 5, 4)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (114, 2, 6, 4)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (115, 2, 7, 4)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (116, 2, 8, 4)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (117, 2, 9, 4)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (118, 2, 10, 4)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (119, 2, 11, 4)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (120, 2, 12, 4)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (121, 2, 1, 5)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (122, 2, 2, 5)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (123, 2, 3, 5)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (124, 2, 4, 5)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (125, 2, 5, 5)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (126, 2, 6, 5)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (127, 2, 7, 5)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (128, 2, 8, 5)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (129, 2, 9, 5)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (130, 2, 10, 5)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (131, 2, 11, 5)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (132, 2, 12, 5)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (133, 2, 1, 6)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (134, 2, 2, 6)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (135, 2, 3, 6)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (136, 2, 4, 6)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (137, 2, 5, 6)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (138, 2, 6, 6)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (139, 2, 7, 6)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (140, 2, 8, 6)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (141, 2, 9, 6)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (142, 2, 10, 6)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (143, 2, 11, 6)
INSERT [dbo].[OutputType] ([OutputTypeID], [PerspectiveID], [AnalysisTypeID], [SummaryLevelID]) VALUES (144, 2, 12, 6)
INSERT [dbo].[CoverageType] ([CoverageTypeID], [CoverageTypeName], [CoverageTypeDesc]) VALUES (0, N'ALL', N'All Coverages')
INSERT [dbo].[CoverageType] ([CoverageTypeID], [CoverageTypeName], [CoverageTypeDesc]) VALUES (1, N'Buildings', N'Buildings')
INSERT [dbo].[CoverageType] ([CoverageTypeID], [CoverageTypeName], [CoverageTypeDesc]) VALUES (2, N'Other', N'Other Structures')
INSERT [dbo].[CoverageType] ([CoverageTypeID], [CoverageTypeName], [CoverageTypeDesc]) VALUES (3, N'Contents', N'Contents')
INSERT [dbo].[CoverageType] ([CoverageTypeID], [CoverageTypeName], [CoverageTypeDesc]) VALUES (4, N'Time', N'Business Interuption')
INSERT [dbo].[FileType] ([FileTypeId], [FileTypeDesc]) VALUES (101, N'Source Location File')
INSERT [dbo].[FileType] ([FileTypeId], [FileTypeDesc]) VALUES (102, N'Source Account File')
INSERT [dbo].[FileType] ([FileTypeId], [FileTypeDesc]) VALUES (103, N'Canonical Location File')
INSERT [dbo].[FileType] ([FileTypeId], [FileTypeDesc]) VALUES (104, N'Canonical Account File')
INSERT [dbo].[FileType] ([FileTypeId], [FileTypeDesc]) VALUES (105, N'Lookup Location File')
INSERT [dbo].[FileType] ([FileTypeId], [FileTypeDesc]) VALUES (106, N'Lookup Return Key File')
INSERT [dbo].[FileType] ([FileTypeId], [FileTypeDesc]) VALUES (107, N'Lookup Return Non Match File')
INSERT [dbo].[FileType] ([FileTypeId], [FileTypeDesc]) VALUES (108, N'Lookup Return Error File')
INSERT [dbo].[FileType] ([FileTypeId], [FileTypeDesc]) VALUES (109, N'XSD Validation File')
INSERT [dbo].[FileType] ([FileTypeId], [FileTypeDesc]) VALUES (110, N'XSLT Transformation File')
INSERT [dbo].[FileType] ([FileTypeId], [FileTypeDesc]) VALUES (111, N'Oasis Items File')
INSERT [dbo].[FileType] ([FileTypeId], [FileTypeDesc]) VALUES (112, N'Oasis Coverages File')
INSERT [dbo].[FileType] ([FileTypeId], [FileTypeDesc]) VALUES (113, N'Oasis Item Dictionary File')
INSERT [dbo].[FileType] ([FileTypeId], [FileTypeDesc]) VALUES (114, N'Oasis FM Programme File')
INSERT [dbo].[FileType] ([FileTypeId], [FileTypeDesc]) VALUES (115, N'Oasis FM Policy TC File')
INSERT [dbo].[FileType] ([FileTypeId], [FileTypeDesc]) VALUES (116, N'Oasis FM Policy Profile File')
INSERT [dbo].[FileType] ([FileTypeId], [FileTypeDesc]) VALUES (117, N'Oasis FM X Ref File')
INSERT [dbo].[FileType] ([FileTypeId], [FileTypeDesc]) VALUES (118, N'Oasis GUL Summary Xref File')
INSERT [dbo].[FileType] ([FileTypeId], [FileTypeDesc]) VALUES (119, N'Oasis IL Summary Xref File')
INSERT [dbo].[FileType] ([FileTypeId], [FileTypeDesc]) VALUES (120, N'Oasis FM Dictionary File')
INSERT [dbo].[FileType] ([FileTypeId], [FileTypeDesc]) VALUES (121, N'Oasis Output File')
declare @FileLocationSQL nvarchar(255)		= (SELECT [VariableName] FROM #TempVariables WHERE [VariableType] = 'FileLocationSQL')
declare @FileLocationShiny nvarchar(255)		= (SELECT [VariableName] FROM #TempVariables WHERE [VariableType] = 'FileLocationShiny')
INSERT 
	[dbo].[Location] ([LocationID], [LocationName], [LocationDesc], [LocationPathUnix])
VALUES 
	(101, @FileLocationSQL+'\APIInput', N'Input Files for API', @FileLocationShiny+'/APIInput')
INSERT
	[dbo].[Location] ([LocationID], [LocationName], [LocationDesc], [LocationPathUnix]) 
VALUES 
	(102, @FileLocationSQL+'\APIOutput', N'Output Files for API', @FileLocationShiny+'/APIOutput')
INSERT 
	[dbo].[Location] ([LocationID], [LocationName], [LocationDesc], [LocationPathUnix]) 
VALUES
	(103, @FileLocationSQL+'\Exposures', N'for exposures files', @FileLocationShiny+'/Exposures')
INSERT 
	[dbo].[Location] ([LocationID], [LocationName], [LocationDesc], [LocationPathUnix]) 
VALUES 
	(104, @FileLocationSQL+'\OasisFiles', N'Share Folder on FE SQL Server', @FileLocationShiny+'/OasisFiles')
INSERT 
	[dbo].[Location] ([LocationID], [LocationName], [LocationDesc], [LocationPathUnix]) 
VALUES
	(105, @FileLocationSQL+'\TransformationFiles', N'XSLT Files', @FileLocationShiny+'/TransformationFiles')
INSERT 
	[dbo].[Location] ([LocationID], [LocationName], [LocationDesc], [LocationPathUnix]) 
VALUES 
	(106, @FileLocationSQL+'\ValidationFiles', N'XSD Files', @FileLocationShiny+'/ValidationFiles')
INSERT [dbo].[OperationFileType] ([OperationFileTypeID], [OperationID], [FileTypeID]) VALUES (1, 1001, 101)
INSERT [dbo].[OperationFileType] ([OperationFileTypeID], [OperationID], [FileTypeID]) VALUES (2, 1001, 103)
INSERT [dbo].[OperationOperationID] ([OperationID], [Operation]) VALUES (1000, N'FO_btn_show_raw_content')
INSERT [dbo].[OperationOperationID] ([OperationID], [Operation]) VALUES (1001, N'FO_btn_show_map')
INSERT [dbo].[Peril] ([PerilId], [PerilCode], [PerilDesc]) VALUES (0, N'A', N'All Perils')
INSERT [dbo].[Peril] ([PerilId], [PerilCode], [PerilDesc]) VALUES (1, N'W', N'Wind')
INSERT [dbo].[Peril] ([PerilId], [PerilCode], [PerilDesc]) VALUES (2, N'S', N'Storm Surge')
INSERT [dbo].[Peril] ([PerilId], [PerilCode], [PerilDesc]) VALUES (3, N'Q', N'Earthquake')
INSERT [dbo].[Peril] ([PerilId], [PerilCode], [PerilDesc]) VALUES (4, N'F', N'Flood')
INSERT [dbo].[Peril] ([PerilId], [PerilCode], [PerilDesc]) VALUES (5, N'X', N'Fire')
INSERT [dbo].[PolicyType] ([PolicyTypeID], [PolicyTypeName]) VALUES (0, N'Standard Policy')
INSERT [dbo].[ServiceType] ([ServiceTypeID], [ServiceTypeName], [ServiceDesc]) VALUES (1, N'Oasis Mid Tier', NULL)
INSERT [dbo].[ServiceType] ([ServiceTypeID], [ServiceTypeName], [ServiceDesc]) VALUES (2, N'API Lookup Servcie', NULL)
INSERT [dbo].[Source] ([SourceId], [SourceName], [SourceDesc]) VALUES (1, N'Sys', N'System')
INSERT [dbo].[Source] ([SourceId], [SourceName], [SourceDesc]) VALUES (2, N'FileUpload', N'File Upload Functionality')
INSERT [dbo].[Table] ([TableID], [TableName]) VALUES (1, N'Correlation')
INSERT [dbo].[Table] ([TableID], [TableName]) VALUES (2, N'CorrelationItem')
INSERT [dbo].[Table] ([TableID], [TableName]) VALUES (3, N'CoverageItem')
INSERT [dbo].[Table] ([TableID], [TableName]) VALUES (4, N'InterestExposure')
INSERT [dbo].[Table] ([TableID], [TableName]) VALUES (5, N'InterestExposureValues')
INSERT [dbo].[Table] ([TableID], [TableName]) VALUES (6, N'InterestGroup')
INSERT [dbo].[Table] ([TableID], [TableName]) VALUES (7, N'InterestGroupValues')
INSERT [dbo].[Table] ([TableID], [TableName]) VALUES (8, N'InterestRisk')
INSERT [dbo].[Table] ([TableID], [TableName]) VALUES (9, N'InterestRiskValues')
INSERT [dbo].[Table] ([TableID], [TableName]) VALUES (10, N'InterestSubRisk')
INSERT [dbo].[Table] ([TableID], [TableName]) VALUES (11, N'InterestSubRiskValues')
INSERT [dbo].[Table] ([TableID], [TableName]) VALUES (12, N'Policy')
INSERT [dbo].[Table] ([TableID], [TableName]) VALUES (13, N'PolicyCoverage')
INSERT [dbo].[Table] ([TableID], [TableName]) VALUES (14, N'PolicyCoverageValues')
INSERT [dbo].[Table] ([TableID], [TableName]) VALUES (15, N'PolicyPeril')
INSERT [dbo].[Table] ([TableID], [TableName]) VALUES (16, N'PolicyValues')
INSERT [dbo].[Table] ([TableID], [TableName]) VALUES (17, N'Schedule')
INSERT [dbo].[Table] ([TableID], [TableName]) VALUES (18, N'ScheduleValues')
INSERT [dbo].[Table] ([TableID], [TableName]) VALUES (19, N'PolicyLayer')
INSERT [dbo].[Table] ([TableID], [TableName]) VALUES (20, N'PolicyLayerValues')
INSERT [dbo].[ModelType] ([ModelTypeID], [ModelTypeName]) VALUES (1, N'Standard Model')
INSERT [dbo].[ModelType] ([ModelTypeID], [ModelTypeName]) VALUES (2, N'Mod 8 Item ID Model')
DECLARE	@EnvironmentName nvarchar(255)	= (SELECT [VariableName] FROM #TempVariables WHERE [VariableType] = 'EnvironmentName')
DECLARE	@Version nvarchar(255)			= (SELECT [VariableName] FROM #TempVariables WHERE [VariableType] = 'Version')
INSERT [dbo].[Version] ([Type], [Version], [Tag]) VALUES ('FLAMINGO', @Version, @EnvironmentName)
INSERT [dbo].[TransformType] ([TransformTypeID], [TransformTypeName], [TransformTypeDesc]) VALUES (1, N'SourceToCan', N'Source to Canonical File Transformation')
INSERT [dbo].[TransformType] ([TransformTypeID], [TransformTypeName], [TransformTypeDesc]) VALUES (2, N'CanToModel', N'Canonical to Model File Transformation')
GO
