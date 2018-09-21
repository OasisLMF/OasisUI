# (c) 2013-2016 Oasis LMF Ltd.
import logging

from  oasislmf.utils import db 
from oasislmf.utils.log import oasis_log


@oasis_log()
def load_programme_data(progid):
    db.execute(
        "exec dbo.LoadProgrammeData ?", progid)

@oasis_log()
def update_prog_status(progid, status):
    db.execute(
        "exec dbo.updateProgStatus ?, ?", progid, status)

@oasis_log()
def update_progoasis_status(progoasisid, status):
    db.execute(
        "exec dbo.updateProgOasisStatus ?, ?", progoasisid, status)



@oasis_log()
def get_transform_file_names_prog(progid):
    return db.fetch_one(
        "exec dbo.getTransformInputFilesForProg ?", progid)

@oasis_log()
def get_transform_file_names_progoasis(progoasisid):
    return db.fetch_one(
        "exec dbo.getTransformInputFilesForProgOasis ?", progoasisid)

@oasis_log()
def get_source_loc_file_for_prog(progid):
    return db.fetch_one(
        "exec dbo.getSourceFileLocForProg ?", progid)

@oasis_log()
def get_source_acc_file_for_prog(progid):
    return db.fetch_one(
        "exec dbo.getSourceFileAccForProg ?", progid)

@oasis_log()
def get_source_reinsurance_file_for_prog(progid):
    return db.fetch_one(
        "exec dbo.getSourceFileReinsuranceForProg ?", progid)

@oasis_log()
def get_source_reinsurance_scope_file_for_prog(progid):
    return db.fetch_one(
        "exec dbo.getSourceFileReinsuranceScopeForProg ?", progid)

@oasis_log()
def get_ProgId_For_ProgOasis(progoasisid):
    return db.fetch_one(
        "exec dbo.getProgIdForProgOasis ?", progoasisid)

@oasis_log()
def generate_output_transform_file_records_for_prog(progid, canlocfilename, canaccfilename):
    db.execute(
        "exec dbo.generateOutputTransformFileRecordsForProg ?, ?, ?",
        progid, canlocfilename, canaccfilename)


@oasis_log()
def generate_output_transform_file_records_for_progoasis(progoasisid, modelfilename):
    db.execute(
        "exec dbo.generateOutputTransformFileRecordsForProgOasos ?, ?",
        progoasisid, modelfilename)


@oasis_log()
def get_profile_details(progid):
    return db.fetchall(
        "exec dbo.getProfileDetailsForIni ?", progid)


@oasis_log()
def generate_canonical_model(progid):
    db.execute(
        "exec dbo.generateCanonicalModel ?", progid)


@oasis_log()
def get_model_file_extension(progoasisid):
    extension =  db.fetch_one(
            "exec dbo.getModelFileExtension ?", progoasisid)
    return extension


@oasis_log()
def load_programme_model_details(progoasisid):
    status =  db.fetch_one(
            "exec dbo.LoadProgrammeModelData ?", progoasisid)
    return status


@oasis_log()
def get_element_run_ids(processrunid):
    return db.fetchall(
        """
        SELECT
            ElementRunID
        FROM
            ElementRun
        WHERE
            ProcessRunID = ?
        ORDER BY
            WorkflowElementID asc
        """, processrunid)


@oasis_log()
def generate_oasis_files_outputs(processrunid):
    db.execute(
        "exec dbo.generateOasisFilesOutputs ?", processrunid)


@oasis_log()
def get_prog_oasis_location(processrunid):
    return db.fetch_one(
        "exec dbo.getLocationNameForProcessRunId ?", processrunid)[0]


@oasis_log()
def get_process_run_locationid(
    prog_oasis_location, process_dir, processrunid):

    return db.fetch_one(
        "exec dbo.generateLocationRecord ?, ?, ?",
        prog_oasis_location, process_dir, "ProcessRun_{}".format(processrunid))[0]


@oasis_log()
def generate_oasis_files_records_outputs(
        processrunid, process_run_locationid):
    db.execute(
        """
        exec dbo.generateOasisFilesRecordsOutputs 
            @ProcessRunId = ?, 
            @LocationID = ?,
            @GulSummaryXrefFileName = ?,
            @FMSummaryXrefFileName = ?
        """,
        processrunid, process_run_locationid, 
        'gulsummaryxref.csv' , 'fmsummaryxref.csv')


@oasis_log()
def get_general_settings(processrunid):
    return db.fetchall(
        "exec dbo.getParametersJSONGeneral ?", processrunid)


@oasis_log()
def get_model_settings(processrunid):
    return db.fetchall(
        "exec dbo.getParametersJSONModel ?", processrunid)


@oasis_log()
def get_gul_summaries(processrunid):
    return db.fetchall(
        "exec dbo.getParametersJSONSummariesGUL ?", processrunid)


@oasis_log()
def get_il_summaries(processrunid): 
    return db.fetchall(
        "exec dbo.getParametersJSONSummariesIL ?", processrunid)


@oasis_log()
def get_ri_summaries(processrunid): 
    return db.fetchall(
        "exec dbo.getParametersJSONSummariesRI ?", processrunid)

@oasis_log()
def log_element_run_to_db(element_run_id, status, description):

    db.execute(
        """
        exec dbo.LogElementRun
            @ElementRunID = ?,
            @Status = ?,
            @Description = ?
        """,
        element_run_id, status, description)


@oasis_log()
def update_process_run_status(
        processrunid, status):
    db.execute(
        """
        UPDATE
            dbo.ProcessRun
        SET
            ProcessRunStatus = ?
        WHERE
            ProcessRunID = ?
        """, status, processrunid)


@oasis_log()
def get_api_return_data(progoasisid, filename, sessionid):
    db.execute(
        "exec getAPI1aReturnData ?, ?, ?",
        progoasisid, filename, sessionid)


@oasis_log()
def create_api_error_file_record(filename, progoasisid):
    db.execute(
        "exec createAPIErrorFileRecord ?, ?",
        filename, progoasisid)


@oasis_log()
def get_prog_oasis_details(progoasisid):
    (progid, modelid, fileid) = db.fetch_one(
        "exec dbo.getProgOasisDetails ?", progoasisid)
    logging.getLogger().debug(
        "Prog ID={}; Model ID = {}; File ID = {}".format(
            progid, modelid, fileid))
    if progid is None or modelid is None or fileid is None:
        raise Exception("Failed to get ProgOasis details")
    return (progid, modelid, fileid)


@oasis_log()
def get_api_uri_and_systemname(modelid):
    (targetURI, systemname) = db.fetch_one(
        "exec dbo.getAPIURL ?", modelid)
    if targetURI is None or systemname is None:
        raise Exception("Failed to get API URL")    
    return (targetURI, systemname)


@oasis_log()
def get_filename(file_id):
    (filelocation, filename) = db.fetch_one(
        """
        SELECT
            L.LocationPathUnix AS LocationName,
            F.FileName
        FROM
            [dbo].[File] as F,
            Location as L
        WHERE
            L.LocationID = F.LocationID
        AND
            F.FileId = ?
        """, file_id)
    return filelocation + "/" + filename


@oasis_log()
def generate_oasis_files(progoasisid):
    status = db.fetch_one(
        "exec dbo.generateOasisFiles2 ?", progoasisid)[0]
    return status


@oasis_log()
def generate_location_record(
        oasis_files_directory, progoasis_directory, filename):

    location_id = db.fetch_one(
        "exec dbo.generateLocationRecord ?, ?, ?",
        oasis_files_directory, progoasis_directory, filename)
    return location_id


@oasis_log()
def generate_oasis_file_records(progoasisid, location_id):
    db.execute(
        """
        exec dbo.generateOasisFilesRecords 
            @ProgOasisId = ?,
            @LocationID = ?, 
            @ItemsFileName = 'items.csv', 
            @CoveragesFileName = 'coverages.csv',
            @ItemDictFileName = 'ItemDict.csv',
            @FMProgrammeFileName = 'fm_programme.csv',
            @FMPolicyTCFileName = 'fm_policytc.csv',
            @FMProfileFileName = 'fm_profile.csv',
            @FMXRefFileName = 'fm_xref.csv',
            @FMDictFileName = 'FMDict.csv'
        """, progoasisid, location_id)


@oasis_log()
def get_base_url(runid):
    base_url = db.fetch_one("exec dbo.getOasisURL ?", runid)[0]
    logging.getLogger().debug("Base URL = {}".format(base_url))
    return base_url

@oasis_log()
def get_output_file_details(processrunid):
    return db.fetchall(
        """
        select  lower(p.perspectivename) 
                    + '_S' 
                    + convert(nvarchar,[or].outputid) 
                    + '_' 
                    + case when at.LECFlag = 1 then 'leccalc_' else '' end 
                    + at.AnalysisFileNameStub 
                    + '.csv' as [FileName],
                'Output ' 
                    + SummaryLevelName 
                    + ' Level ' 
                    + PerspectiveName 
                    + ' ' 
                    + AnalysisTypeName AS [FileDesc],
                p.PerspectiveName AS [PerspectiveName],
                convert(nvarchar,[or].OutputID) AS [OutputID],
                convert(nvarchar,LECFlag) AS [LECFlag],
                at.AnalysisFileNameStub AS [AnalysisFileNameStub],
                SummaryLevelName AS [SummaryLevelName]
        from    processrun as pr
        join    elementrun as er on er.ProcessRunID = pr.ProcessRunID
        join    OutputRun as [or] on er.ElementRunID = [or].ElementRunID
        join    OutputType as ot on [or].OutputTypeID = ot.OutputTypeID
        join    AnalysisType as at on ot.AnalysisTypeID = at.AnalysisTypeID
        join    perspective as p on ot.PerspectiveID = p.PerspectiveID
        join    summarylevel as sl on ot.SummaryLevelID = sl.SummaryLevelID
        where   pr.ProcessRunID = ?
        """, processrunid)
