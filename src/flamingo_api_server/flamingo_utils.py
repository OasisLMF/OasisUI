# (c) 2013-2016 Oasis LMF Ltd.

# Python 2 standard library imports
import csv
import io
import json
import logging
import os
import re
import shutil
import subprocess
import sys
import time

# Python 2 3rd party imports
import jsonpickle
import requests
import pandas as pd

# Oasis imports
from oasislmf.api_client.client import OasisAPIClient
from oasislmf.exposures.csv_trans import Translator
from oasislmf.exposures.oed import OedValidator
from oasislmf.exposures.oed import load_oed_dfs
from oasislmf.exposures.reinsurance_layer import generate_files_for_reinsurance
from oasislmf.utils import status as status_code
from oasislmf.utils import (
    log,
    db,
    http,
    values,
)

import flamingo_db_utils


ENVIRONMENT = ""
FILES_DIRECTORY = ""
OASIS_FILES_DIRECTORY = ""
# File handling needs to be different if the host
# filesystem is Windows e.g. Symlinking doesn't work.
IS_WINDOWS_HOST = False

def _check_file(filename):
    """
    Raise an exception if the specified file doe not exists
    """
    if not os.path.exists(filename):
        raise Exception("File does not exist: {}".format(filename))
        return False
    else:
        return True

def _do_transform(
    progid, sourcefilename, validationfilelocname, transformationfilename,
    destinationfilepath, destinationfilename, do_sequence):
    '''
    Transform source to canonical.
    '''

    _check_file(sourcefilename)
    _check_file(validationfilelocname)
    _check_file(transformationfilename)

    destinationfilename = os.path.join(destinationfilepath, destinationfilename)

    # Pass logger? check in docker logs
    translator = Translator(
        input_path=sourcefilename,
        output_path=destinationfilename,
        xsd_path=validationfilelocname,
        xslt_path=transformationfilename,
        append_row_nums=do_sequence,
        logger=logging.getLogger()
    )
    translator()
    return _check_file(destinationfilename)


@log.oasis_log()
def transform_source_to_canonical(
    progid, sourcefilename, validationfilelocname, transformationfilename,
    destinationfilepath, destinationfilename):
    '''
    Transform source to canonical.
    '''
    do_sequence = True
    trasform_status = _do_transform(
        progid, sourcefilename, validationfilelocname, transformationfilename,
    destinationfilepath, destinationfilename, do_sequence)
    if trasform_status is False:
        flamingo_db_utils.update_prog_status(progid, "Failed")



@log.oasis_log()
def transform_canonical_to_model(
    progoasisid, sourcefilename, validationfilelocname, transformationfilename,
    destinationfilepath, destinationfilename):
    '''
    Transform canonical to model.
    '''
    do_sequence = False
    trasform_status = _do_transform(
        progoasisid, sourcefilename, validationfilelocname, transformationfilename,
    destinationfilepath, destinationfilename, do_sequence)
    if trasform_status is False:
        flamingo_db_utils.update_progoasis_status(progoasisid, "Failed")

@log.oasis_log()
def do_load_programme_data(progid):
    '''
    Load programme data. This populates the canonical model
    from the exposure files.
    '''
    try:
        row = flamingo_db_utils.get_transform_file_names_prog(progid)
        ts = values.get_timestamp()
        dest_file_path = str(FILES_DIRECTORY) + "/Exposures/"
        canonicallocfilename = "CanLocProg" + str(progid) + "_" + str(ts) + ".csv"
        transform_source_to_canonical(
            progid, row[0], row[1], row[2],
            dest_file_path, canonicallocfilename)
        ts = values.get_timestamp()
        canonicalaccfilename = "CanAccProg" + str(progid) + "_" + str(ts) + ".csv"
        transform_source_to_canonical(
            progid, row[3], row[4], row[5],
            dest_file_path, canonicalaccfilename)
        flamingo_db_utils.generate_output_transform_file_records_for_prog(
            progid, canonicallocfilename, canonicalaccfilename)

        rows = flamingo_db_utils.get_profile_details(progid)
        schema_filepath = FILES_DIRECTORY +  "/Exposures/Schema.ini"
        with io.open(schema_filepath, "w", encoding='utf-8') as schema_file:
            for line in rows:
                schema_file.write(str(line[0]) + "\t\n")

        flamingo_db_utils.generate_canonical_model(progid)
    except:
        logging.getLogger().exception("Error in do_load_programme_data")
        flamingo_db_utils.update_prog_status(progid, "Failed")


@log.oasis_log()
def do_load_programme_model(progoasisid):
    '''
    Load programme model.
    '''
    try:
        row = flamingo_db_utils.get_transform_file_names_progoasis(progoasisid)
        extension = flamingo_db_utils.get_model_file_extension(progoasisid)
        ts = values.get_timestamp()
        dest_file_path = str(FILES_DIRECTORY) + "/APIInput/"
        destinationfile = "ModelLocProgOasis" + str(progoasisid) + "_" + str(ts) + "." + str(extension[0])
        transform_canonical_to_model(
            progoasisid, row[0], row[1], row[2], dest_file_path, destinationfile)

        flamingo_db_utils.generate_output_transform_file_records_for_progoasis(progoasisid, destinationfile)

        do_call_keys_service(progoasisid)
        do_generate_oasis_files(progoasisid)

    except:
        logging.getLogger().exception("Error in do_load_programme_model")
        flamingo_db_utils.update_progoasis_status(progoasisid, "Failed")


@log.oasis_log()
def do_run_prog_oasis(processrunid):
    '''
    Run a programme model combination.
    '''

    element_run_ids = list()
    element_run_id = -1

    flamingo_db_utils.update_process_run_status(
        processrunid, "In Progress")

    try:
        base_url = flamingo_db_utils.get_base_url(processrunid)
        element_run_ids = \
            flamingo_db_utils.get_element_run_ids(processrunid)

        upload_directory = generate_summary_files(processrunid)
        logging.getLogger().debug(
            "Upload_directory: {}".format(upload_directory))

        analysis_settings_json = get_analysis_settings_json(processrunid)
        logging.getLogger().debug(analysis_settings_json)

        if 'il_output' in analysis_settings_json['analysis_settings']:
            create_il_bins = analysis_settings_json['analysis_settings']['il_output']
        else:
            create_il_bins = False
        if 'ri_output' in analysis_settings_json['analysis_settings']:
            create_ri_bins = analysis_settings_json['analysis_settings']['ri_output']
            create_il_bins = True
        else:
            create_ri_bins = False
        logging.getLogger().debug(
            "create_il_bins: {}, create_ri_bins: {}".format(create_il_bins,create_ri_bins))

        analysis_poll_interval_in_seconds = 5
        client = OasisAPIClient(base_url, logging.getLogger())

        element_run_id = element_run_ids[0][0]
        input_location = client.upload_inputs_from_directory(
            upload_directory,
            do_il=create_il_bins,
            do_ri=create_ri_bins,
            do_build=True,
            do_clean=True)
        logging.getLogger().info(
            "Input location: {}".format(input_location))

        flamingo_db_utils.log_element_run_to_db(
            element_run_id, "Success", "Exposure files location: {}".format(input_location))

        element_run_id = element_run_ids[1][0]
        analysis_status_location = client.run_analysis(
            analysis_settings_json, input_location)

        flamingo_db_utils.log_element_run_to_db(
            element_run_id, "Success", "Started analysis")

        element_run_id = element_run_ids[2][0]
        outputs_location = ""
        while True:
            logging.getLogger().debug(
                "Polling analysis status for: {}".format(analysis_status_location))
            (status, outputs_location) = \
                client.get_analysis_status(analysis_status_location)
            flamingo_db_utils.log_element_run_to_db(element_run_id, status, "In Progress")

            if status == status_code.STATUS_SUCCESS:
                if outputs_location is None:
                    raise Exception("Complete but no outputs location")
                flamingo_db_utils.log_element_run_to_db(
                    element_run_id, status, "Analysis Completed")
                break
            elif status == status_code.STATUS_FAILURE:
                error_message = "Analysis failed: {}".format(message)
                logging.getLogger().error(error_message)
                raise Exception(error_message)
            time.sleep(analysis_poll_interval_in_seconds)
        element_run_id = element_run_ids[3][0]

        # download outputs and cleanup
        outputs_file = os.path.join(
            upload_directory,
            outputs_location + ".tar.gz"
        )
        client.download_outputs(outputs_location, outputs_file)
        client.delete_exposure(input_location)
        client.delete_outputs(outputs_location)
        flamingo_db_utils.log_element_run_to_db(
            element_run_id, 'Success', 'Downloaded output files successfully')

        extract_tarball(
            os.path.join(upload_directory, outputs_location + ".tar.gz"),
            upload_directory)

        output_file_list = ','.join(map(str, os.listdir(os.path.join(upload_directory, "output"))))
        logging.getLogger().debug("Output_file_list: {}".format(output_file_list))
        db.execute(
            "exec dbo.linkOutputFileToProcessRun @ProcessRunId = ?, @OutputFiles = ?",
            processrunid, output_file_list)
        flamingo_db_utils.update_process_run_status(processrunid, 'Completed')

        # append summary id meanings to output files
        output_file_details = flamingo_db_utils.get_output_file_details(processrunid)
        logging.getLogger().debug("output_file_details: {}".format(output_file_details))
        columns = ["FileName","FileDesc","PerspectiveName","OutputID","LECFlag","AnalysisFileNameStub","SummaryLevelName"]
        df_output_file_details = pd.DataFrame(columns=columns)
        recs = map(lambda tup: dict(zip(columns, list(tup))), output_file_details)
        df_output_file_details = df_output_file_details.append(recs)
        logging.getLogger().debug("df_output_file_details:\n{}".format(df_output_file_details))

        prog_oasis_location = flamingo_db_utils.get_prog_oasis_location(processrunid)
        itemdict = prog_oasis_location + '/ItemDict.csv'
        fmdict = prog_oasis_location + '/FMDict.csv'
        df_itemdict = pd.read_csv(itemdict)
        df_fmdict = pd.read_csv(fmdict)
        df_fmdict["policy_layer"] = df_fmdict["policy_name"].map(str) + '--' + df_fmdict["layer_name"].map(str)

        for index, row in df_output_file_details.iterrows():
            output = upload_directory + '/output/' + row['FileName']
            logging.getLogger().debug("FileName: {}".format(output))
            output_tmp = output + '.tmp'
            SummaryLevelName = row['SummaryLevelName']
            df_output = pd.read_csv(output)
            SummaryLevelId = SummaryLevelName.lower() + '_id'
            SummaryLevelDesc = SummaryLevelName.lower() + '_desc'
            logging.getLogger().debug("SummaryLevelName: {}".format(SummaryLevelName))
            if SummaryLevelName != "Portfolio":
                if SummaryLevelName == "Policy":
                    # join fmdict to file
                    df_summarydict = df_fmdict[['agg_id','policy_layer']]
                    logging.getLogger().debug("df_summarydict: {}".format(df_summarydict))
                    df_summarydict_distinct = df_summarydict.drop_duplicates()
                    df_output_temp = df_output.join(df_summarydict_distinct.set_index('agg_id'), on='summary_id')
                else:
                    # join itemdict to file
                    df_summarydict = df_itemdict[[SummaryLevelId,SummaryLevelDesc]]
                    df_summarydict_distinct = df_summarydict.drop_duplicates()
                    df_output_temp = df_output.join(df_summarydict_distinct.set_index(SummaryLevelId), on='summary_id')
                    logging.getLogger().debug("df_summarydict_distinct: {}".format(df_summarydict_distinct))
                df_output_temp.to_csv(output, encoding='utf-8', index=False)


    except Exception as e:
        flamingo_db_utils.update_process_run_status(processrunid, "Failed")
        if element_run_id != -1:
            flamingo_db_utils.log_element_run_to_db(element_run_id, 'Failed: ', str(e))
        logging.getLogger().exception(
            "Failed to run prog oasis: {}".format(processrunid))


@log.oasis_log()
def generate_summary_files(processrunid):

    flamingo_db_utils.generate_oasis_files_outputs(processrunid)
    prog_oasis_location = \
        flamingo_db_utils.get_prog_oasis_location(processrunid)

    ts = values.get_timestamp()
    process_dir = "ProcessRun_" + str(processrunid) + "_" + ts
    input_location = str(prog_oasis_location) + "/" + str(process_dir)
    if not os.path.isdir(input_location):
        os.mkdir(input_location)


    for i in ("items", "coverages", "fm_programme", "fm_policytc", "fm_xref", "fm_profile"):
        source_file = "{}/{}.csv".format(prog_oasis_location, i)
        target_file = "{}/{}.csv".format(input_location, i)
        if not IS_WINDOWS_HOST:
            os.symlink(source_file, target_file)
        else:
            shutil.copy(source_file, target_file)

    input_dir_list = os.listdir(prog_oasis_location)
    ri_dir_list = []
    for dirs in input_dir_list:
        if dirs.startswith('RI_'):
            ri_dir_list.append(dirs)

    for dirs in ri_dir_list:
        ri_full_path = prog_oasis_location + '/' + dirs
        ri_target_path = input_location + '/' + dirs
        if not IS_WINDOWS_HOST:
            os.symlink(ri_full_path, ri_target_path)
        else:
            shutil.copytree(ri_full_path, ri_target_path)

    db.bcp("OasisGULSUMMARYXREF", input_location+ "/gulsummaryxref_temp.csv")
    db.bcp("OasisFMSUMMARYXREF", input_location + "/fmsummaryxref_temp.csv")
    db.bcp("OasisRISUMMARYXREF", input_location + "/risummaryxref_temp.csv")

    gulsummaryxref = input_location + "/gulsummaryxref.csv"
    destination = open(gulsummaryxref, 'wb')
    destination.write("coverage_id,summary_id,summaryset_id\n")
    shutil.copyfileobj(
        open(input_location + "/gulsummaryxref_temp.csv", 'rb'),
        destination)
    destination.close()
    os.remove(input_location + "/gulsummaryxref_temp.csv")

    fmsummaryxref = input_location + "/fmsummaryxref.csv"
    destination = open(fmsummaryxref, 'wb')
    destination.write("output_id,summary_id,summaryset_id\n")
    shutil.copyfileobj(
        open(input_location + "/fmsummaryxref_temp.csv", 'rb'),
        destination)
    destination.close()
    os.remove(input_location + "/fmsummaryxref_temp.csv")

    risummaryxref = input_location + "/risummaryxref.csv"
    destination = open(risummaryxref, 'wb')
    destination.write("output_id,summary_id,summaryset_id\n")
    shutil.copyfileobj(
        open(input_location + "/risummaryxref_temp.csv", 'rb'),
        destination)
    destination.close()
    os.remove(input_location + "/risummaryxref_temp.csv")

    dirs = os.listdir(input_location)
    ri_dirs = []
    for dir in dirs:
        if dir.startswith('RI_'):
            ri_dirs.append(dir)
    for dir in ri_dirs:
        full_dir = os.path.join(input_location,dir)
        shutil.copy(os.path.join(input_location,"risummaryxref.csv"), os.path.join(full_dir,"fmsummaryxref.csv"))

    process_run_locationid = flamingo_db_utils.get_process_run_locationid(
        prog_oasis_location, process_dir, processrunid)

    flamingo_db_utils.generate_oasis_files_records_outputs(
        processrunid, process_run_locationid)

    return input_location


@log.oasis_log()
def get_analysis_settings_json(processrunid):

    general_settings_data = \
        flamingo_db_utils.get_general_settings(processrunid)
    model_settings_data = \
        flamingo_db_utils.get_model_settings(processrunid)
    gul_summaries_data = \
        flamingo_db_utils.get_gul_summaries(processrunid)
    il_summaries_data = \
        flamingo_db_utils.get_il_summaries(processrunid)
    ri_summaries_data = \
        flamingo_db_utils.get_ri_summaries(processrunid)

    analysis_settings = dict()
    general_settings = dict()

    for row in general_settings_data:
        if row[2] == 'bool':
            general_settings[row[0]] = eval("{}({})".format(row[2], row[1]))
        else:
            general_settings[row[0]] = eval("{}('{}')".format(row[2], row[1]))

    model_settings = dict()
    logging.getLogger().debug("Model settings:")
    for row in model_settings_data:
        logging.getLogger().debug("row: %r" % row)
        if row[2] == 'bool':
            model_settings[row[0]] = eval("{}({})".format(row[2], row[1]))
        else:
            model_settings[row[0]] = eval("{}('{}')".format(row[2], row[1]))

    gul_summaries_dict = dict()
    for row in gul_summaries_data:
        id = int(row[0])
        if not gul_summaries_dict.has_key(id):
            gul_summaries_dict[id] = dict()
        if row[3] == 1:
            if not gul_summaries_dict[id].has_key('leccalc'):
                gul_summaries_dict[id]['leccalc'] = dict()
                gul_summaries_dict[id]['leccalc']['outputs'] = dict()
            if row[1] == 'return_period_file':
                gul_summaries_dict[id]['leccalc'][row[1]] = bool(row[2])
            else:
                gul_summaries_dict[id]['leccalc']['outputs'][row[1]] = bool(row[2])
        else:
            gul_summaries_dict[id][row[1]] = bool(row[2])

    gul_summaries = list()
    for id in gul_summaries_dict.keys():
        gul_summaries_dict[id]['id'] = id
        gul_summaries.append(gul_summaries_dict[id])

    il_summaries_dict = dict()
    for row in il_summaries_data:
        id = int(row[0])
        if not il_summaries_dict.has_key(id):
            il_summaries_dict[id] = dict()
        if row[3] == 1:
            if not il_summaries_dict[id].has_key('leccalc'):
                il_summaries_dict[id]['leccalc'] = dict()
                il_summaries_dict[id]['leccalc']['outputs'] = dict()
            if row[1] == 'return_period_file':
                il_summaries_dict[id]['leccalc'][row[1]] = bool(row[2])
            else:
                il_summaries_dict[id]['leccalc']['outputs'][row[1]] = bool(row[2])
        else:
            il_summaries_dict[id][row[1]] = bool(row[2])

    il_summaries = list()
    for id in il_summaries_dict.keys():
        il_summaries_dict[id]['id'] = id
        il_summaries.append(il_summaries_dict[id])

    ri_summaries_dict = dict()
    for row in ri_summaries_data:
        id = int(row[0])
        if not ri_summaries_dict.has_key(id):
            ri_summaries_dict[id] = dict()
        if row[3] == 1:
            if not ri_summaries_dict[id].has_key('leccalc'):
                ri_summaries_dict[id]['leccalc'] = dict()
                ri_summaries_dict[id]['leccalc']['outputs'] = dict()
            if row[1] == 'return_period_file':
                ri_summaries_dict[id]['leccalc'][row[1]] = bool(row[2])
            else:
                ri_summaries_dict[id]['leccalc']['outputs'][row[1]] = bool(row[2])
        else:
            ri_summaries_dict[id][row[1]] = bool(row[2])

    ri_summaries = list()
    for id in ri_summaries_dict.keys():
        ri_summaries_dict[id]['id'] = id
        ri_summaries.append(ri_summaries_dict[id])

    general_settings['model_settings'] = model_settings
    general_settings['gul_summaries'] = gul_summaries
    general_settings['il_summaries'] = il_summaries
    general_settings['ri_summaries'] = ri_summaries
    analysis_settings['analysis_settings'] = general_settings
    apijson = jsonpickle.encode(analysis_settings)
    return json.loads(apijson)


@log.oasis_log()
def extract_tarball(tar_file, output_dir):
    unzip_command = "tar xf {} -C {}".format(tar_file, output_dir)
    unzip_result = subprocess.call(unzip_command, shell=True)
    if int(unzip_result) != 0:
        logging.getLogger().debug(
            "unzip command: {}".format(unzip_command))
        raise Exception("Failed to extract tarball")


@log.oasis_log()
def process_keys_response(progoasisid, modelid, apiJSON, sessionid):
    all_location_count = 0
    success_location_count = 0
    nomatch_location_count = 0
    fail_location_count = 0
    ts = values.get_timestamp()
    mapped_exposure_file = FILES_DIRECTORY + "/APIOutput/ExposureKeys_" + str(ts) + ".csv"
    logging.getLogger().info("Writing mapped exposure to {}".format(mapped_exposure_file))
    error_file = FILES_DIRECTORY + "/APIOutput/ExposureKeysError_" + str(ts) + ".csv"
    logging.getLogger().info("Writing non-mapped and failed exposure to {}".format(error_file))
    error_file = FILES_DIRECTORY + "/APIOutput/ExposureKeysError_" + str(ts) + ".csv"
    with io.open(mapped_exposure_file, "w", encoding='utf-8') as out_file, open(error_file, "w") as error_file:

        out_writer = csv.writer(out_file)
        error_writer = csv.writer(error_file)

        out_writer.writerow(["LocID", "PerilID", "CoverageID", "AreaPerilID", "VulnerabilityID"])
        error_writer.writerow(["LocID", "PerilID", "CoverageID", "Message"])
        for location in apiJSON:
            all_location_count = all_location_count + 1
            if location['status'] == 'success':
                success_location_count = success_location_count + 1
                out_writer.writerow([
                    location['id'], 
                    location['peril_id'], 
                    location.get("coverage") or location.get("coverage_type"), 
                    location['area_peril_id'], 
                    location['vulnerability_id']
                    ])

            elif location['status'] == 'nomatch':
                nomatch_location_count = nomatch_location_count + 1
                error_writer.writerow([
                    location['id'], 
                    location['peril_id'], 
                    location.get("coverage") or location.get("coverage_type"), 
                    location['message']
                    ])

            elif location['status'] == 'fail':
                fail_location_count = fail_location_count + 1
                error_writer.writerow([
                    location['id'], 
                    location['peril_id'], 
                    location.get("coverage") or location.get("coverage_type"), 
                    location['message']
                    ])

    logging.getLogger().info('{:,} locations'.format(all_location_count))
    logging.getLogger().info('{0:.2f}% success'.format(100.0 * success_location_count/all_location_count))
    logging.getLogger().info('{0:.2f}% fail'.format(100.0 * fail_location_count/all_location_count))
    logging.getLogger().info('{0:.2f}% no match'.format(100.0 * nomatch_location_count/all_location_count))

    flamingo_db_utils.get_api_return_data(
        progoasisid, "ExposureKeys_" + str(ts) + ".csv", sessionid)

    flamingo_db_utils.create_api_error_file_record(
        "ExposureKeysError_" + str(ts) + ".csv", progoasisid)

@log.oasis_log()
def do_call_keys_service(progoasisid):

    (progid, modelid, fileid) = \
        flamingo_db_utils.get_prog_oasis_details(progoasisid)

    (targetURI, systemname) = \
        flamingo_db_utils.get_api_uri_and_systemname(modelid)
    logging.getLogger().debug(
        "Target URI: {}, System name: {}".format(
            targetURI, systemname))

    filename = flamingo_db_utils.get_filename(fileid)
    logging.getLogger().debug("Filename = {}".format(filename))

    locations = []

    reload(sys)
    sys.setdefaultencoding('utf-8')
    logging.getLogger().info("Encoding mode: %r" % sys.getdefaultencoding())
    logging.getLogger().info("Reading exposures from {}".format(filename))

    data = None
    with io.open(filename, 'r', encoding='utf-8') as exposure_file:
        data = exposure_file.read().encode()

    headers = {
        'Accept-Encoding': 'identity,deflate,gzip,compress',
        'Content-Type': http.HTTP_REQUEST_CONTENT_TYPE_CSV,
        'Content-Length': str(len(data))
    }

    response = requests.post(targetURI, headers=headers, data=data)

    response_json = json.loads(response.content)
    locations = response_json['items']
    sessionid = response_json['session_id'] if 'session_id' in response_json else 1
    process_keys_response(progoasisid, modelid, locations, sessionid)

    return {'In': 'Yes'}


@log.oasis_log()
def do_generate_oasis_files(progoasisid):

    location_id = -1
    progid = flamingo_db_utils.get_ProgId_For_ProgOasis(progoasisid)[0]
    status = flamingo_db_utils.generate_oasis_files(progoasisid)

    if status != "Done":
        raise Exception("Failed to generate Oasis files")

    progoasis_dir = "ProgOasis_" + progoasisid
    input_location = OASIS_FILES_DIRECTORY + "/" + progoasis_dir
    if not os.path.isdir(input_location):
        os.mkdir(input_location)

    location_id = flamingo_db_utils.generate_location_record(
        OASIS_FILES_DIRECTORY, progoasis_dir, "ProgOasis_" + progoasisid)[0]

    logging.getLogger().info("location_id: {}".format(location_id))
    if location_id == -1:
        raise Exception("Failed to generate location record")

    items = input_location + "/items.csv"
    coverages = input_location + "/coverages.csv"
    fm_programme = input_location + "/fm_programme.csv"
    fm_policytc = input_location + "/fm_policytc.csv"
    fm_xref = input_location + "/fm_xref.csv"
    fm_profile = input_location + "/fm_profile.csv"
    itemdict = input_location + "/ItemDict.csv"
    fmdict = input_location + "/FMDict.csv"

    db.bcp("OasisCOVERAGES", OASIS_FILES_DIRECTORY + "/Coverages_temp.csv")
    db.bcp("OasisITEMS", OASIS_FILES_DIRECTORY + "/Items_temp.csv")
    db.bcp("OasisFM_PROGRAMME", OASIS_FILES_DIRECTORY + "/FMProgramme_temp.csv")
    db.bcp("OasisFM_POLICYTC", OASIS_FILES_DIRECTORY + "/FMPolicyTC_temp.csv")
    db.bcp("OasisFM_PROFILE", OASIS_FILES_DIRECTORY + "/FMProfile_temp.csv")
    db.bcp("OasisFM_XREF", OASIS_FILES_DIRECTORY + "/FMXRef_temp.csv")
    db.bcp("OasisITEMDICT", OASIS_FILES_DIRECTORY + "/ItemDict_temp.csv")
    db.bcp("OasisFMDICT", OASIS_FILES_DIRECTORY + "/FMDict_temp.csv")

    destination = open(coverages, 'wb')
    destination.write("coverage_id,tiv\n")
    shutil.copyfileobj(open(OASIS_FILES_DIRECTORY + "/Coverages_temp.csv", 'rb'), destination)
    destination.close()
    os.remove(OASIS_FILES_DIRECTORY + "/Coverages_temp.csv")

    destination = open(items, 'wb')
    destination.write("item_id,coverage_id,areaperil_id,vulnerability_id,group_id\n")
    shutil.copyfileobj(open(OASIS_FILES_DIRECTORY + "/Items_temp.csv", 'rb'), destination)
    destination.close()
    os.remove(OASIS_FILES_DIRECTORY + "/Items_temp.csv")

    destination = open(fm_programme, 'wb')
    destination.write("from_agg_id,level_id,to_agg_id\n")
    shutil.copyfileobj(open(OASIS_FILES_DIRECTORY + "/FMProgramme_temp.csv", 'rb'), destination)
    destination.close()
    os.remove(OASIS_FILES_DIRECTORY + "/FMProgramme_temp.csv")

    destination = open(fm_policytc, 'wb')
    destination.write("layer_id,level_id,agg_id,policytc_id\n")
    shutil.copyfileobj(open(OASIS_FILES_DIRECTORY + "/FMPolicyTC_temp.csv", 'rb'), destination)
    destination.close()
    os.remove(OASIS_FILES_DIRECTORY + "/FMPolicyTC_temp.csv")

    destination = open(fm_profile, 'wb')
    destination.write("policytc_id,calcrule_id,deductible1,deductible2,deductible3,attachment1,limit1,share1,share2,share3\n")
    shutil.copyfileobj(open(OASIS_FILES_DIRECTORY + "/FMProfile_temp.csv", 'rb'), destination)
    destination.close()
    os.remove(OASIS_FILES_DIRECTORY + "/FMProfile_temp.csv")

    destination = open(fm_xref, 'wb')
    destination.write("output_id,agg_id,layer_id\n")
    shutil.copyfileobj(open(OASIS_FILES_DIRECTORY + "/FMXRef_temp.csv", 'rb'), destination)
    destination.close()
    os.remove(OASIS_FILES_DIRECTORY + "/FMXRef_temp.csv")

    destination = open(itemdict, 'wb')
    destination.write("item_id,coverage_id,location_id,location_desc,lob_id,lob_desc,county_id,county_desc,state_id,state_desc\n")
    shutil.copyfileobj(open(OASIS_FILES_DIRECTORY + "/ItemDict_temp.csv", 'rb'), destination)
    destination.close()
    os.remove(OASIS_FILES_DIRECTORY + "/ItemDict_temp.csv")

    destination = open(fmdict, 'wb')
    destination.write("output_id,item_id,agg_id,layer_id,policy_name,layer_name\n")
    shutil.copyfileobj(open(OASIS_FILES_DIRECTORY + "/FMDict_temp.csv", 'rb'), destination)
    destination.close()
    os.remove(OASIS_FILES_DIRECTORY + "/FMDict_temp.csv")

    flamingo_db_utils.generate_oasis_file_records(progoasisid, location_id)

    try:
        reins_info_file = flamingo_db_utils.get_source_reinsurance_file_for_prog(progid)[0]
        reins_scope_file = flamingo_db_utils.get_source_reinsurance_scope_file_for_prog(progid)[0]
        is_reinsurance = True
    except:
        is_reinsurance = False
    logging.getLogger().info("is_reinsurance: {}".format(is_reinsurance))

    if is_reinsurance:
        do_generate_reinsurance_files(progoasisid)

@log.oasis_log()
def do_generate_reinsurance_files(progoasisid):

    progoasis_dir = "ProgOasis_" + progoasisid
    input_location = OASIS_FILES_DIRECTORY + "/" + progoasis_dir

    # oed files
    oed_location = input_location + '/oed_files'
    if not os.path.isdir(oed_location):
        os.mkdir(oed_location)
    progid = flamingo_db_utils.get_ProgId_For_ProgOasis(progoasisid)[0]
    
    # location
    location_file = flamingo_db_utils.get_source_loc_file_for_prog(progid)[0]
    source_file = "/var/www/oasis/Files/Exposures/{}".format(location_file)
    target_file = "{}/location.csv".format(oed_location)
    if not IS_WINDOWS_HOST:
        os.symlink(source_file, target_file)
    else:
        shutil.copy(source_file, target_file)

    # account
    account_file = flamingo_db_utils.get_source_acc_file_for_prog(progid)[0]
    source_file = "/var/www/oasis/Files/Exposures/{}".format(account_file)
    target_file = "{}/account.csv".format(oed_location)
    if not IS_WINDOWS_HOST:
        os.symlink(source_file, target_file)
    else:
        shutil.copy(source_file, target_file)

    # reinsurance info
    try:
        reins_info_file = flamingo_db_utils.get_source_reinsurance_file_for_prog(progid)[0]
        source_file = "/var/www/oasis/Files/Exposures/{}".format(reins_info_file)
        target_file = "{}/ri_info.csv".format(oed_location)
        if not IS_WINDOWS_HOST:
            os.symlink(source_file, target_file)
        else:
            shutil.copy(source_file, target_file)
    except:
        reins_info_file = None

    # reinsurance scope
    try:
        reins_scope_file = flamingo_db_utils.get_source_reinsurance_scope_file_for_prog(progid)[0]
        source_file = "/var/www/oasis/Files/Exposures/{}".format(reins_scope_file)
        target_file = "{}/ri_scope.csv".format(oed_location)
        if not IS_WINDOWS_HOST:
            os.symlink(source_file, target_file)
        else:
            shutil.copy(source_file, target_file)
    except:
        reins_scope_file = None

    # xref description
    item_dict_file = input_location + '/ItemDict.csv'
    item_dict = pd.read_csv(item_dict_file)
    fm_dict_file = input_location + '/FMDict.csv'
    fm_dict = pd.read_csv(fm_dict_file)
    combined_dict = item_dict.merge(fm_dict, on=('item_id'))
    xref_description = combined_dict[['layer_name','policy_name','location_desc']]
    xref_description.columns = ['policy_number','account_number','location_number']
    xref_description['xref_id'] = xref_description.index + 1
    xref_description['coverage_type_id'] = 1
    xref_description['peril_id'] = 1
    xref_description['tiv'] = 1
    xref_description = xref_description[['xref_id','policy_number','account_number','location_number','coverage_type_id','peril_id','tiv']]
    xref_description_file = input_location + '/xref_descriptions.csv'
    xref_description.to_csv(xref_description_file, index=False)

    # generate dfs
    #(account_df, location_df, ri_info_df, ri_scope_df, do_reinsurance) = load_oed_dfs(oed_location)
    (ri_info_df, ri_scope_df, do_reinsurance) = load_oed_dfs(oed_location)

    # direct layers
    items = pd.read_csv(input_location + "/items.csv")
    coverages = pd.read_csv(input_location + "/coverages.csv")
    fm_xrefs = pd.read_csv(input_location + "/fm_xref.csv")
    xref_descriptions = pd.read_csv(input_location + "/xref_descriptions.csv")

    validate_inst = OedValidator()
    #(main_is_valid, inuring_layers) = validate_inst.validate(account_df, location_df, ri_info_df, ri_scope_df)
    (main_is_valid, inuring_layers) = validate_inst.validate(ri_info_df, ri_scope_df)

    logging.getLogger().info("main_is_valid: {}".format(main_is_valid))
    logging.getLogger().info("inuring_layers: {}".format(inuring_layers))

#    if not main_is_valid:
#        print("Reinsuarnce structure not valid")
#        for inuring_layer in inuring_layers:
#            if not inuring_layers.is_valid:
#                print("Inuring layer {} invalid:".format(
#                    inuring_layer.inuring_priority))
#                for validation_message in inuring_layer.validation_messages:
#                    print("\t{}".format(validation_message))
#                exit(0)

    generate_files_for_reinsurance(
            #account_df,
            #location_df,
            items,
            coverages,
            fm_xrefs,
            xref_descriptions,
            ri_info_df,
            ri_scope_df,
            input_location)


















