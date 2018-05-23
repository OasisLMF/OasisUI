# (c) 2013-2018 Oasis LMF Ltd.

import inspect
import logging
import os
import sys
import threading

from flask import Flask, jsonify

from oasislmf.utils import (
    log,
    db,
    conf,
)

import flamingo_utils

app = Flask(__name__)

CURRENT_DIRECTORY = os.path.dirname(os.path.abspath(inspect.getfile(inspect.currentframe())))

INI_PATH = os.path.abspath(os.path.join(CURRENT_DIRECTORY, 'FlamingoApi.ini'))

CONFIG_PARSER = conf.load_ini_file(INI_PATH)
CONFIG_PARSER['LOG_FILE'] = CONFIG_PARSER['LOG_FILE'].replace('%LOG_DIRECTORY%', CONFIG_PARSER['LOG_DIRECTORY'])

flamingo_utils.IS_WINDOWS_HOST = bool(CONFIG_PARSER['IS_WINDOWS_HOST'])
flamingo_utils.FILES_DIRECTORY = os.path.join('/', 'var', 'www', 'oasis', 'Files')
flamingo_utils.OASIS_FILES_DIRECTORY = os.path.join(flamingo_utils.FILES_DIRECTORY, 'OasisFiles')

logger = logging.getLogger()

logger.info("FILES_DIRECTORY: {}".format(flamingo_utils.FILES_DIRECTORY))
logger.info("IS_WINDOWS_HOST: {}".format(flamingo_utils.IS_WINDOWS_HOST))

# Logging configuration
log.read_log_config(CONFIG_PARSER)

# Database configuration
db.read_db_config(CONFIG_PARSER)
if db.check_connection():
    logger.info("Successfully connected to Flamingo database")
else:
    logger.exception("Failed to connect to Flamingo database")
    exit()


@app.route("/healthcheck")
@log.oasis_log()
def healthcheck():
    '''
    Basic healthcheck
    '''
    return "OK"


@app.route("/loadprogrammemodel/<progoasisid>")
@log.oasis_log()
def load_programme_model(progoasisid):
    '''
    Start a background thread to load programme model
    '''
    run_async(target=flamingo_utils.do_load_programme_model, args=(progoasisid,))
    returnval = {}
    returnval['Status'] = 'Loading Programme data'
    return jsonify(returnval)


@app.route("/loadprogrammedata/<progid>")
@log.oasis_log()
def load_programme_data(progid):
    '''
    Start a background thread to load programme data
    '''
    run_async(target=flamingo_utils.do_load_programme_data, args=(progid,))
    returnval = {}
    returnval['Status'] = 'Loading Programme data'
    return jsonify(returnval)


@app.route("/runprogoasis/<processrunid>")
@log.oasis_log()
def run_prog_oasis(processrunid):
    '''
    Start a background thread to execute a programme model
    '''
    run_async(target=flamingo_utils.do_run_prog_oasis, args=(processrunid,))
    returnval = {}
    returnval['Status'] = 'Initiated process run'
    return jsonify(returnval)


def run_async(target, args):
    '''
    Asynchronous method execution.
    Uses a thread rather than a subprocess.
    '''
    new_thread = threading.Thread(
        target=target, args=args)
    new_thread.start()
