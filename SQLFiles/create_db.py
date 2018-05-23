#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Flamingo DB build script
"""

from __future__ import print_function

__author__ = 'Sandeep Murthy, Marek Dabek'

__all__ = [
    'archive_script',
    'build_flamingo_db',
    'generate_build_sql_script',
    'parse_args',
    '_replace_in_file',
    '_unified_diff'
]

import argparse
import difflib
import os
import subprocess
import sys
import time

from subprocess import (
    check_call,
    check_output,
)

cwd = os.getcwd()
sys.path.insert(0, cwd)

source_sql_script = os.path.join(cwd, 'schema.sql')
build_sql_script = os.path.join(cwd, 'schema_run.sql')


def parse_args():
    """
    Parses script arguments and constructs an args dictionary.
    """

    parser = argparse.ArgumentParser(description='Build Flamingo database.')
    
    parser.add_argument(
        '-s', '--sql_server_ip', type=str,  required=True,
        help="The SQL Server IP.")
    
    parser.add_argument(
        '-p', '--sa_password', type=str,  required=True,
        help="The SQL Server sa password.")
    
    parser.add_argument(
        '-n', '--environment_name', type=str,  required=True,
        help="The environment name.")
    
    parser.add_argument(
        '-l', '--login_password', type=str,  required=True,
        help='The SQL login password.')
    
    parser.add_argument(
        '-f', '--file_location_sql_server', type=str,  required=True,
        help='The file location on the SQL server - enter path using forward slashes for the file separator.')
    
    parser.add_argument(
        '-F', '--file_location_shiny', type=str,  required=True,
        help='The file location on the shiny server - enter path using forward slashes for the file separator.')
    
    parser.add_argument(
        '-v', '--version', type=str,  required=True,
        help='The database version.')
    
    parser.add_argument('--mock', dest='mock', action='store_true', help='Mock the SQL command to build the DB')
    parser.add_argument('--no-mock', dest='mock', action='store_false', help='Do not mock the SQL command to build the DB')
    parser.set_defaults(mock=False)

    parser.add_argument('--archive', dest='archive', action='store_true', help='Archive the generated build SQL script after the DB build')
    parser.add_argument('--no-archive', dest='archive', action='store_false', help='Delete the generated build SQL script after the DB build')
    parser.set_defaults(archive=True)

    args = parser.parse_args()

    args_dict = vars(args)
    args_dict['file_location_sql_server'] = r''.join(args_dict['file_location_sql_server'].replace('/', '\\').replace(' ', '\ '))
    args_dict['file_location_shiny'] = r''.join(args_dict['file_location_shiny'])
    args_dict['user_password'] = args_dict['login_password']

    return args_dict


def _replace_in_file(source_file_path, target_file_path, var_names, var_values):
    """
    Replaces a list of placeholders / variable names in a source file with a
    matching set of values, and writes it out to a new target file.
    """
    if len(var_names) != len(var_values):
        raise Exception('Number of variable names does not equal the number of variable values to replace - please check and try again.')

    try:
        with open(source_file_path, 'r') as f:
            lines = f.readlines()

        with open(target_file_path, 'w') as f:
            for i in range(len(lines)):
                outline = inline = lines[i]
                present_var_names = filter(lambda var_name: var_name in inline, var_names)
                if present_var_names:
                    for var_name in present_var_names:
                        var_value = var_values[var_names.index(var_name)]
                        outline = outline.replace(var_name, var_value)
                f.write(outline)
    except (OSError, IOError) as e:
        raise e


def _unified_diff(file1, file2, as_string=False):
    """
    Generates a unified diff of two files: ``file1`` and ``file2``. The files must
    be passed in as absolute paths.
    """

    try:
        with open(file1, 'r') as f1:
            with open(file2, 'r') as f2:
                diff = difflib.unified_diff(
                    f1.readlines(),
                    f2.readlines(),
                    fromfile=f1,
                    tofile=f2,
                )
    except (OSError, IOError) as e:
        raise e
    else:
        if as_string:
            return ''.join(diff)
        return diff


def generate_build_sql_script(args, source_sql_script, build_sql_script):
    """
    Generates the DB build SQL script using the args dictionary by reading in
    the source SQL script and replacing a fixed set of variable names (or
    placeholders) with values from the create_db.py script arguments.
    """

    var_names = [
        '%ENVIRONMENT_NAME%',
        '%USER_PASSWORD%',
        '%VERSION%',
        '%FILE_LOCATION_SQL_SERVER%',
        '%FILE_LOCATION_SHINY%'
    ]

    var_values = [
        args[var_name.strip('%').lower()] if var_name else None for var_name in var_names
    ]

    _replace_in_file(source_sql_script, build_sql_script, var_names, var_values)


def build_flamingo_db(args, build_sql_script, mock=False):
    """
    Runs the SQL command to build the Flamingo DB using the args dictionary
    and the generated build SQL script, and returns the absolute path of the
    script. Can be mocked by setting the optional `mock` parameter to `True`.
    """

    sqlcmd_str = 'sqlcmd -S {} -d master -U sa -P {} -i {}'.format(
        args['sql_server_ip'], args['sa_password'], build_sql_script)
    
    if not mock:
        check_call(sqlcmd_str.split(' '))

    return sqlcmd_str


def archive_script(build_sql_script):
    """
    Archives the generated build SQL script in the working directory using a timestamp.
    """

    timestamp_str = time.strftime('%d-%m-%Y_%H:%M:%S_GMT', time.gmtime())
    archived_script = '{}.{}'.format(build_sql_script, timestamp_str)
    os.rename(build_sql_script, archived_script)

    return archived_script


if __name__ == '__main__':

    try:
        print('\nParsing arguments: ', end='')
        args = parse_args()
        print(args)
        time.sleep(3)

        print('\nGenerating build SQL script {} from source SQL script {}'.format(build_sql_script, source_sql_script))
        generate_build_sql_script(args, source_sql_script, build_sql_script)
        time.sleep(3)
        script_diff = _unified_diff(source_sql_script, build_sql_script, as_string=True)
        print('\nDiff of source script -> build script: {}'.format(script_diff))
        time.sleep(5)

        print('\nBuilding Flamingo DB: ' if not args['mock'] else '\nMocking Flamingo DB build: ', end='')
        build_cmd = build_flamingo_db(args, build_sql_script, mock=args['mock'])
        print('command used "{}"'.format(build_cmd))
        time.sleep(3)

        if args['archive']:
            print('\nArchiving build SQL script: ', end='')
            archived_script = archive_script(build_sql_script)
            print('archived as "{}"'.format(archived_script))
        else:
            print('\nDeleting build SQL script')
            os.remove(build_sql_script)
        time.sleep(3)
    except Exception as e:
        print(str(e))
        sys.exit(1)
    else:
        if not args['mock']:
            print('\nScript executed successfully - exiting.')
            time.sleep(3)
        sys.exit(0)
