"""Views for CCBuilder Mk.II"""

import datetime
import os
import sys

from flask import jsonify, render_template, request
import pymongo

from ccbmk2 import app
from ccbmk2.model_building import build_coiled_coil

client = pymongo.MongoClient('db', 27017)
build_log = client.ccbuilder.build_log

BUILD = "build_model"

@app.route('/')
def welcome():
    """Welcome to CCBuilder splash screen."""
    return render_template('welcome.html')


@app.route('/builder')
def builder():
    """Main view for the builder interface."""
    return render_template('builder.html')


@app.route('/builder/<cmd>', methods=['POST'])
def process_builder_command(cmd=None):
    """Processes commands passed to the builder module."""
    if cmd == BUILD:
        build_start_time = datetime.datetime.now()
        pdb_and_score = build_coiled_coil(request.json)
        build_start_end = datetime.datetime.now()
        build_time = build_start_end - build_start_time
        log_build_request(request, build_time)
    return jsonify(pdb_and_score)


def log_build_request(request, build_time):
    build_request = {
        'ip': request.remote_addr,
        'date': datetime.datetime.now(),
        'build_time': build_time.total_seconds()
    }
    build_log.insert_one(build_request)
    return
