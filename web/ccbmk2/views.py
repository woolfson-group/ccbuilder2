"""Views for CCBuilder Mk.II"""

import datetime
import sys

from flask import jsonify, render_template, request

from ccbmk2 import app
from ccbmk2 import database
from ccbmk2 import model_building


@app.route('/')
def welcome():
    """Welcome to CCBuilder splash screen."""
    return render_template('welcome.html')


@app.route('/builder')
def builder():
    """Main view for the builder interface."""
    return render_template('builder.html')


@app.route('/builder/api/v0.1/build/coiled-coil', methods=['POST'])
def build_coiled_coil_model():
    """Passes to the build commands to the model_building module."""
    parameters_list = request.json['Parameters']
    (build_request_id, save_model) = database.store_build_request(
        parameters_list)
    model_record = database.models.find_one({'_id': build_request_id})
    if model_record is None:
        build_start_time = datetime.datetime.now()
        pdb, score, rpt = model_building.build_coiled_coil(
            parameters_list, debug=app.debug)
        build_start_end = datetime.datetime.now()
        build_time = build_start_end - build_start_time
        database.log_build_info(request, build_time, build_request_id)
        model_id = database.store_model(build_request_id, pdb, score, rpt)
        model_and_info = {
            'model_id': str(model_id),
            'pdb': pdb,
            'score': score,
            'mean_rpt_value': rpt,
        }
    else:
        model_and_info = {
            'model_id': str(model_record['_id']),
            'pdb': model_record['pdb'],
            'score': model_record['score'],
            'mean_rpt_value': model_record['mean_rpt_value'],
        }
    return jsonify(model_and_info)


@app.route('/builder/api/v0.1/optimise/coiled-coil', methods=['POST'])
def optimise_coiled_coil_model():
    """Runs a parameter optimisation for a supplied model."""
    build_start_time = datetime.datetime.now()
    opt_id = database.create_opt_job_entry(request.json)
    return jsonify(str(opt_id))
