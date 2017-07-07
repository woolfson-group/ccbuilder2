"""Views for CCBuilder Mk.II"""

import datetime

from flask import jsonify, redirect, render_template, request
from bson.objectid import ObjectId

from ccbmk2 import app, database, model_building


@app.route('/')
def welcome():
    """Welcome to CCBuilder splash screen."""
    return redirect('/builder')


@app.route('/builder')
def builder():
    """Main view for the builder interface."""
    return render_template('builder.html')


@app.route('/builder/api/v0.1/build/coiled-coil', methods=['POST'])
def build_coiled_coil_model():
    """Generates and returns a coiled-coil model."""
    model_and_info = build_and_record_model(
        request, model_building.HelixType.ALPHA)
    return jsonify(model_and_info)


@app.route('/builder/api/v0.1/build/collagen', methods=['POST'])
def build_collagen_model():
    """Generates and returns a collagen model."""
    model_and_info = build_and_record_model(
        request, model_building.HelixType.COLLAGEN)
    return jsonify(model_and_info)


def build_and_record_model(request, helix_type):
    """Records request and either builds a model of retrieves from DB."""
    parameters_list = request.json['Parameters']
    # Number of times before save not currently in use
    (build_request_id, save_model) = database.store_build_request(
        parameters_list, helix_type)
    model_record = database.models.find_one({'_id': build_request_id})
    if model_record is None:
        build_start_time = datetime.datetime.now()
        if helix_type is model_building.HelixType.ALPHA:
            pdb, score, rpt, knob_ids = model_building.build_coiled_coil(
                parameters_list, debug=app.debug)
        elif helix_type is model_building.HelixType.COLLAGEN:
            pdb, score, rpt, knob_ids = model_building.build_collagen(
                parameters_list, debug=app.debug)
        else:
            raise ValueError('Unknown helix type.')
        build_start_end = datetime.datetime.now()
        build_time = build_start_end - build_start_time
        database.log_build_info(request, build_time, build_request_id)
        model_id = database.store_model(
            build_request_id, pdb, score, rpt, knob_ids)
        model_record = {
            'model_id': str(model_id),
            'helix_type': helix_type.name,
            'pdb': pdb,
            'score': score,
            'mean_rpt_value': rpt,
            'knob_ids': knob_ids
        }
    else:
        # Change to string from ObjectID for response
        model_record['model_id'] = str(model_record.pop('_id'))
        model_record['helix_type'] = helix_type.name
    return model_record


@app.route('/builder/api/v0.1/optimise/model', methods=['POST'])
def optimise_model():
    """Runs a parameter optimisation for a supplied model."""
    opt_id = database.create_opt_job_entry(request.json)
    return jsonify(str(opt_id))


@app.route('/builder/api/v0.1/optimise/check-job-status', methods=['GET'])
def get_optimisation_status():
    """Get the status of an optimisation job."""
    opt_job_id = request.args.get('opt-job-id')
    opt_job = database.opt_jobs.find_one({'_id': ObjectId(opt_job_id)})
    return jsonify({'_id': opt_job_id, 'status': opt_job['status']})


@app.route('/builder/api/v0.1/optimise/retrieve-opt-job', methods=['GET'])
def get_optimisation_result():
    """Get the status of an optimisation job."""
    opt_job_id = request.args.get('opt-job-id')
    opt_job = database.opt_jobs.find_one({'_id': ObjectId(opt_job_id)})
    model = database.models.find_one({'_id': opt_job['model_id']})
    model_and_parameters = {
        'model_and_info': {
            'model_id': str(model['_id']),
            'helix_type': opt_job['helix_type'],
            'pdb': model['pdb'],
            'score': model['score'],
            'mean_rpt_value': model['mean_rpt_value'],
            'knob_ids': model['knob_ids']
        },
        'parameters': opt_job['final_parameters'],
        'oligomeric_state': opt_job['oligomeric_state']
    }
    return jsonify(model_and_parameters)
