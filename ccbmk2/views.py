"""Views for CCBuilder Mk.II"""

import datetime
import os
import sys

from flask import jsonify, render_template, request
import pymongo

from ccbmk2 import app
from ccbmk2.model_building import build_coiled_coil, optimise_coiled_coil


client = pymongo.MongoClient('db', 27017)
parameters_store = client.ccbuilder.chain_parameters
request_store = client.ccbuilder.requests
build_log = client.ccbuilder.build_log
model_store = client.ccbuilder.models


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
    (request_log_id, save_model) = store_request(request.json)
    model = model_store.find_one({'_id': request_log_id})
    if model is None:
        build_start_time = datetime.datetime.now()
        pdb_and_score = build_coiled_coil(request.json)
        build_start_end = datetime.datetime.now()
        build_time = build_start_end - build_start_time
        log_build_info(request, build_time, request_log_id)
        if save_model:
            store_model(request_log_id, pdb_and_score)
    else:
        pdb_and_score = {
            'pdb': model['pdb'],
            'score': model['score'],
            'mean_rpt_value': model['mean_rpt_value']
            }
    return jsonify(pdb_and_score)


@app.route('/builder/api/v0.1/optimise/coiled-coil', methods=['POST'])
def optimise_coiled_coil_model():
    """Runs a parameter optimisation for a supplied model."""
    build_start_time = datetime.datetime.now()
    optimisation_result = optimise_coiled_coil(request.json)
    build_start_end = datetime.datetime.now()
    build_time = build_start_end - build_start_time
    return jsonify(optimisation_result)


def store_request(request, number_for_save=5):
    """Saves the requested parameters in the database.
    
    An additional 'requested' field is added to the request before
    it is logged in the database, which is an integer that is
    incremented whenever the same parameters are requested again.

    Parameters
    ----------

    request : dict
        Json body of the Http request that contains the build
        parameters.
    
    number_for_save : int
        The number of times the model must be requested before
        it is cached in the models database.

    Returns
    -------

    request_log_id : bson.ObjectID
        The id of the parameters, this will be passed to the build
        log to be stored to avoid duplication.
    
    save_model : Bool
        Indicates whether the parameters have been requested enough
        to make it desirable to save the model in the model
        collection.
    """
    chain_ids = list(map(get_chain_parameters_id, request))
    request_log = request_store.find_one({'parameter_ids': chain_ids})
    if request_log == None:
        request_log = {'parameter_ids': chain_ids}
        request_log['requested'] = 1
        request_log_id = request_store.insert_one(request_log).inserted_id
    else:
        request_log['requested'] += 1
        request_log_id = request_log['_id']
        request_store.update_one(
            {'_id': request_log_id},
            {'$set': {'requested': request_log['requested']}})
    if request_log['requested'] == number_for_save:
        save_model = True
    else:
        save_model = False
    return request_log_id, save_model


def get_chain_parameters_id(chain_parameters):
    parameter_record = parameters_store.find_one(chain_parameters)
    if parameter_record:
        pr_id = parameter_record['_id']
    else:
        pr_id = parameters_store.insert_one(chain_parameters).inserted_id
    return pr_id


def log_build_info(request, build_time, request_log_id):
    """Saves informations about the build process to the database."""
    build_info = {
        'ip': request.remote_addr,
        'date': datetime.datetime.now(),
        'build_time': build_time.total_seconds(),
        'parameters_id': request_log_id
    }
    build_log.insert_one(build_info)
    return


def store_model(request_log_id, pdb_and_score):
    """Stores a model in the database.""" 
    model = {'_id': request_log_id}
    model['pdb'] = pdb_and_score['pdb']
    model['score'] = pdb_and_score['score']
    model['mean_rpt_value'] = pdb_and_score['mean_rpt_value']
    model_store.insert_one(model)
    return
