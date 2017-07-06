"""Database client and collections for CCBMk2"""

import datetime
import enum

import pymongo

client = pymongo.MongoClient('db', 27017)
parameters_store = client.ccbuilder.chain_parameters
build_requests = client.ccbuilder.build_requests
build_log = client.ccbuilder.build_log
models = client.ccbuilder.models
opt_jobs = client.ccbuilder.opt_jobs


def store_build_request(parameters_list, helix_type, number_for_save=5):
    """Saves the requested parameters in the database.

    An additional 'requested' field is added to the request before
    it is logged in the database, which is an integer that is
    incremented whenever the same parameters are requested again.

    Parameters
    ----------

    parameters_list : [dict]
        Json body of the Http request that contains a list of build
        parameter dicts.

    helix_type : model_building.HelixType
        HelixType enumerable contains options that are available
        for helix types.

    number_for_save : int
        The number of times the model must be requested before
        it is cached in the models database.

    Returns
    -------

    build_request_id : bson.ObjectID
        The id of the parameters, this will be passed to the build
        log to be stored to avoid duplication.

    save_model : Bool
        Indicates whether the parameters have been requested enough
        to make it desirable to save the model in the model
        collection.
    """
    chain_ids = list(map(get_chain_parameters_id, parameters_list))
    build_request = build_requests.find_one(
        {'parameter_ids': chain_ids, 'helix_type': helix_type.name})
    if build_request is None:
        build_request = {
            'parameter_ids': chain_ids,
            'helix_type': helix_type.name
        }
        build_request['requested'] = 1
        build_request_id = build_requests.insert_one(
            build_request
        ).inserted_id
    else:
        build_request['requested'] += 1
        build_request_id = build_request['_id']
        build_requests.update_one(
            {'_id': build_request_id},
            {'$set': {'requested': build_request['requested']}})
    if build_request['requested'] == number_for_save:
        save_model = True
    else:
        save_model = False
    return build_request_id, save_model


def get_chain_parameters_id(chain_parameters):
    """Finds the '_id' of parameters in the parameter_stores.

    Adds parameters if they are not found.
    """
    parameter_record = parameters_store.find_one(
        chain_parameters)
    if parameter_record:
        pr_id = parameter_record['_id']
    else:
        pr_id = parameters_store.insert_one(
            chain_parameters).inserted_id
    return pr_id


def log_build_info(request, build_time, build_request_id):
    """Saves informations about the build process to the database."""
    build_info = {
        'ip': request.remote_addr,
        'date': datetime.datetime.now(),
        'build_time': build_time.total_seconds(),
        'build_request_id': build_request_id
    }
    build_info_id = build_log.insert_one(build_info).inserted_id
    return build_info_id


def store_model(request_log_id, pdb, score, rpt):
    """Stores a model in the database."""
    model = {
        '_id': request_log_id,
        'pdb': pdb,
        'score': score,
        'mean_rpt_value': rpt
    }
    model_id = models.insert_one(model).inserted_id
    return model_id


def create_opt_job_entry(request):
    """Creates and stores a optimisation job in the database.

    Parameters
    ----------
    request : Dict
        Contains the Parameters and Heat required for the optimisation.

    Returns
    -------
    opt_job_id : ObjectId
        ID for the submitted optimsation job.
    """
    opt_job = {
        'helix_type': request['Helix Type'],
        'initial_parameter_ids':
            [get_chain_parameters_id(p) for p in request['Parameters']],
        'oligomeric_state': len(request['Parameters']),
        'final_parameters': None,
        'heat': request['Heat'],
        'status': JobStatus.SUBMITTED.name,
        'time_submitted': datetime.datetime.now(),
        'time_finished': None,
        'model_id': None
    }
    opt_job_id = opt_jobs.insert_one(opt_job).inserted_id
    return opt_job_id


class JobStatus(enum.Enum):
    """Options availble for the status of optimisation jobs."""
    SUBMITTED = 1
    QUEUED = 2
    RUNNING = 3
    COMPLETE = 4
    FAILED = 5
