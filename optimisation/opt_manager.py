"""Contains code for managing and processing optimisation requests."""

import datetime
import importlib
import multiprocessing as mp
import time
import os
import sys

import database
import model_building


def main():
    """Establishes the manager and listener subprocesses"""
    processes = int(os.getenv(key='OPT_PROCS', default='1'))
    with mp.Manager() as manager:
        queue = manager.Queue()
        assigned_jobs = manager.list([None] * processes)
        listeners = [
            mp.Process(target=get_and_process_opt_jobs,
                       args=(queue, assigned_jobs, proc_i))
            for proc_i in range(processes)
        ]
        for listener in listeners:
            listener.start()

        while True:
            submitted_jobs = database.opt_jobs.find(
                {'status': database.JobStatus.SUBMITTED.name})
            for job in sorted(submitted_jobs,
                              key=lambda x: x['time_submitted']):
                queue.put(job['_id'])
                database.opt_jobs.update_one(
                    {'_id': job['_id']},
                    {'$set': {'status': database.JobStatus.QUEUED.name}})
            running_jobs = database.opt_jobs.find(
                {'status': database.JobStatus.RUNNING.name})
            for job in running_jobs:
                if job['_id'] not in assigned_jobs:
                    update_job_status(job['_id'], database.JobStatus.FAILED)
            time.sleep(10)
    return


def get_and_process_opt_jobs(opt_job_queue, assigned_jobs, proc_i):
    """Collect and run optimisation jobs from queue.

    Used by the the OptimizationManager to initialise Processes.

    Parameters
    ----------
    opt_job_queue : multiprocessing.Queue
        Optimisation job queue.
    """
    # The module is reloaded to establish a new connection
    # to the database for the process fork
    importlib.reload(database)
    while True:
        job_id = opt_job_queue.get()
        print("Got opt job {}!".format(job_id), file=sys.stderr)
        opt_job = database.opt_jobs.find_one(job_id)
        parameters = list(map(
            database.parameters_store.find_one,
            opt_job['initial_parameter_ids']))
        update_job_status(job_id, database.JobStatus.RUNNING)
        assigned_jobs[proc_i] = job_id
        print("Running opt job {}!".format(job_id), file=sys.stderr)
        run_optimisation(job_id, opt_job['helix_type'],
                         parameters)
        print("Finished opt job {}!".format(job_id), file=sys.stderr)
        assigned_jobs[proc_i] = None
    return


def update_job_status(opt_job_id, status):
    """Updates status in database entry for optimisation job."""
    database.opt_jobs.update_one(
        {'_id': opt_job_id},
        {'$set': {'status': status.name}})
    return


def run_optimisation(opt_job_id, helix_type, parameters):
    """Runs the optimisation job."""
    if helix_type == "ALPHA":
        optimised_parameters, model_and_info = model_building.optimise_coiled_coil(
            parameters, debug=True)
    elif helix_type == "COLLAGEN":
        optimised_parameters, model_and_info = model_building.optimise_collagen(
            parameters, debug=True)
    else:
        raise ValueError('Unknown helix type.')
    model_id = database.store_model(
        opt_job_id, model_and_info['pdb'], model_and_info['score'],
        model_and_info['mean_rpt_value'], model_and_info['knob_ids'])
    database.opt_jobs.update_one(
        {'_id': opt_job_id},
        {'$set': {
            'final_parameters': optimised_parameters,
            'status': database.JobStatus.COMPLETE.name,
            'time_finished': datetime.datetime.now(),
            'model_id': model_id
        },
        })
    return


if __name__ == '__main__':
    main()
