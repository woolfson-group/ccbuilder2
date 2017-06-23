"""Contains code for managing and processing optimisation requests."""

import multiprocessing as mp
import time
import sys

import database
import model_building


def main():
    processes = 1
    queue = mp.Queue()
    listeners = [
        mp.Process(target=get_and_process_opt_jobs,
                   args=(queue, database.opt_jobs))
        for _ in range(processes)
    ]
    for listener in listeners:
        listener.start()

    while True:
        jobs = database.opt_jobs.find(
            {'status': database.JobStatus.SUBMITTED.name})
        for job in database.opt_jobs.find():
            print('{_id}, {status}'.format(**job))
        for job in sorted(jobs, key=lambda x: x['time_submitted']):
            queue.put(job['_id'])
            database.opt_jobs.update_one(
                {'_id': job['_id']},
                {'$set': {'status': database.JobStatus.QUEUED.name}})
        time.sleep(10)
    return


def get_and_process_opt_jobs(opt_job_queue, job_collection):
    """Collect and run optimisation jobs from queue.

    Used by the the OptimizationManager to initialise Processes.

    Parameters
    ----------
    opt_job_queue : multiprocessing.Queue
        Optimisation job queue.
    """
    while True:
        job_id = opt_job_queue.get()
        print("Got opt job {}!".format(job_id), file=sys.stderr)
        database.opt_jobs.update_one(
                {'_id': job_id},
                {'$set': {'status': database.JobStatus.RUNNING.name}})
        build_start_time = datetime.datetime.now()
        opt_id = create_opt_job_entry(request.json)
        optimisation_result = optimise_coiled_coil(
            request.json['Parameters'], debug=app.debug)
        build_start_end = datetime.datetime.now()
        build_time = build_start_end - build_start_time
        return jsonify(optimisation_result)
    return


def run_optimisation(parameters):
    build_start_time = datetime.datetime.now()
    optimisation_result = optimise_coiled_coil(
        parameters, debug=app.debug)
    build_start_end = datetime.datetime.now()
    return model_id

if __name__ == '__main__':
    main()
