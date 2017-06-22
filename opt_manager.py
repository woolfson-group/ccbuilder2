"""Contains code for managing and processing optimisation requests."""

import multiprocessing as mp
import time
import sys

from ccbmk2 import database


class OptimizationManager:
    """Manages queue and processes for model optimisation.

    Parameters
    ----------
    job_collection : Collection
        Collection from the CCBuilder database containing job
        information for the optimiser.

    processes : Int
        Number of processes for optimisation runs to be mainained by
        the OptimizationManager.
    """

    def __init__(self, job_collection, processes=1):
        self.queue = mp.Queue()
        self.job_collection = job_collection
        self.listeners = [
            mp.Process(target=self.get_and_process_opt_jobs,
                       args=(self.queue, job_collection))
            for _ in range(processes)
        ]
        for listener in self.listeners:
            listener.start()

    @staticmethod
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
            time.sleep(10)
            print("Got opt job {}!".format(job_id), file=sys.stderr)
        return


if __name__ == '__main__':
    manager = OptimizationManager(database.opt_jobs, 2)
    recs = database.opt_jobs.find()
    for rec in recs:
        manager.queue.put(rec['_id'])
    while True:
        print("I'm still alive!")
        time.sleep(10)
