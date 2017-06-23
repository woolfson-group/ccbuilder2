"""Database client and collections for CCBMk2"""

import enum

import pymongo

client = pymongo.MongoClient('db', 27017)
parameters_store = client.ccbuilder.chain_parameters
request_store = client.ccbuilder.requests
build_log = client.ccbuilder.build_log
model_store = client.ccbuilder.models
opt_jobs = client.ccbuilder.opt_jobs


class JobStatus(enum.Enum):
    SUBMITTED = 1
    QUEUED = 2
    RUNNING = 3
    FINISHED = 4
