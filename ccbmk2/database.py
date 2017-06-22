"""Database client and collections for CCBMk2"""

import pymongo

client = pymongo.MongoClient('db', 27017)
parameters_store = client.ccbuilder.chain_parameters
request_store = client.ccbuilder.requests
build_log = client.ccbuilder.build_log
model_store = client.ccbuilder.models
opt_jobs = client.ccbuilder.opt_jobs
