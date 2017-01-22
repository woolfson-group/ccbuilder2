from flask import Flask

app = Flask(__name__, instance_relative_config=True)
app.config.from_object('config.default')
app.config.from_pyfile('config.py')
app.config.from_envvar('CCBMK2_CONFIG_FILE', silent=True)

import ccbmk2.views
