from flask import Flask


app = Flask(__name__, static_url_path='/ccbuilder2/static')

import ccbmk2.views
