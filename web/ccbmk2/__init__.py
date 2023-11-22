from flask import Flask


app = Flask(__name__, static_url_path="/builder/static")

import ccbmk2.views
