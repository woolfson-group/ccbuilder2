import flask

from ccbmk2 import app


@app.route('/')
def builder():
    return flask.render_template('builder.html')
