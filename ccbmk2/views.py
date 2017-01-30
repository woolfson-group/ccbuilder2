"""Views for CCBuilder Mk.II"""

import flask

from ccbmk2 import app


@app.route('/')
def builder():
    """Main view for the builder interface."""
    return flask.render_template('builder.html')
