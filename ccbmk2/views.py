"""Views for CCBuilder Mk.II"""

import sys

from flask import jsonify, render_template, request

from ccbmk2 import app

BUILD = "build_model"

@app.route('/')
def welcome():
    """Welcome to CCBuilder splash screen."""
    return render_template('welcome.html')


@app.route('/builder')
def builder():
    """Main view for the builder interface."""
    return render_template('builder.html')


@app.route('/builder/<cmd>', methods=['POST'])
def process_builder_command(cmd=None):
    if cmd == BUILD:
        print(request.json, file=sys.stderr)
    return jsonify("Building")