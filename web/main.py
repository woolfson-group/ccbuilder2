import ccbmk2
import config

app = ccbmk2.app
app.config.from_object(config.get_config())


if __name__ == '__main__':
    app.run(debug=True, host='0.0.0.0')
