import os


def get_config():
    config_options = {
        "development": DevelopmentConfig(),
        "production": ProductionConfig(),
        "default": DevelopmentConfig()
    }
    config_name = os.getenv(key='CCBMK2_CONFIG', default='default')
    return config_options[config_name]


class BaseConfig:
    MAIL_FROM_EMAIL = "chris.wood@bristol.ac.uk"  # For use in application emails


class DevelopmentConfig(BaseConfig):
    OPT_PROCESSES = 2
    DEBUG = True


class ProductionConfig(BaseConfig):
    OPT_PROCESSES = 4
    DEBUG = False
