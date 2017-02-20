"""Module for building models using ISAMBARD."""

import isambard


def build_coiled_coil(parameter_dict):
    """Builds a model of a coiled coil using the input parameters.
    
    Parameters
    ----------
    parameter_dict : dict[str, int/float/str]
        Contains parameters required for building a coiled coil i.e.
        oligomer state, radius, pitch, phiCA, sequence.
    
    Returns
    -------
    coiled_coil : isambard.specification.CoiledCoil
        Model of a coiled coil, built by ISAMBARD.
    """
    coiled_coil = isambard.specifications.CoiledCoil.from_parameters(
        2,
        28,
        5.1,
        180,
        16)
    coiled_coil.pack_new_sequences(['LKAIAQELKAIAQELKAIAQELKAIAQE'] * 2)
    return coiled_coil.sequences
