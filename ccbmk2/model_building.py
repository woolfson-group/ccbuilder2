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
        parameter_dict['Oligomer State'],
        len(parameter_dict['Sequence']),
        parameter_dict['Radius'],
        parameter_dict['Pitch'],
        parameter_dict['Interface Angle'])
    coiled_coil.pack_new_sequences([parameter_dict['Sequence']] * parameter_dict['Oligomer State'])
    return coiled_coil.pdb
