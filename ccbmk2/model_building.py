"""Module for building models using ISAMBARD."""

import isambard


registerAdjust = {
    'a': 0,
    'b': 102.8,
    'c': 205.6,
    'd': 308.4,
    'e': 51.4,
    'f': 154.2,
    'g': 257
}


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
    phica = parameter_dict['Interface Angle'] + registerAdjust[parameter_dict['Register']]
    coiled_coil = isambard.specifications.CoiledCoil.from_parameters(
        parameter_dict['Oligomer State'],
        len(parameter_dict['Sequence']),
        parameter_dict['Radius'],
        parameter_dict['Pitch'],
        phica
        )
    coiled_coil.pack_new_sequences([parameter_dict['Sequence']] * parameter_dict['Oligomer State'])
    return {'pdb': coiled_coil.pdb, 'score':coiled_coil.buff_internal_energy.total_energy}
