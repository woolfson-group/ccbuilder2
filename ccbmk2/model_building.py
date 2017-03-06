"""Module for building models using ISAMBARD."""

import itertools

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
    model_data : dict
        A dictionary containing information about the model that has
        been produced.
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
    ave_rpt = calculate_average_rpt(coiled_coil)
    model_data = {
        'pdb': coiled_coil.pdb,
        'mean_rpt_value': ave_rpt,
        'score':coiled_coil.buff_internal_energy.total_energy
        }
    return model_data


def calculate_average_rpt(ampal):
    """Returns the mean residues per turn value for an AMPAL object."""
    rpt_lists = [isambard.analyse_protein.residues_per_turn(ch)[:-1] for ch in ampal]
    rpt_values = list(itertools.chain(*rpt_lists))
    average_rpt = sum(rpt_values)/len(rpt_values)
    return average_rpt
