"""Module for building models using ISAMBARD."""

import itertools
import sys

import isambard


REGISTER_ADJUST = {
    'a': 0,
    'b': 102.8,
    'c': 205.6,
    'd': 308.4,
    'e': 51.4,
    'f': 154.2,
    'g': 257
}


def build_coiled_coil(parameters, debug=False):
    """Builds a model of a coiled coil using the input parameters.

    Parameters
    ----------
    parameter : [ dict[str, int/float/str] ]
        List of parameter dictionaries required for building a coiled coil
        i.e. oligomer state, radius, pitch, phiCA, sequence.

    Returns
    -------
    model_data : dict
        A dictionary containing information about the model that has
        been produced.
    """
    if debug:
        print(parameters, file=sys.stderr)
    coiled_coil = isambard.specifications.CoiledCoil(
        len(parameters), auto_build=False)
    coiled_coil.aas = [len(p['Sequence']) for p in parameters]
    coiled_coil.major_radii = [p['Radius'] for p in parameters]
    coiled_coil.major_pitches = [p['Pitch'] for p in parameters]
    raw_phi = [p['Interface Angle'] for p in parameters]
    registers = [p['Register'] for p in parameters]
    coiled_coil.phi_c_alphas = [ia + REGISTER_ADJUST[r]
                                for ia, r in zip(raw_phi, registers)]
    sequences = [p['Sequence'] for p in parameters]
    coiled_coil.rotational_offsets = [
        d + p['Super-Helical Rotation'] for d, p in zip(
            coiled_coil.rotational_offsets, parameters)]
    coiled_coil.orientations = [-1 if p['Orientation']
                                else 1 for p in parameters]
    coiled_coil.z_shifts = [p['Z-Shift'] for p in parameters]
    coiled_coil.build()
    coiled_coil.pack_new_sequences(sequences)
    mean_rpt_value = calculate_average_rpt(coiled_coil)
    score = coiled_coil.buff_interaction_energy.total_energy
    return coiled_coil.pdb, score, mean_rpt_value


def optimise_coiled_coil(parameters, debug=False):
    """Optimises the parameters for a given structure.

    Parameters
    ----------
    parameter : [ dict[str, int/float/str] ]
        List of parameter dictionaries required for building a coiled coil i.e.
        oligomer state, radius, pitch, phiCA, sequence.

    Returns
    -------
    model_data : dict
        A dictionary containing information about the model that has
        been produced.
    """
    if debug:
        print(parameters, file=sys.stderr)
    oligomer_state = len(parameters)
    radius_centre = parameters[0]['Radius']
    phi_centre = parameters[0]['Interface Angle'] + \
        REGISTER_ADJUST[parameters[0]['Register']]
    opt = isambard.optimisation.GA_Opt(
        isambard.specifications.CoiledCoil.from_parameters)
    opt.parameters(
        [parameters[0]['Sequence']] * oligomer_state,
        [radius_centre, 200, phi_centre],
        [2, 100, 20],
        [oligomer_state, len(parameters[0]['Sequence']),
         'var0', 'var1', 'var2']
    )
    opt.run_opt(20, 5, 2)
    top_model = opt.best_model
    optimised_parameters = {
        'radius': top_model.major_radii[0],
        'pitch': top_model.major_pitches[0],
        'phiCA': top_model.phi_c_alphas[0],
        'sequence': top_model[0].sequence,
        'register': parameters[0]['Register']
    }
    model_and_info = {
        'pdb': top_model.pdb,
        'mean_rpt_value': calculate_average_rpt(top_model),
        'score': top_model.buff_interaction_energy.total_energy
    }
    return optimised_parameters, model_and_info


def build_collagen(parameters, debug=False):
    """Builds a model of a collagen triple-helix using the input parameters.

    Parameters
    ----------
    parameter : [ dict[str, int/float/str] ]
        List of parameter dictionaries required for building a coiled coil
        i.e. radius, pitch, phiCA, sequence.

    Returns
    -------
    model_data : dict
        A dictionary containing information about the model that has
        been produced.
    """
    if debug:
        print(parameters, file=sys.stderr)
    collagen = isambard.specifications.CoiledCoil.tropocollagen(
        auto_build=False)
    collagen.aas = [len(p['Sequence']) for p in parameters]
    collagen.major_radii = [p['Radius'] for p in parameters]
    collagen.major_pitches = [p['Pitch'] for p in parameters]
    collagen.phi_c_alphas = [p['Interface Angle'] for p in parameters]
    sequences = [p['Sequence'] for p in parameters]
    collagen.build()
    collagen.pack_new_sequences(sequences)
    mean_rpt_value = calculate_average_rpt(collagen)
    score = collagen.buff_interaction_energy.total_energy
    return collagen.pdb, score, mean_rpt_value


def calculate_average_rpt(ampal):
    """Returns the mean residues per turn value for an AMPAL object."""
    rpt_lists = [isambard.analyse_protein.residues_per_turn(ch)[
        :-1] for ch in ampal]
    rpt_values = list(itertools.chain(*rpt_lists))
    average_rpt = sum(rpt_values) / len(rpt_values)
    return average_rpt
