"""Module for building models using ISAMBARD."""

import enum
import itertools
import sys

import isambard
import isambard.add_ons.knobs_into_holes as kihs


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
    coiled_coil.z_shifts = [p['Z-Shift'] for p in parameters]
    lshr_adjust = [(p['Z-Shift'] / p['Pitch']) * 360 if p['Linked SHR'] else 0
                   for p in parameters]
    coiled_coil.rotational_offsets = [d + p['Super-Helical Rotation'] - a
                                      for d, p, a in zip(
                                          coiled_coil.rotational_offsets,
                                          parameters,
                                          lshr_adjust)]
    coiled_coil.orientations = [-1 if p['Orientation']
                                else 1 for p in parameters]
    coiled_coil.build()
    coiled_coil.pack_new_sequences(sequences)
    mean_rpt_value = calculate_average_rpt(coiled_coil)
    score = coiled_coil.buff_interaction_energy.total_energy
    knobs = [k.knob_residue for k in kihs.find_kihs(coiled_coil)]
    knob_ids = [[k.ampal_parent.id, k.id] for k in knobs]
    return coiled_coil.pdb, score, mean_rpt_value, knob_ids


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
    original_sequences = [p['Sequence'] for p in parameters]
    proline_sequences = [seq.replace('O', 'P') for seq in original_sequences]
    collagen.z_shifts = [
        d + p['Z-Shift'] for d, p in zip(
            collagen.z_shifts, parameters)]
    lshr_adjust = [(z / p['Pitch']) * 360 if p['Linked SHR'] else 0
                   for z, p in zip(collagen.z_shifts, parameters)]
    collagen.rotational_offsets = [d + p['Super-Helical Rotation'] + a
                                   for d, p, a in zip(
                                       collagen.rotational_offsets,
                                       parameters,
                                       lshr_adjust)]
    collagen.orientations = [-1 if p['Orientation']
                             else 1 for p in parameters]
    collagen.build()
    collagen.pack_new_sequences(proline_sequences)
    score = collagen.buff_interaction_energy.total_energy
    mean_rpt_value = calculate_average_rpt(collagen)
    for res, ml in zip(collagen.get_monomers(), ''.join(original_sequences)):
        if ml == 'O':
            isambard.ampal.non_canonical.convert_pro_to_hyp(res)
    knob_ids = []  # Collagen can't have KIHs
    return collagen.pdb, score, mean_rpt_value, knob_ids


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
    knobs = [k.knob_residue for k in kihs.find_kihs(top_model)]
    knob_ids = [[k.ampal_parent.id, k.id] for k in knobs]
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
        'score': top_model.buff_interaction_energy.total_energy,
        'knob_ids': knob_ids
    }
    return optimised_parameters, model_and_info


def optimise_collagen(parameters, debug=False):
    """Optimises the parameters for a collagen structure.

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
    radius_centre = parameters[0]['Radius']
    phi_centre = parameters[0]['Interface Angle']
    opt = isambard.optimisation.GA_Opt(OptCollagen)
    opt.parameters(
        [parameters[0]['Sequence']] * 3,
        [radius_centre, 150, phi_centre],
        [2, 100, 20],
        [len(parameters[0]['Sequence']), 'var0', 'var1', 'var2']
    )
    opt.run_opt(20, 5, 2)
    top_model = opt.best_model
    knob_ids = []
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
        'score': top_model.buff_interaction_energy.total_energy,
        'knob_ids': knob_ids
    }
    return optimised_parameters, model_and_info


def calculate_average_rpt(ampal):
    """Returns the mean residues per turn value for an AMPAL object."""
    rpt_lists = [isambard.analyse_protein.residues_per_turn(ch)[
        :-1] for ch in ampal]
    rpt_values = list(itertools.chain(*rpt_lists))
    average_rpt = sum(rpt_values) / len(rpt_values)
    return average_rpt


class OptCollagen(isambard.specifications.CoiledCoil):
    """A wrapper class for building collagens with simple parameters."""
    def __init__(self, aas, major_radius, major_pitch, interface_angle):
        n = 3
        super().__init__(n, auto_build=False)
        self.aas = [aas] * n
        self.phi_c_alphas = [interface_angle] * n
        self.minor_helix_types = ['PPII'] * n
        self.major_pitches = [major_pitch] * n
        self.major_radii = [major_radius] * n
        self.major_handedness = ['r'] * n
        self.minor_repeats = [None] * n
        rpr_ppii = 3.1
        self.z_shifts = [-rpr_ppii * 2, 0.0, -rpr_ppii]
        rot_adj = [(z / p) * 360 for z,
                   p in zip(self.z_shifts, self.major_pitches)]
        self.rotational_offsets = [r + ra for r, ra in zip(
            self.rotational_offsets, rot_adj)]
        self.build()


class HelixType(enum.Enum):
    """Possible helix types that can be encountered."""
    ALPHA = 1
    COLLAGEN = 2
