import numpy as np

from pyhmcode.hmx import HMcode2015 as HMcode2015
from pyhmcode.hmx import HMcode2016 as HMcode2016
from pyhmcode.hmx import HMcode2020 as HMcode2020
from pyhmcode.hmx import HMcode2020_feedback as HMcode2020_feedback

from pyhmcode.hmx import HMx2020_matter_pressure_w_temp_scaling as HMx2020_matter_pressure_w_temp_scaling
from pyhmcode.hmx import HMx2020_matter_w_temp_scaling as HMx2020_matter_w_temp_scaling

from pyhmcode.hmx import field_dmonly as field_dmonly
from pyhmcode.hmx import field_matter as field_matter
from pyhmcode.hmx import field_cdm as field_cdm
from pyhmcode.hmx import field_gas as field_gas
from pyhmcode.hmx import field_stars as field_stars
from pyhmcode.hmx import field_electron_pressure as field_electron_pressure

class Cosmology(pyhmcode.cosmology_functions.cosmology):
    def __init__(self):
        super().__init__()

        # Assign reasonable default parameters (icosmo=1 == "boring cosmology")
        pyhmcode.cosmology_functions.assign_cosmology(icosmo=1, cosm=self, verbose=False)

        self.norm_method = pyhmcode.cosmology_functions.norm_none
        self.itk = pyhmcode.cosmology_functions.iTk_external
        self.has_power = True

        self._has_linear_power_set = False

    def set_linear_power_spectrum(self, k, z, pofk_lin):
        if pofk_lin.shape != (len(z), len(k)):
            raise ValueError(f"Shape mismatch between k, z, pofk_lin: {k.shape}, {z.shape}, {pofk_lin.shape}")
        if z[0] >= z[-1]:
            raise ValueError("Redshifts need to be in increasing order.")

        pk_lin = np.asfortranarray(pofk_lin[::-1].T, dtype=np.float64)
        self.a_lin = np.asfortranarray((1/(1+z))[::-1], dtype=np.float64)
        self.k_lin = np.asfortranarray(k, dtype=np.float64)

        pyhmcode.cosmology_functions.init_external_linear_power_tables(self, self.k_lin, self.a_lin, pk_lin)
        pyhmcode.cosmology_functions.init_cosmology(self)

        self._has_linear_power_set = True

class Halomodel(pyhmcode.hmx.halomod):
    def __new__(cls, mode, verbose=False):
        # Need to use __new__ to intercept creation of the instance and use
        # assign_halomod instead
        instance = pyhmcode.hmx.assign_halomod(mode, verbose)
        return instance



def calculate_nonlinear_power_spectrum(cosmology, halomodel, fields=np.array([field_dmonly]),
                                       return_halo_terms=False, verbose=False):
    if not cosmology._has_linear_power_set:
        raise RuntimeError("Cosmology has no linear power spectrum set.")

    nk = len(cosmology.k_lin)
    na = len(cosmology.a_lin)
    nf = len(fields)

    # Output arrays
    pow_lin = np.zeros((nk, na), dtype=np.float64, order="F")
    pow_2h = np.zeros((nf, nf, nk, na), dtype=np.float64, order="F")
    pow_1h = np.zeros((nf, nf, nk, na), dtype=np.float64, order="F")
    pow_hm = np.zeros((nf, nf, nk, na), dtype=np.float64, order="F")

    pyhmcode.hmx.calculate_hmx_old(ifield=fields, nf=nf, 
                                   k=cosmology.k_lin, nk=nk, 
                                   a=cosmology.a_lin, na=na, 
                                   pow_li=pow_lin, 
                                   pow_2h=pow_2h, pow_1h=pow_1h, pow_hm=pow_hm, 
                                   hmod=halomodel, cosm=cosmology, verbose=verbose)

    pofk_hmc = np.swapaxes(pow_hm[...,::-1], 2, 3) / (cosmology.k_lin**3/(2*np.pi**2))

    if return_halo_terms:
        pofk_1h = pow_1h[:,:,:,::-1].T / cosmology.k_lin**3/(2*np.pi**2)
        pofk_2h = pow_2h[:,:,:,::-1].T / cosmology.k_lin**3/(2*np.pi**2) 
        return pofk_hmc, pofk_1h, pofk_2h
    else:
        return pofk_hmc
