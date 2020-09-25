import numpy as np

import camb

import contextlib
import time


import pyhmcode

@contextlib.contextmanager
def timer():
    t = []
    t.append(time.perf_counter())
    yield t
    t[-1] = time.perf_counter() - t[-1]

def test_wrapper():
    c = pyhmcode.cosmology_functions.cosmology()

    pyhmcode.cosmology_functions.assign_cosmology(1, c, False)
    #pyhmcode.cosmology_functions.print_cosmology(c)

    assert np.isclose(c.om_m, 0.3)
    assert np.isclose(c.om_b, 0.05)
    assert np.isclose(c.om_v, 0.7)
    assert np.isclose(c.h, 0.7)

    # print("Om_m:", c.om_m)
    # print("Om_b:", c.om_b)
    # print("h:", c.h)

    c.om_m = 0.31
    #pyhmcode.cosmology_functions.print_cosmology(c)

def test_power_spectrum(plot=False):
    camb.set_feedback_level(0)

    # Cosmological parameters for CAMB
    h = 0.7
    omc = 0.25
    omb = 0.048
    mnu = 0.06
    w = -1.0
    wa = 0.0
    ns = 0.97
    As = 2.1e-9

    halo_model = {"eta0" : 0.603,
                  "As"   : 3.13}
    z_max = 2.0
    n_z = 100

    k_max = 20.0
    z_lin = np.linspace(0, z_max, n_z, endpoint=True)
    
    # Get linear power spectrum
    # Set up CAMB
    p = camb.CAMBparams(WantTransfer=True, 
                        WantCls=False, 
                        Want_CMB_lensing=False, 
                        DoLensing=False,
                        NonLinearModel=camb.nonlinear.Halofit(halofit_version="mead", 
                        HMCode_A_baryon=halo_model["As"], HMCode_eta_baryon=halo_model["eta0"]))
    p.set_cosmology(H0=h*100, omch2=omc*h**2, ombh2=omb*h**2, mnu=mnu)
    p.set_dark_energy(w=w, wa=wa)
    p.set_initial_power(camb.InitialPowerLaw(As=As, ns=ns))

    p.set_matter_power(redshifts=z_lin, kmax=k_max, nonlinear=True)

    r = camb.get_results(p)
    k_lin, z_lin, pofk_lin_camb = r.get_linear_matter_power_spectrum(nonlinear=False)

    Pk_nl_CAMB_interpolator = r.get_matter_power_interpolator()
    pofk_nonlin_camb = Pk_nl_CAMB_interpolator.P(z_lin, k_lin, grid=True)

    sigma8 = r.get_sigma8()[-1]

    omv = r.omega_de + r.get_Omega("photon") + r.get_Omega("neutrino")
    omm = p.omegam

    c = pyhmcode.Cosmology()

    c.om_m = omm
    c.om_b = omb
    c.om_v = omv
    c.h = h

    c.ns = ns
    c.sig8 = sigma8
    c.m_nu = mnu

    c.set_linear_power_spectrum(k_lin, z_lin, pofk_lin_camb)

    hmod = pyhmcode.Halomodel(pyhmcode.HMcode2016, verbose=False)
    hmod.As = halo_model["As"]
    hmod.eta0 = halo_model["eta0"]

    pofk_hmc = pyhmcode.calculate_nonlinear_power_spectrum(c, hmod, verbose=True)

    if plot:
        import matplotlib.pyplot as plt
        import matplotlib.colorbar

        cmap = plt.get_cmap("magma_r")
        fig, ax = plt.subplots(2, 1)
        fig.subplots_adjust(left=0.2, right=0.95)

        cb_ax = matplotlib.colorbar.make_axes(ax)
        norm = matplotlib.colors.Normalize(vmin=z_lin[0], vmax=z_lin[-1])
        cb1 = matplotlib.colorbar.ColorbarBase(cb_ax[0], cmap=cmap,
                                        norm=norm, **cb_ax[1])
        cb1.set_label('z')

        _ = [ax[0].loglog(k_lin, pofk_hmc[i], 
                         c=cmap(i/len(z_lin))) for i in range(len(z_lin))]
        _ = [ax[0].loglog(k_lin, pofk_nonlin_camb[i], 
                         ls="--", c=cmap(i/len(z_lin))) for i in range(len(z_lin))]
        ax[0].set_title("Non-linear power spectrum")
        ax[0].set_xlabel("k [h/Mpc]")
        ax[0].set_ylabel("P(k)")


        _ = [ax[1].semilogx(k_lin, pofk_hmc[i]/pofk_nonlin_camb[i] - 1, 
                         c=cmap(i/len(z_lin))) for i in range(len(z_lin))]
        ax[1].set_title("Non-linear power spectrum")
        ax[1].set_xlabel("k [h/Mpc]")
        ax[1].set_ylabel("HMCode/CAMB-1")

        plt.show()


if __name__ == "__main__":
    test_wrapper()
    test_power_spectrum(True)

