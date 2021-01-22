import numpy as np

import camb
import pyhmcode

def massfunction_params(plot=False):
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

    n_z = 4

    k_max = 20.0
    z_lin = np.linspace(0, 2.0, n_z)
    
    # Get linear power spectrum
    # Set up CAMB
    p = camb.CAMBparams(WantTransfer=True, 
                        WantCls=False, 
                        Want_CMB_lensing=False, 
                        DoLensing=False,
                        NonLinearModel=camb.nonlinear.Halofit(halofit_version="mead"))
    p.set_cosmology(H0=h*100, omch2=omc*h**2, ombh2=omb*h**2, mnu=mnu)
    p.set_dark_energy(w=w, wa=wa)
    p.set_initial_power(camb.InitialPowerLaw(As=As, ns=ns))
    p.set_matter_power(redshifts=z_lin, kmax=k_max, nonlinear=True)

    # Compute CAMB results
    r = camb.get_results(p)
    k_lin, z_lin, pofk_lin_camb = r.get_linear_matter_power_spectrum(nonlinear=False)

    Pk_nl_CAMB_interpolator = r.get_matter_power_interpolator()
    pofk_nonlin_camb = Pk_nl_CAMB_interpolator.P(z_lin, k_lin, grid=True)

    sigma8 = r.get_sigma8()[-1]

    omv = r.omega_de + r.get_Omega("photon") + r.get_Omega("neutrino")
    omm = p.omegam

    # Setup HMCode
    c = pyhmcode.Cosmology()

    c.om_m = omm
    c.om_b = omb
    c.om_v = omv
    c.h = h
    c.ns = ns
    c.sig8 = sigma8
    c.m_nu = mnu

    c.set_linear_power_spectrum(k_lin, z_lin, pofk_lin_camb)

    pofk_hmc = {"logT" : [], "A" : []}

    hmod = pyhmcode.Halomodel(pyhmcode.HMcode2016, verbose=False)
    
    A = np.linspace(2.0, 3.13, 7)
    print("Varying halo A")
    for i, As in enumerate(A):
        hmod.As = As
        #hmod.eta0 = 0.98 - 0.12*As
        pofk_hmc["A"].append(pyhmcode.calculate_nonlinear_power_spectrum(c, hmod, verbose=False)[0])

    print("Varying TAGN")

    hmod = pyhmcode.Halomodel(pyhmcode.HMcode2020_feedback, verbose=False)
    logTAGN = np.linspace(7.3, 8.3, 7)
    for i, logT in enumerate(logTAGN):
        c.theat = 10**logT
        pofk_hmc["logT"].append(pyhmcode.calculate_nonlinear_power_spectrum(c, hmod, verbose=True)[0])

    if plot:
        import matplotlib.pyplot as plt
        import matplotlib.colorbar

        cmap = plt.get_cmap("magma_r")

        fig, ax = plt.subplots(2, 1, figsize=(5, 4))
        fig.subplots_adjust(left=0.2, hspace=0.3, right=0.95, bottom=0.1)

        for param_name, ls, param in [("logT", "-", logTAGN), ("A", "--", A)]:
            

            cb_ax = matplotlib.colorbar.make_axes(ax)
            norm = matplotlib.colors.Normalize(vmin=param[0], vmax=param[-1])
            cb1 = matplotlib.colorbar.ColorbarBase(cb_ax[0], cmap=cmap,
                                            norm=norm, **cb_ax[1])
            cb1.set_label(param_name)

            _ = [ax[0].loglog(k_lin, pofk_hmc[param_name][i], 
                            c=cmap(i/len(param))) for i in range(len(param))]
            _ = ax[0].loglog(k_lin, pofk_nonlin_camb[0], ls="--", c="k")
            ax[0].set_title(f"Non-linear power spectrum, vary {param_name}")
            ax[0].set_xlabel("k [h/Mpc]")
            ax[0].set_ylabel("P(k) [Mpc^3 h^-3]")

            _ = [ax[1].semilogx(k_lin, pofk_hmc[param_name][i]/pofk_nonlin_camb[0] - 1, 
                                ls=ls, c=cmap(i/len(param))) for i in range(len(param))]
            ax[1].set_xlabel("k [h/Mpc]")
            ax[1].set_ylabel("HMCode/CAMB-1")

        fig.dpi = 300
        fig.savefig(f"plots/vary_baryon.png")

        plt.show()

if __name__ == "__main__":
    massfunction_params(plot=True)