# HMcode

This code produces the `HMcode` non-linear matter power spectrum using the augmented halo-model approach described in Mead (2020; https://arxiv.org/abs/2009.01858). It can also produce `HMcode` results from Mead et al. (2016; https://arxiv.org/abs/1602.02154) or from Mead et al. (2015; https://arxiv.org/abs/1505.07833). Appendix B of the 2015 paper details the numerical methods used in the calculation. If you use this work, or this code, I would be very grateful if you were to cite the relevant papers. For the enthusiast, the code itself can also be cited: http://ascl.net/1508.001.

Clone the repository using
```
git clone --recursive https://github.com/alexander-mead/HMcode
```
the `--recursive` is important because that will also ensure that necessary libraries are cloned in to the `library/` subdirectory. `HMcode` can then be compiled using `make`. If you get an error: 
```
*** No rule to make target `build/precision.o', needed by `bin/HMcode'.
```
this is because you did not use the `-- recursive` flag. `HMcode` should compile with any `Fortran` compiler, the default is `gfortran`, but you can change the compiler within the `Makefile` if necessary. To run the compiled code type `./bin/HMcode`.

Six cosmological parameters can be specified via the command line in the order: `Om_m`; `Om_b`; `h`; `ns`; `sig8`; `w`. If these are not specified then they take on default values: `Om_m = 0.30`; `Om_b = 0.05`; `h = 0.70`; `ns = 0.96`; `sig8 = 0.80`; `w = -1.0`. The cosmological model is taken to be flat *w*CDM, with constant *w* and flatness is enforced via the dark-energy density. These restrictions can be relaxed if necessary, and more complicated dark-energy models can be investigated, but adding this would require a small bit of hacking and thought. Please contact me if you are interested in this and have any trouble implementing it yourself. 

In addition, a  `CAMB`-format linear power spectrum (two columns: *k* and *P(k)* with units *[h/Mpc]* and *[(Mpc/h)^3]* respectively, with a single leading *#* comment line) can be provided as a seventh command-line argument. This linear spectrum is taken to be at *z=0* and its amplitude at higher redshifts is calculated assuming a scale-independet growth factor that is calculated via the cosmological parameters. If a linear spectrum is specified in this way then it is assumed to be normalised correctly and the value of `sig8` provided via the command line will be ignored.

Initally the code fills up arrays for the wavenumbers, *k*, and scale-factors, *a*, for which the power spectrum is required. The code then calls the subroutine `assign_cosmology`, which sets the cosmological parameters - if you wish to make additional changes to the cosmological parameters then this needs to be done after `assign_cosmology` has been called, but before `init_cosmology` is called. The code calls the `calculate_HMcode` routine to do the halo-model calculation and finally writes results using the `write_power_a` routine. The data file is written to `data/power.dat`: the first line starts with ### and then lists the scale factors. The first column is the wavenumbers and subsquent columns are values of the power spectrum at the corresponding *k* and *a* values. These can be checked against the included `data/power_example.dat` file to check that they agree.

There are different options for the `version`: either `HMcode2020_feedback`, `HMcode2020`, `HMcode2016` or `HMcode2015`. By default the linear power is calculated from the approximate Eistenstein & Hu (1998; astro-ph/9709112) fitting function, which is accurate at only around the 5% level, with particular inaccuracy around the BAO scale. If this accuracy is not sufficient for your needs then you should use either the version of `HMcode` that is included within `CAMB` (https://github.com/cmbant/CAMB),that within `CLASS` (http://class-code.net/), or else specify a linear spectrum via the command line as described above. 

Using HMcode within `CAMB` or `CLASS` is also the only way to get results for massive-neutrino cosmologies, because I could not find a suitably accurate fitting function for the linear matter power spectrum in the presence of massive neutrinos. If you know of one, and if this would be useful for your work, then please let me know.

`HMcode2016` is compatabile with DGP and f(R) modified gravity cosmologies as detailed in the 2016 paper. These can be activated by setting the `cosm%img` flag and then setting relevant modified-gravity parameters in the code. Look in `cosmology_functions.f90` to see examples of how to do this and please contact me if you have any trouble.

In testing I was able to get the power at 16 *a* and 128 *k* points in 0.15 seconds for a regular LCDM cosmology using `gfortran` on a 2018 Macbook with -O3 optimisation. 

The `gnuplot` script `power.p` in the `plot/` directory can be used to plot the output. It can be run using `gnuplot > load 'plot/power.p`.

Please let me know if you need any help running the code. Or if you have any questions whatsoever.

The development of HMcode between 2017 and 2020 was assisted by the Horizon 2020 research and innovation programme of the European Union under Marie Sklodowska-Curie grant agreement No. 702971.

Alexander Mead
(alexander.j.mead@googlemail.com)

=== UPDATES ===

2021/01/14:
Support for cosmological parameters and an external linear spectrum to be provided via the command line.

2020/09/22:
Added library as a `git submodule` so that they do not need to be cloned separately. Also support for `HMcode2020_feedback` was added some time between this update and that documented below.

2020/07/03:
Complete rewrite of code. Lots of options listed below are now suppressed. Support for `HMcode2020`, `HMcode2016` and `HMcode2015` versions. Enabled support for modified gravity models for the `HMcode2016` version. New dependence on my library: https://github.com/alexander-mead/library. The old repository has been archived and can be found at https://github.com/alexander-mead/HMcode-old.

----

=== OLD STUFF ===

2018/02/14:
Added support for a standard two-halo term. This can be activated by setting `ihm=3` in the code. Now `ihm=1` is the accurate calculation whereas `ihm=2` is the standard calculation but with a linear theory two-halo term. The variable `imead` has been removed. There is a new logical `verbose`. Also added option `ihm=0` to do linear theory only.

2018/01/18:
Added support for an input linear spectrum from `CAMB`. This can be input via the command line as described above.

2016/08/02:
Small update to the README and some very minor fixes in the code to facilitate direct comparisons with other halomodel power codes.

2016/02/04:
Included updates from Mead et al. (2016) including parameterising the two-halo damping term in terms of f(sigma_v) and slightly recalibrated values of *alpha* and *f_damp*. Now works for w(a)CDM models, where *w(a)=w_0+(1.-a)*w_a*.

2016/01/23:
Updated the code a little so that it no longer calculates a range in nu and simply converts a mass range into a nu range to do the integral. The mass range is fixed from haloes of *1e2* to *1e18* Solar masses, it is difficult to imagine an application of this code where this halo mass range would not be sufficient. This further helps when considering strange cosmological models at high redshift that suffer from a lack of haloes, for these models doing a *nu* to *M* inversion occasionally reached incredibly tiny halo masses that contribute negligbly to the power spectrum on cosmological scales due to their tiny sizes.

2015/07/07:
One user reported crashes that occured for quite extreme cosmological models (*n_s < 0.5*, *sig8 < 0.3*, *z>5*). I have fixed this rather crudely by adding IF statements that catch problems (which manifest themsevles as extreme parameter values). The physical reason for these problems is that models with these odd cosmological parameters have *R_nl << 1 Mpc* and so have very few haloes. Some of the routines I was using previously had assumed that *R_nl* would not be so tiny.
