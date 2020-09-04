# HMcode

This code produces the ```HMcode``` non-linear matter power spectrum using the augmented halo-model approach described in Mead (2020; xxxx.xxxx). It can also produce ```HMcode``` results from Mead et al. (2016; http://arxiv.org/abs/1602.02154) or from Mead et al. (2015; http://arxiv.org/abs/1505.07833). Appendix B of the 2015 paper details the numerical methods used in the calculation. If you use this work, or this code, I would be very grateful if you were to cite the relevant papers. For the enthusiast, the code itself can also be cited: http://ascl.net/1508.001.

```HMcode``` should compile with any ```Fortran``` compiler, and you can change the compiler within the ```Makefile```. ```HMcode``` requires my library (https://github.com/alexander-mead/library) to be downloaded in order to work and you will need to change the ```Makefile``` to point to the ```src``` directory within the directory (e.g., ```MOD_DIR = /path/to/library/src``` in the ```Makefile```) where you have downloaded the library. Note well that you do **not** need to compile the library in order for ```HMcode``` to compile -- ```HMcode``` just needs to know the location of the source files within the library. Once the required edits to the ```Makefile``` have been implemented, simply type ```>make```. To run the compiled code type ```>./bin/HMcode```.

Initally the code fills up arrays for the wavenumbers, *k*, and scale-factors, *a*, for which the power spectrum is required. The code then calls the subroutine ```assign_cosmology```, which sets the cosmological parameters - if you wish to change cosmological parameters then this needs to be done after this subroutine, but before ```init_cosmology``` runs. The code then calls the ```calculate_HMcode``` routine to do the actual augmented halo-model calculation and finally writes results using the ```write_power_a``` routine. The data file is written to ```data/power.dat```: the first line starts with ### and then lists the scale factors. The first column is the wavenumbers and subsquent columns are values of the power spectrum at the corresponding *k* and *a* values. These can be checked against the included ```data/power_example.dat``` file to check that they agree.

There are different options for the ```version``` that you can use: either ```HMcode2020_feedback```, ```HMcode2020```,```HMcode2016``` or ```HMcode2015```. In all cases the linear power is calculated from the approximate Eistenstein & Hu (1998; astro-ph/9709112) fitting function. This is accurate at only around the 5% level, with particular inaccuracy around the BAO scale. If this accuracy is not sufficient for your needs then you should use either the version of HMcode that is included within ```CAMB``` (https://github.com/cmbant/CAMB) or that within ```CLASS``` (http://class-code.net/). 

Using HMcode within ```CAMB``` or ```CLASS``` is also the only way to get results for massive-neutrino cosmologies, because I could not find a suitably accurate fitting function for the linear matter power spectrum in the presence of massive neutrinos. If you know of one, and if this would be useful for your work, then please let me know.

```HMcode2016``` is compatabile with DGP and f(R) modified gravity cosmologies as detailed in that paper. These can be activated by setting the ```cosm%img``` flag and then setting relevant modified-gravity parameters in the code. Look in ```cosmology_functions.f90``` to see examples of how to do this.

In testing I was able to get the power at 16 *a* and 128 *k* points in 0.15 seconds for a regular LCDM cosmology using ```gfortran``` on a 2018 Macbook with -O3 optimisation. 

The ```gnuplot``` script ```power.p``` in the ```plot``` directory can be used to plot the output. It can be run using ```gnuplot > load 'plot/power.p```.

Please let me know if you need any help running the code. Or if you have any questions whatsoever.

Alexander Mead
(alexander.j.mead@googlemail.com)

=== UPDATES ===

2020/07/03:
Complete rewrite of code. Support for ```HMcode2020```, ```HMcode2016``` and ```HMcode2015``` versions. Enabled support for modified gravity models for the ```HMcode2016``` version. New dependence on my library: https://github.com/alexander-mead/library. The old repository has been archived and can be found at https://github.com/alexander-mead/HMcode-old.

2018/02/14:
Added support for a standard two-halo term. This can be activated by setting ```ihm=3``` in the code. Now ```ihm=1``` is the accurate calculation whereas ```ihm=2``` is the standard calculation but with a linear theory two-halo term. The variable ```imead``` has been removed. There is a new logical ```verbose```. Also added option ```ihm=0``` to do linear theory only.

2018/01/18:
Added support for an input linear spectrum from ```CAMB```. This can be input via the command line as described above.

2016/08/02:
Small update to the README and some very minor fixes in the code to facilitate direct comparisons with other halomodel power codes.

2016/02/04:
Included updates from Mead et al. (2016) including parameterising the two-halo damping term in terms of f(sigma_v) and slightly recalibrated values of *alpha* and *f_damp*. Now works for w(a)CDM models, where *w(a)=w_0+(1.-a)*w_a*.

2016/01/23:
Updated the code a little so that it no longer calculates a range in nu and simply converts a mass range into a nu range to do the integral. The mass range is fixed from haloes of *1e2* to *1e18* Solar masses, it is difficult to imagine an application of this code where this halo mass range would not be sufficient. This further helps when considering strange cosmological models at high redshift that suffer from a lack of haloes, for these models doing a *nu* to *M* inversion occasionally reached incredibly tiny halo masses that contribute negligbly to the power spectrum on cosmological scales due to their tiny sizes.

2015/07/07:
One user reported crashes that occured for quite extreme cosmological models (*n_s < 0.5*, *sig8 < 0.3*, *z>5*). I have fixed this rather crudely by adding IF statements that catch problems (which manifest themsevles as extreme parameter values). The physical reason for these problems is that models with these odd cosmological parameters have *R_nl << 1 Mpc* and so have very few haloes. Some of the routines I was using previously had assumed that *R_nl* would not be so tiny.
