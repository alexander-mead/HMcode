# HMcode

This code is produces the non-linear matter power spectrum using the halomodel approach described in Mead et al. (2015; http://arxiv.org/abs/1505.07833). Appendix B of that paper details the methods for doing the calculation. It also includes some small updates from Mead et al. (2016; http://arxiv.org/abs/1602.02154). If you use this work, or this code, I would be very grateful if you were to cite the original paper, and the updated paper if you use results from that. The code itself can also be cited: http://ascl.net/1508.001.

The code should compile with any fortran compiler, and does not need to be pointed to any libraries. I use ```ifort``` and compile with ```ifort HMcode.f90``` but it also works with gfortran.

When it starts, the code fills up some arrays for the wavenumber *k* and redshift *z* values for which the user wants the power spectrum. The code then calls the subroutine ```assign_cosmology```, which sets the cosmological parameters - if you wish to change these, then this needs to be done in this subroutine. The code then has two options for the linear power spectum:

1 - By default, the code uses the Eistenstein & Hu (1998) approximation for the linear power spectrum (good to *~5%*, main deviations around BAO scale). The code then calls 'initialise_cosmology', which normalises the power spectrum to the correct *sigma_8* and fills up look-up tables that contain the linear growth function and *sigma(R)* for later use. If your binary is called ```HMcode``` thens simply typing ```HMcode``` on the command line defaults to this behaviour.

2 - The user can instead provide an input linear matter power spectrum from ```CAMB``` via the command line followed by the redshift. If your binary file is called ```HMcode``` then do ```HMcode CAMB_matterpower.dat 1``` for a matter power spectrum at *z=1*. Note that HMcode will use the cosmological parameters in the code to calculate the growth functions and other things, so these should be manually set to the same cosmological parameters that were used to generate the ```CAMB``` linear spectrum. Note that the code assumes that linear growth is scale-independent, and you will get bogus results if your model has scale-dependent linear growth. Also note that in this case HMcode will not normalise your input linear spectrum, so the *sigma_8* value set in the code will not be used, so you should provide your ```CAMB``` spectrum with the correct normalisation. Also note that the code will extrapolate your linear power as a power-law beyond the minimum and maximum values of *k* for some calculations, so you should provide a linear power spectrum for which this extrapolation is valid: ~ *0.001<k<10*, but you should check for your particular calculation!

The option ```ihm``` can be changed in the code:

```ihm=0``` - Linear theory only. Useful for testing and comparisons.

```ihm=1``` - (Default) performs the accurate calculation from Mead et al., detailed in Appendix B of http://arxiv.org/abs/1505.07833 

```ihm=2``` - the code performs the standard halo model calculation (*Dv=200*, *dc=1.686*, Sheth & Tormen (1999) mass function, Bullock (2001) concentration-mass relation; although it neglects the standard bias factors in the two-halo term, because this is not important for the matter spectrum)

```ihm=3``` - Similar to ```ihm=2``` but including a two-halo term with the linear halo bias terms

The code then loops through redshift *z* (outer loop) and wavenumber *k* (inner loop) producing power at each redshift and wave number. The ordering of loops (*z* then *k*) is important because for each new redshift the code needs to call ```halomod_init``` to fill up redshift-dependent look-up tables for the halomodel calculation. These look-up tables contain various redshift-dependent halo properties, such as mass, virial radius, concentration etc. which are then used in the one-halo integral.

Once these look-up tables have been filled the halomodel integral can then be carried out. This calculation is done by the routine 'halomod', which calls ```p_1h``` and ```p_2h``` to evaluate 1- and 2-halo terms and then uses these to compute the full power spectrum. The power spectrum at each k and z is then added to an array which is printed out to ```power.dat``` (*k*, *power(z1)*, *power(z2)*, ...) when the code finishes.

In testing I was able to get 16 redshifts, with 200 *k*-points, in 0.3 seconds (using gfortran on a 2015 Macbook with -O3 optimisation). 

The gnuplot script ```power.p``` that can be used to plot the output. It can be run using ```gnuplot > load 'power.p```.

Please let me know if you need any help running the code. Or if you have any questions whatsoever.

HMcode is now also integrated within ```CAMB``` (https://github.com/cmbant/CAMB), and this includes support for massive-neutrino models. I suggest using the version within ```CAMB``` preferentially.

Alexander Mead
(mead@phas.ubc.ca)

UPDATE: 2015/07/07
One user reported crashes that occured for quite extreme cosmological models (*n_s < 0.5*, *sig8 < 0.3*, *z>5*). I have fixed this rather crudely by adding IF statements that catch problems (which manifest themsevles as extreme parameter values). The physical reason for these problems is that models with these odd cosmological parameters have *R_nl << 1 Mpc* and so have very few haloes. Some of the routines I was using previously had assumed that *R_nl* would not be so tiny.

UPDATE: 2016/01/23
Updated the code a little so that it no longer calculates a range in nu and simply converts a mass range into a nu range to do the integral. The mass range is fixed from haloes of *1e2* to *1e18* Solar masses, it is difficult to imagine an application of this code where this halo mass range would not be sufficient. This further helps when considering strange cosmological models at high redshift that suffer from a lack of haloes, for these models doing a *nu* to *M* inversion occasionally reached incredibly tiny halo masses that contribute negligbly to the power spectrum on cosmological scales due to their tiny sizes.

UPDATE: 2016/02/04
Included updates from Mead et al. (2016) including parameterising the two-halo damping term in terms of f(sigma_v) and slightly recalibrated values of *alpha* and *f_damp*. Now works for w(a)CDM models, where *w(a)=w_0+(1.-a)*w_a*.

UPDATE: 2016/08/02
Small update to the README and some very minor fixes in the code to facilitate direct comparisons with other halomodel power codes.

UPDATE: 2018/01/18
Added support for an input linear spectrum from ```CAMB```. This can be input via the command line as described above.

UPDATE: 2018/02/14
Added support for a standard two-halo term. This can be activated by setting ```ihm=3``` in the code. Now ```ihm=1``` is the accurate calculation whereas ```ihm=2``` is the standard calculation but with a linear theory two-halo term. The variable ```imead``` has been removed. There is a new logical ```verbose```. Also added option ```ihm=0``` to do linear theory only.
