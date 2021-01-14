PROGRAM HMcode

   USE constants
   USE basic_operations
   USE array_operations
   USE cosmology_functions
   USE HMx
   USE camb_stuff

   IMPLICIT NONE

   INTEGER :: icosmo
   !INTEGER, PARAMETER :: icosmo_default = 1 ! 1 - Boring default cosmology

   !CALL read_command_argument(1, icosmo, 'Please specify cosmology', icosmo_default)

   CALL HMcode_example(icosmo) 

CONTAINS

   SUBROUTINE HMcode_example(icos)
      
      INTEGER, INTENT(INOUT) :: icos
      REAL, ALLOCATABLE :: k(:), a(:)
      REAL, ALLOCATABLE :: Pk(:, :)
      REAL, ALLOCATABLE :: k_lin(:), a_lin(:), Pk_lin(:)
      INTEGER :: nk_lin, na_lin
      CHARACTER(len=256) :: infile
      TYPE(cosmology) :: cosm

      ! Parameters
      INTEGER, PARAMETER :: icos_default = 1
      REAL, PARAMETER :: kmin = 1e-3
      REAL, PARAMETER :: kmax = 1e2
      INTEGER, PARAMETER :: nk = 128
      REAL, PARAMETER :: amin = 0.2
      REAL, PARAMETER :: amax = 1.0
      INTEGER, PARAMETER :: na = 16
      LOGICAL, PARAMETER :: verbose = .TRUE.
      CHARACTER(len=256), PARAMETER :: outfile = 'data/power.dat'

      ! Different HMcode versions
      !INTEGER, PARAMETER :: version = HMcode2015
      !INTEGER, PARAMETER :: version = HMcode2016
      INTEGER, PARAMETER :: version = HMcode2020
      !INTEGER, PARAMETER :: version = HMcode2020_feedback

      ! Fill arrays with desired k and a points
      CALL fill_array_log(kmin, kmax, k, nk)
      CALL fill_array(amin, amax, a, na)

      ! Initial white space
      WRITE(*, *)

      ! Assign cosmological model
      icos = icos_default
      CALL assign_cosmology(icos, cosm, verbose)

      ! Set cosmological parameters
      CALL read_command_argument(1, cosm%Om_m, '', def=cosm%Om_m)
      CALL read_command_argument(2, cosm%Om_b, '', def=cosm%Om_b)
      CALL read_command_argument(3, cosm%h, '', def=cosm%h)
      CALL read_command_argument(4, cosm%ns, '', def=cosm%ns)
      CALL read_command_argument(5, cosm%sig8, '', def=cosm%sig8)
      CALL read_command_argument(6, cosm%w, '', def=cosm%w)
      cosm%Om_w = 1.-cosm%Om_m ! Set dark-energy density assuming flatness
      cosm%iw = iw_wCDM        ! Set to wCDM dark energy
      cosm%Om_v = 0.           ! Force vacuum energy density to zero (note that DE density is non-zero)

      ! Read in linear spectrum if provided
      CALL read_command_argument(7, infile, '', def='')
      IF (infile /= '') THEN
         CALL read_CAMB_Pk(k_lin, Pk_lin, nk_lin, infile, verbose)
         na_lin = 1
         ALLOCATE(a_lin(na_lin))
         a_lin = 1.         
         Pk_lin = Pk_Delta(Pk_lin, k_lin)
         CALL init_external_linear_power_tables(cosm, k_lin, a_lin, reshape(Pk_lin, [nk_lin, 1]))
      END IF 

      !! ================================================ !!
      !! You can ONLY change cosmological parameters here !!
      !! AFTER assign_cosmology but BEFORE init_cosmology !!
      ! For example
      !cosm%Om_m = 0.4
      !cosm%h = 0.67
      !cosm%Theat = 10**8.0 ! Will only work if version = HMcode2020_feedback
      !! ================================================ !!

      ! Initialise cosmological model
      CALL init_cosmology(cosm)
      CALL print_cosmology(cosm)

      ! Do the HMcode calculation
      CALL calculate_HMcode(k, a, Pk, nk, na, cosm, version=version)

      ! Write the power to a file
      CALL write_power_a(k, a, Pk, nk, na, outfile, verbose)

   END SUBROUTINE HMcode_example

END PROGRAM HMcode
