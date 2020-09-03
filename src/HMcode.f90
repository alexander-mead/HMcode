PROGRAM HMcode

   USE basic_operations
   USE array_operations
   USE cosmology_functions
   USE HMx

   IMPLICIT NONE

   INTEGER :: icosmo
   INTEGER, PARAMETER :: icosmo_default = 1

   CALL read_command_argument(1, icosmo, 'Please specify cosmology', icosmo_default)

   CALL HMcode_example(icosmo) 

CONTAINS

   SUBROUTINE HMcode_example(icos)
      
      INTEGER, INTENT(INOUT) :: icos
      REAL, ALLOCATABLE :: k(:), a(:)
      REAL, ALLOCATABLE :: Pk(:,:)
      TYPE(cosmology) :: cosm

      ! Parameters
      REAL, PARAMETER :: kmin = 1e-3
      REAL, PARAMETER :: kmax = 1e2
      INTEGER, PARAMETER :: nk = 128
      REAL, PARAMETER :: amin = 0.2
      REAL, PARAMETER :: amax = 1.0
      INTEGER, PARAMETER :: na = 16
      LOGICAL, PARAMETER :: verbose = .TRUE.
      CHARACTER(len=256), PARAMETER :: outfile = 'data/power.dat'
      INTEGER, PARAMETER :: version = HMcode2020

      ! Fill arrays with desired k and a points
      CALL fill_array_log(kmin, kmax, k, nk)
      CALL fill_array(amin, amax, a, na)

      ! Initial white space
      WRITE(*, *)

      ! Assign cosmological model
      CALL assign_cosmology(icos, cosm, verbose)

      !! ================================================ !!
      !! You can ONLY change cosmological parameters here !!
      !! AFTER assign_cosmology but BEFORE init_cosmology !!
      ! For example
      !cosm%Om_m = 0.4
      !cosm%h = 0.67
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
