PROGRAM HMcode2020

   USE basic_operations
   IMPLICIT NONE
   INTEGER :: icosmo

   CALL read_command_argument(1, icosmo, 'Please specify cosmology')

   CALL HMcode2020_example(icosmo)
   

CONTAINS

   SUBROUTINE HMcode2020_example(icos)

      USE array_operations
      USE cosmology_functions
      USE HMx
      IMPLICIT NONE
      INTEGER, INTENT(INOUT) :: icos
      REAL, ALLOCATABLE :: k(:), a(:)
      REAL, ALLOCATABLE :: Pk(:,:)
      TYPE(cosmology) :: cosm

      REAL, PARAMETER :: kmin = 1e-3
      REAL, PARAMETER :: kmax = 1e2
      INTEGER, PARAMETER :: nk = 128
      REAL, PARAMETER :: amin = 0.2
      REAL, PARAMETER :: amax = 1.0
      INTEGER, PARAMETER :: na = 16
      LOGICAL, PARAMETER :: verbose = .TRUE.
      CHARACTER(len=256), PARAMETER :: outfile = 'data/power.dat'

      CALL fill_array(log(kmin), log(kmax), k, nk)
      k = exp(k)

      CALL fill_array(amin, amax, a, na)
      a = exp(a)

      CALL assign_cosmology(icos, cosm, verbose)
      CALL init_cosmology(cosm)
      CALL print_cosmology(cosm)

      ALLOCATE(Pk(nk, na))
      CALL calculate_HMcode(k, a, Pk, nk, na, cosm)

      CALL write_power_a(k, a, Pk, nk, na, outfile, verbose)

   END SUBROUTINE HMcode2020_example

END PROGRAM HMcode2020
