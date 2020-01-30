PROGRAM HMcode2020

   IMPLICIT NONE

   CALL HMcode2020_example()

CONTAINS

   SUBROUTINE HMcode2020_example()

      USE array_operations
      USE cosmology_functions
      USE HMx
      IMPLICIT NONE
      REAL, ALLOCATABLE :: k(:), a(:)
      REAL, ALLOCATABLE :: Pk(:,:)
      TYPE(cosmology) :: cosm

      INTEGER :: icosmo = -1
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

      CALL assign_cosmology(icosmo, cosm, verbose)
      CALL init_cosmology(cosm)
      CALL print_cosmology(cosm)

      ALLOCATE(Pk(nk, na))
      CALL calculate_HMcode(k, a, Pk, nk, na, cosm)

      CALL write_power_a(k, a, Pk, nk, na, outfile, verbose)

   END SUBROUTINE HMcode2020_example

END PROGRAM HMcode2020
