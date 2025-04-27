! ##############################################################################
!
!  This is version 1.7a, last updated on 4/2/2001.
!  Last minor bug found 6/14/00.
!
!  Copyright David L. Carroll; this code may not be reproduced for sale
!  or for use in part of another code for sale without the express 
!  written permission of David L. Carroll.
!
! ##############################################################################
PROGRAM gafortran
  implicit none

  ! ----------------------------------------------------------------------------
  ! maximum # of individuals, i.e. max population size
  !integer, parameter :: indmax = 200
  integer, parameter :: indmax = 5
  ! maximum # of chromosomes (binary bits) per individual
  !integer, parameter :: nchrmax = 30
  integer, parameter :: nchrmax = 22 
  ! maximum # of parameters which the chromosomes make up
  !integer, parameter :: nparmax = 2
  integer, parameter :: nparmax = 5  
  ! TNX: added for RegCM5
  integer, parameter :: ndommax = 3 ! don's change currently

  ! integer, parameter :: i1 = 1
  ! GA's files
  character(len = *), parameter :: ga_out = "ga.run.log", &
                                   ga_nml = "ga.namelist.in", &
                                   ga_res = "ga.restart"

  ! ----------------------------------------------------------------------------
  ! Global (common) variables
  ! TNX: add parname
  character(len=20),  dimension(nparmax) :: parname
  character(len=200), dimension(ndommax) :: fnml  
  real*8, dimension(nparmax, indmax)     :: parent, child
  real*8, dimension(indmax)              :: fitness 
  real*8, dimension(nparmax)             :: g0, g1, parmax, parmin, pardel
  real*8, dimension(1000000)             :: geni, genavg, genmax

  integer, dimension(nchrmax, indmax)    :: iparent, ichild
  integer, dimension(nparmax)            :: ig2, nposibl
  integer, dimension(nchrmax)            :: ibest

  ! ----------------------------------------------------------------------------
  ! GA input parameter. Defined in namelist file
  integer :: idum, ielite, iend, irestrt, iskip, itourny, &
             iunifrm, kountmx, maxgen, nchild, nchrome, nowrite, &
             nparam, npopsiz
  real*8  :: best, fbar, evals, pcross

  ! ----------------------------------------------------------------------------
  ! TNX: RegCM input parameter. Defined in namelist file
  integer                               :: ndom, gdate1, gdate2
  character(len=10)                     :: calendar
  character(len=99)                     :: dirglob, inpglob, dirout, &
                                           coarse_outdir
  character(len=20), dimension(ndommax) :: domname, iproj, coarse_domname
  character(len=10), dimension(ndommax) :: ssttyp, dattyp
  integer,           dimension(ndommax) :: domlvl, idynamic, jx, iy, kz, &
                                           ibdyfrq, iboudy, ibltyp, idiffu, &
                                           icup_lnd, icup_ocn, ipptls, &
                                           iocnflx, iocnrough, iocnzoq, &
                                           icldfrac, irrtm
  real*8,            dimension(ndommax) :: ds, clon, clat, dt

  ! ----------------------------------------------------------------------------
  ! TNX: Tiedtke input parameter.
  integer   :: iconv
  real*8    :: entrmax,   entrdd,      entrpen_lnd, entrpen_ocn, entrscv,  entrmid, &
               cprcon,    detrpen_lnd, detrpen_ocn, entshalp,    rcuc_lnd, rcuc_ocn, &
               rcpec_lnd, rcpec_ocn,   rhebc_lnd,   rhebc_ocn,   rprc_ocn, rprc_lnd, &
               revap_lnd, revap_ocn,   cmtcape
  logical   :: lmfpen,    lmfmid,      lmfdd,       lepcld,      lmfdudv,  lmfscv, &
               lmfuvdis,  lmftrac,     lmfsmooth,   lmfwstar

  ! ----------------------------------------------------------------------------
  ! Runtime variables
  integer :: i, ig2sum, inext, inextp, ipick, istart, &
             j, kount, mate1, mate2, npossum, ncross
  real*8  :: rand

  ! ----------------------------------------------------------------------------
  ! TNX: Add parname
  namelist /gaparam/ irestrt, npopsiz, nparam, maxgen, &
    idum, pcross, itourny, ielite, iunifrm, nchild, &
    iskip, iend, nowrite, kountmx, parname, parmin, parmax, nposibl

  ! TNX: RegCM namelist
  namelist /regcmparam/ ndom, domname, domlvl, idynamic, jx, iy, kz, &
    iproj, ds, clon, clat, ibdyfrq, ssttyp, dattyp, gdate1, gdate2, calendar, &
    dirglob, inpglob, coarse_outdir, coarse_domname, dt, dirout, &
    iboudy, ibltyp, idiffu, icup_lnd, icup_ocn, ipptls, iocnflx, iocnrough, &
    iocnzoq, icldfrac, irrtm

  ! TNX: Tiedtke namelist
  namelist /tiedtkeparam/ iconv , entrmax , entrdd , entrpen_lnd , &
    entrpen_ocn , entrscv , entrmid , cprcon , detrpen_lnd ,       &
    detrpen_ocn , entshalp , rcuc_lnd , rcuc_ocn , rcpec_lnd ,     &
    rcpec_ocn , rhebc_lnd , rhebc_ocn , rprc_ocn , rprc_lnd ,      &
    revap_lnd , revap_ocn , cmtcape , lmfpen , lmfmid , lmfdd ,    &
    lepcld , lmfdudv , lmfscv , lmfuvdis , lmftrac , lmfsmooth ,   &
    lmfwstar

! ------------------------------------------------------------------------------
! ##############################################################################


  ! ############################################################################
  ! BEGIN

  ! Reading input
  call input

  ! First setting
  call initial(istart, npossum, ig2sum)

  ! ////////////////////////////////////////////////////////////////////////////
  ! TNX: initial for RegCM
  call regcm_initial

  ! Main generational processing loop.
  kount=0
  do i = istart, maxgen+istart-1
    write (6,  1111) i
    write (24, 1111) i
    write (24, 1050) (trim(parname(j)), j=1, nparam)
!stop 
    ! Evaluate the population, assign fitness, establish the best
    ! individual, and write output information.
    call evalout(iskip, iend, ibest, fbar, best)

    geni(i)   = float(i)
    genavg(i) = fbar
    genmax(i) = best

    if(npopsiz .eq. 1 .or. iskip .ne. 0) then
      close(24)
      stop
    endif

    ! Enter selection, crossover and mutation loop.
    ncross = 0
    ipick  = npopsiz
    do j = 1, npopsiz, nchild
      ! Perform selection.
      call selectn(ipick, j, mate1, mate2)

      ! Now perform crossover between the randomly selected pair.
      call crosovr(ncross, j, mate1, mate2)
    enddo

    write (6,  1225) ncross
    write (24, 1225) ncross

    ! Write child array back into parent array for new generation.
    ! Check to see if the best parent was replicated.
    call newgen(ielite, npossum, ig2sum, ibest)

    ! Implement micro-GA if enabled.
    call gamicro(i, npossum, ig2sum, ibest)

    ! Write to restart file.
    call restart(i, istart, kount)
  enddo

  write(24, 3000)

  do i = istart, maxgen+istart-1
    evals = float(npopsiz)*geni(i)
    write(24, 3100) geni(i), evals, genavg(i), genmax(i)
  enddo

  close (24)


1050 format(1x, ' #      Binary Code', 16x, <nparam>(A, 3x), ' Fitness')
1111 format(//'#################  Generation', i5, '  #################')
1225 format(/'  Number of Crossovers      =', i5)
3000 format(2x//'Summary of Output'/&
            2x, 'Generation   Evaluations   Avg.Fitness   Best Fitness')
3100 format(2x, 3(e11.4, 4x), e12.5)

  stop

! ------------------------------------------------------------------------------
! ##############################################################################
CONTAINS
include 'prog.mod-preproc.f90'
include 'prog.mod-random-encode.f90'
include 'prog.mod-evaluate.f90'
include 'prog.mod-microga.f90'
include 'prog.mod-regcm.f90'
END
!!! EOF
