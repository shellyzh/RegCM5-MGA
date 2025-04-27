! ##############################################################################
subroutine regcm_initial
  implicit none

  open (unit=23, file=ga_nml, status="old")
    read(23, nml = regcmparam)
    read(23, nml = tiedtkeparam)
  close (23)
  
  print*, parname
!stop

  return
end subroutine regcm_initial

! ##############################################################################
subroutine regcm_GAparEncode(j)
  implicit none
  integer, intent(in)             :: j
  integer                         :: k, opt

  do k = 1, nparam
    opt = int(parent(k, j))

    select case (parname(k))
      case ("iboudy")                       ! Lateral Boundary conditions scheme
        if (opt .eq. 1) iboudy(:) = 0       !     0: Fixed
        if (opt .eq. 2) iboudy(:) = 1       !     1: Relaxation, linear technique
        if (opt .eq. 3) iboudy(:) = 2       !     2: Time-dependent
        if (opt .eq. 4) iboudy(:) = 3       !     3: Time & inflow/outflow dependent
        if (opt .eq. 5) iboudy(:) = 4       !     4: Sponge
        if (opt .eq. 6) iboudy(:) = 5       !     5: Relaxation, exponential technique
      case ("ibltyp")                       ! Boundary layer scheme    
        if (opt .eq. 1) ibltyp(:) = 1       !     1: Holtslag
        if (opt .eq. 2) ibltyp(:) = 2       !     2: UW PBL
        !if (opt .eq. 3) ibltyp(:) = 3       !     3: GFS 2011
        !if (opt .eq. 4) ibltyp(:) = 4       !     4: MYJ
        !if (opt .eq. 5) ibltyp(:) = 0      !     0: Frictionless / currently of to reach 2^2=4 option
      case ("idiffu")                       ! Diffusion scheme
        if (opt .eq. 1) idiffu(:) = 0       !     0: No diffusion
        if (opt .eq. 2) idiffu(:) = 1       !     1: MM5 4th order interior / 2nd order boundary
        if (opt .eq. 3) idiffu(:) = 2       !     2: LeVeque 4th order 9 point laplacian
        if (opt .eq. 4) idiffu(:) = 3       !     3: Xue 6th order with flux limiter
      case ("icup_lnd")                     ! Cumulus convection scheme Over Land
        !if (opt .eq. 1) icup_lnd(:) = -1   !    -1: MM5 Shallow cumulus scheme: No precipitation but only mixing.
        !if (opt .eq. 2) icup_lnd(:) = 0    !     0: OFF
        !if (opt .eq. 3) icup_lnd(:) = 1    !     1: Kuo
        !if (opt .eq. 4) icup_lnd(:) = 2    !     2: Grell
        !if (opt .eq. 5) icup_lnd(:) = 3    !     3: Betts-Miller (1986) DOES NOT WORK !!!
        !if (opt .eq. 6) icup_lnd(:) = 4    !     4: Emanuel (1991)
        !if (opt .eq. 7) icup_lnd(:) = 5    !     5: Tiedtke (1996)
        !if (opt .eq. 8) icup_lnd(:) = 6    !     6: Kain-Fritsch (1990), Kain (2004)
        if (opt .eq. 1) icup_lnd(:) = 2     !     2: Grell
        if (opt .eq. 2) icup_lnd(:) = 4     !     4: Emanuel (1991)
        if (opt .eq. 3) icup_lnd(:) = 5     !     5: Tiedtke (1996)
        if (opt .eq. 4) icup_lnd(:) = 6     !     6: Kain-Fritsch (1990), Kain (2004)
      case ("icup_ocn")                     ! Cumulus convection scheme Over Icean
        !if (opt .eq. 1) icup_ocn(:) = -1
        !if (opt .eq. 2) icup_ocn(:) = 0
        !if (opt .eq. 3) icup_ocn(:) = 1
        !if (opt .eq. 4) icup_ocn(:) = 2
        !if (opt .eq. 5) icup_ocn(:) = 3
        !if (opt .eq. 6) icup_ocn(:) = 4
        !if (opt .eq. 7) icup_ocn(:) = 5
        !if (opt .eq. 8) icup_ocn(:) = 6
        if (opt .eq. 1) icup_ocn(:) = 2     !     2: Grell
        if (opt .eq. 2) icup_ocn(:) = 4     !     4: Emanuel (1991)
        if (opt .eq. 3) icup_ocn(:) = 5     !     5: Tiedtke (1996)
        if (opt .eq. 4) icup_ocn(:) = 6     !     6: Kain-Fritsch (1990), Kain (2004)
      case ("ipptls")                       ! Moisture scheme, i.e., microphysics
        if (opt .eq. 1) ipptls(:) = 1       !     1: SUBEX
        if (opt .eq. 2) ipptls(:) = 2       !     2: ICTP, i.e., Nogherotto-Tompkins
        if (opt .eq. 3) ipptls(:) = 3       !     3: WSM5, i.e., WRF 5-class (TNX: only recommend for resolution < 4km
        !if (opt .eq. 4) ipptls(:) = 2
      case ("iocnflx")                      ! Ocean Flux scheme
        if (opt .eq. 1) iocnflx(:) = 1      !     1: BATS1e Monin-Obukhov  (TNX: weaker than Zeng in manyyyyy studiessss)
        if (opt .eq. 2) iocnflx(:) = 2      !     2: Zeng et al (1998)
        if (opt .eq. 3) iocnflx(:) = 3      !     3: Coare bulk flux algorithm (TNX: may be not good too)
      case ("iocnrough")                    ! Zeng Ocean model roughness formula to use.
        if (opt .eq. 1) iocnrough(:) = 1    !     1: (0.0065*ustar*ustar)/egrav
        if (opt .eq. 2) iocnrough(:) = 2    !     2: (0.013*ustar*ustar)/egrav + 0.11*visa/ustar
        if (opt .eq. 3) iocnrough(:) = 3    !     3: (0.017*ustar*ustar)/egrav
        if (opt .eq. 4) iocnrough(:) = 4    !     4: Huang 2012 free convection and swell effects
        if (opt .eq. 5) iocnrough(:) = 5    !     5: four regime formulation
      case ("iocnzoq")                      ! Zeng Ocean model factors for t,q roughness
        if (opt .eq. 1) iocnzoq(:) = 1      !     1: 2.67*(re**d_rfour) - 2.57
        if (opt .eq. 2) iocnzoq(:) = 2      !     2: min(4.0e-4, 2.0e-4*re**(-3.3))
        if (opt .eq. 3) iocnzoq(:) = 3      !     3: COARE formulation as in bulk flux above
      case ("icldfrac")                     ! Cloud fraction algorithm
        if (opt .eq. 1) icldfrac(:) = 0     !     0: Original SUBEX
        if (opt .eq. 2) icldfrac(:) = 1     !     1: Xu-Randall empirical
        if (opt .eq. 3) icldfrac(:) = 2     !     2: Thompson scheme
        if (opt .eq. 4) icldfrac(:) = 3     !     3: Gultepe-Isaac scheme
        if (opt .eq. 5) icldfrac(:) = 4     !     4: Texeira scheme
        if (opt .eq. 6) icldfrac(:) = 5     !     5: Tompkins linearized scheme
        if (opt .eq. 7) icldfrac(:) = 6     !     6: Echam5 like scheme
      case ("irrtm")                        ! Use RRTM radiation scheme instead of CCSM
        if (opt .eq. 1) irrtm(:) = 0        !     0: No
        if (opt .eq. 2) irrtm(:) = 1        !     1: Yes
      case ("iconv")
        iconv = int(parent(k, j))
      case ("entrmax")
        entrmax = parent(k, j)
      case ("entrdd")
        entrdd = parent(k, j)
      case ("entrpen_lnd")
        entrpen_lnd = parent(k, j)
      case ("entrpen_ocn")
        entrpen_ocn = parent(k, j)
      case ("entrscv")
        entrscv = parent(k, j)
      case ("entrmid")
        entrmid = parent(k, j)
      case ("cprcon")
        cprcon = parent(k, j)
      case ("detrpen_lnd")
        detrpen_lnd = parent(k, j)
      case ("detrpen_ocn")
        detrpen_ocn = parent(k, j)
      case ("entshalp")
        entshalp = parent(k, j)
      case ("rcuc_lnd")
        rcuc_lnd = parent(k, j)
      case ("rcuc_ocn")
        rcuc_ocn = parent(k, j)
      case ("rcpec_lnd")
        rcpec_lnd = parent(k, j)
      case ("rcpec_ocn")
        rcpec_ocn = parent(k, j)
      case ("rhebc_lnd")
        rhebc_lnd = parent(k, j)
      case ("rhebc_ocn")
        rhebc_ocn = parent(k, j)
      case ("rprc_lnd")
        rprc_lnd = parent(k, j)
      case ("rprc_ocn")
        rprc_ocn = parent(k, j)
      case ("cmtcape")
        cmtcape = parent(k, j)
      case ("lmfpen")
        if (opt .eq. 1) lmfpen    = .true.
        if (opt .eq. 2) lmfpen    = .false.
      case ("lmfmid")
        if (opt .eq. 1) lmfmid    = .true.
        if (opt .eq. 2) lmfmid    = .false.
      case ("lmfdd")
        if (opt .eq. 1) lmfdd     = .true.
        if (opt .eq. 2) lmfdd     = .false.
      case ("lepcld")
        if (opt .eq. 1) lepcld    = .true.
        if (opt .eq. 2) lepcld    = .false.
      case ("lmfdudv")
        if (opt .eq. 1) lmfdudv   = .true.
        if (opt .eq. 2) lmfdudv   = .false.
      case ("lmfscv")
        if (opt .eq. 1) lmfscv    = .true.
        if (opt .eq. 2) lmfscv    = .false.
      case ("lmfuvdis")
        if (opt .eq. 1) lmfuvdis  = .true.
        if (opt .eq. 2) lmfuvdis  = .false.
      case ("lmftrac")
        if (opt .eq. 1) lmftrac   = .true.
        if (opt .eq. 2) lmftrac   = .false.
      case ("lmfsmooth")
        if (opt .eq. 1) lmfsmooth = .true.
        if (opt .eq. 2) lmfsmooth = .false.
      case ("lmfwstar")
        if (opt .eq. 1) lmfwstar  = .true.
        if (opt .eq. 2) lmfwstar  = .false.
    end select  
  enddo

  return
end subroutine regcm_GAparEncode


! ##############################################################################
! Write out a regcm_namelist
subroutine regcm_namelist(i, j, idom)
  implicit none
  integer, intent(in) :: i, j, idom
  character(len=100)  :: ci, cj, cidom, hostname, ncores
  character(len=200)  :: subdirout, command
  integer             :: nspgx, nspgd
  real*8              :: high_nudge, medium_nudge, low_nudge

  ! --------------------------------------------------------------------------
  ! Pre defined namelist here
  write(ci, '(I3.3)') i
  write(cj, '(I2.2)') j

  fnml(idom) = "regcm5-nml/test.namelist."//trim(domname(idom))//&
                "_gen"//trim(ci)//"_idv"//trim(cj)//".in"

  subdirout = trim(dirout)//"/gen"//trim(ci)//"_idv"//trim(cj)
  command = "[[ ! -f "//trim(subdirout)//" ]] && mkdir -p "//trim(subdirout)

  ! print*, command

  call system(command)

  ! --------------------------------------------------------------------------
  ! --------------------------------------------------------------------------

  ! Performing some adjust for each domain and GA

  ! --------------------------------------------------------------------------
  ! Adjust Bufferzone parameter
  ! TNX: Let's change these value as shown in ICTP namelist
  if (ds(idom) .gt. 10.) then
    nspgx   = 24
    nspgd   = 24
    high_nudge   = 6.0d0
    medium_nudge = 4.0d0
    low_nudge    = 4.0d0
  else
    nspgx   = 30
    nspgd   = 30
    high_nudge   = 9.0d0
    medium_nudge = 6.0d0
    low_nudge    = 3.0d0
  endif


  open (unit=13, file=fnml, status="unknown")
    ! Choice of dynamic core
    write(13, *) "&coreparam"
    write(13, *) " idynamic = ", idynamic(idom), ","
    write(13, *) "/"

    ! Domain dimension
    write(13, *) "&dimparam"
    write(13, *) " jx       = ", jx(idom), ","
    write(13, *) " iy       = ", iy(idom), ","
    write(13, *) " kz       = ", kz(idom), ","
    write(13, *) " dsmin    = 0.01,"
    write(13, *) " dsmax    = 0.05,"
    write(13, *) " nsg      = 1,"
    write(13, *) " njxcpus  = -1,"
    write(13, *) " niycpus  = -1,"
    write(13, *) "/"

    ! Doman geolocation
    write(13, *) "&geoparam"
    write(13, *) " iproj    = '", trim(iproj(idom)), "',"
    write(13, *) " ds       = ", ds(idom), ","
    write(13, *) " ptop     = 5.0,"
    write(13, *) " clon     = ", clon(idom), ","
    write(13, *) " clat     = ", clat(idom), ","
    write(13, *) " cntri    = -1,"
    write(13, *) " cntrj    = -1,"
    write(13, *) " truelatl = 30.0,"
    write(13, *) " truelath = 60.0,"
    write(13, *) "/"

    ! Domain terrain generation parameters
    write(13, *) "&terrainparam"
    write(13, *) " domname  = '", trim(domname(idom)), "',"
    write(13, *) " lresamp  = .false.,"
    write(13, *) " smthbdy  = .true.,"
    write(13, *) " ismthlev = 2,"
    write(13, *) " roidem   = 1.5,"
    write(13, *) " h2ohgt   = .false.," ! shelly changed: true to false, based on ICTP
    write(13, *) " h2opct   = 50.0,"
    write(13, *) " lakedpth    = .false.,"
    write(13, *) " lsmoist     = .false.,"
    write(13, *) " fudge_lnd   = .false.,"
    write(13, *) " fudge_lnd_s = .false.,"
    write(13, *) " fudge_tex   = .false.,"
    write(13, *) " fudge_tex_s = .false.,"
    write(13, *) " fudge_lak   = .false.,"
    write(13, *) " fudge_lak_s = .false., "
    write(13, *) " dirter   = '", trim(dirglob), "',"
    write(13, *) " inpter   = '", trim(inpglob), "',"
    write(13, *) " tersrc   = 'GMTED',"
    write(13, *) " smsrc    = 'ESACCI',"
    write(13, *) " moist_filename = 'moist.nc',"
    write(13, *) "/"

    ! ICBC Global data input control
    write(13, *) "&globdatparam"
    write(13, *) " ibdyfrq  = ", ibdyfrq(idom), ","
    write(13, *) " ssttyp   = '", trim(ssttyp(idom)), "',"
    write(13, *) " dattyp   = '", trim(dattyp(idom)), "',"
    write(13, *) " chemtyp  = 'MZCLM',"
    write(13, *) " gdate1   = ", gdate1, ","
    write(13, *) " gdate2   = ", gdate2, ","
    write(13, *) " calendar = '", trim(calendar), "',"
    write(13, *) " dirglob  = '", trim(dirglob), "',"
    write(13, *) " inpglob  = '", trim(inpglob), "',"
    write(13, *) " ensemble_run = .false.,"
    write(13, *) "/"

    ! Nesting control
    write(13, *) "&fnestparam"
    write(13, *) " coarse_outdir  = '", trim(coarse_outdir), "',"
    write(13, *) " coarse_domname = '", trim(coarse_domname(idom)), "',"
    write(13, *) "/"

    ! Buffer Zone Control relaxation + diffusion term
    write(13, *) "&boundaryparam"
    write(13, *) " nspgx  = ", nspgx, ","
    write(13, *) " nspgd  = ", nspgd, ","
    write(13, *) " high_nudge   = ", high_nudge, ","
    write(13, *) " medium_nudge = ", medium_nudge, ","
    write(13, *) " low_nudge    = ", low_nudge, ","
    write(13, *) "/"

    ! Model start/restart control
    write(13, *) "&restartparam"
    write(13, *) " ifrest = .false.,"
    write(13, *) " mdate0 = ", gdate1, ","
    write(13, *) " mdate1 = ", gdate1, ","
    write(13, *) " mdate2 = ", gdate2, ","
    write(13, *) "/"

    ! Model timing parameters
    write(13, *) "&timeparam"
    write(13, *) " dt = ", dt(idom), ","
    write(13, *) "/"

    ! Model Output control
    write(13, *) "&outparam"
    write(13, *) " prestr  = '',"
    write(13, *) " outnwf  = 0.,"
    write(13, *) " ifsave  =  .false.,"     ! If 1-month run -> off (file for restart)
    write(13, *) " savfrq  =      0.,"
    write(13, *) " ifatm   =  .false.,"     ! If 1-month run -> off (Atmosphere variables - super heavy)
    write(13, *) " atmfrq  =      6.,"
    write(13, *) " ifrad   =  .false.,"     ! If 1-month run -> off (Radiation variables - super heavy)
    write(13, *) " radfrq  =      6.,"
    write(13, *) " ifsrf   =  .true.,"      ! If 1-month run -> off (Surface variables - very heavy)
                                            ! unfortunately IF STS is on SRF have to be on too
    write(13, *) " srffrq  =      3.,"
    write(13, *) " ifsts   =  .true.,"
    write(13, *) " ifshf   =  .false.,"     ! Some variables at 1-hourly interval -> useless for us most of the time
    write(13, *) " ifsub   = .false.,"
    write(13, *) " subfrq  =      6.,"
    write(13, *) " iflak   = .false.,"
    write(13, *) " lakfrq  =      6.,"
    write(13, *) " ifchem  = .false.,"
    write(13, *) " ifopt   = .false.,"
    write(13, *) " chemfrq =      6.,"
    write(13, *) " enable_atm_vars = 73*.true.,"
    write(13, *) " enable_srf_vars = 46*.true.,"
    write(13, *) " enable_rad_vars = 28*.true.,"
    write(13, *) " enable_sub_vars = 18*.true.,"
    write(13, *) " enable_sts_vars = 18*.true.,"
    write(13, *) " enable_shf_vars = 8*.true., "
    write(13, *) " enable_lak_vars = 18*.true.,"
    write(13, *) " enable_opt_vars = 20*.true.,"
    write(13, *) " enable_che_vars = 26*.true.,"
    write(13, *) " dirout  = '", trim(subdirout), "',"
    write(13, *) " lsync   = .false.,"
    write(13, *) " uvrotate = .false.,"
    write(13, *) " icosp = 0,"
    write(13, *) " idiag = 0,"
    write(13, *) " do_parallel_netcdf_in  = .false.,"
    write(13, *) " do_parallel_netcdf_out = .false.,"
    write(13, *) " deflate_level = 1,"
    write(13, *) "/"

    ! Model Physics
    write(13, *) "&physicsparam"
    write(13, *) " iboudy     = ", iboudy(idom), ","
    write(13, *) " isladvec   = 0,"
    write(13, *) " iqmsl      = 1,"
    write(13, *) " ibltyp     = ", ibltyp(idom), ","
    write(13, *) " idiffu     = ", idiffu(idom), ","
    write(13, *) " icup_lnd   = ", icup_lnd(idom), ","
    write(13, *) " icup_ocn   = ", icup_ocn(idom), ","
    write(13, *) " ipptls     = ", ipptls(idom), ","
    write(13, *) " iocncpl    = 0,"
    write(13, *) " iwavcpl    = 0,"
    write(13, *) " icopcpl    = 0,"
    write(13, *) " iocnflx    = ", iocnflx(idom), ","
    write(13, *) " iocnrough  = ", iocnrough(idom), ","
    write(13, *) " iocnzoq    = ", iocnzoq(idom), ","
    write(13, *) " ipgf       = 0,"
    write(13, *) " iemiss     = 0,"
    write(13, *) " lakemod    = 0,"
    write(13, *) " ichem      = 0,"
    write(13, *) " scenario  = 'SSP585'," ! shelly changed based on ICTP
    write(13, *) "! ghg_year_const = 1950,"
    write(13, *) " idcsst    = 0," ! shelly changed based on ICTP
    write(13, *) "! ipcpcool  = 0,"
    write(13, *) "! iwhitecap = 0,"
    write(13, *) " iseaice   = 0," ! shelly changed based on ICTP
    write(13, *) "! idesseas  = 0,"
    write(13, *) " iconvlwp   = 0," ! shelly changed: from 1 to 0, based on ICTP
    write(13, *) " icldfrac   = ", icldfrac(idom), ","
    write(13, *) " icldmstrat = 1,"
    write(13, *) " icumcloud  = 1,"
    write(13, *) " irrtm      = ", irrtm(idom), ","
    write(13, *) " iclimao3   = 1," ! shelly changed: from 0 to 1, based on ICTP
    write(13, *) " iclimaaer  = 2," ! shelly changed: from 0 to 2, based on ICTP
    write(13, *) " isolconst  = 0,"
    write(13, *) " islab_ocean= 0,"
    write(13, *) "! itweak    = 0,"
    write(13, *) "! ipgwrun   = 0,"
    write(13, *) "! ifixsolar = 0,"
    write(13, *) "! fixedsolarval =  343.,"
    write(13, *) " radclimpath = 'globdata/MERRA2/OPPMONTH',"
    write(13, *) "/"
    
    write(13, *) "&cldparam"
    write(13, *) " ncld = 0,"
    write(13, *) " cftotmax = 1.0,"
    write(13, *) " clfrcvmax = 1.0,"

    ! Tiedtke parameters
    write(13, *) "&tiedtkeparam"
    write(13, *) " iconv       = ", iconv, ","
    write(13, 131) " entrmax     = ", entrmax, ","
    write(13, 131) " entrdd      = ", entrdd, ","
    write(13, 131) " entrpen_lnd = ", entrpen_lnd, ","
    write(13, 131) " entrpen_ocn = ", entrpen_ocn, ","
    write(13, 131) " entrscv     = ", entrscv, ","
    write(13, 131) " entrmid     = ", entrmid, ","
    write(13, 131) " cprcon      = ", cprcon, ","
    write(13, 131) " detrpen_lnd = ", detrpen_lnd, ","
    write(13, 131) " detrpen_ocn = ", detrpen_ocn, ","
    write(13, 131) " entshalp    = ", entshalp, ","
    write(13, 131) " rcuc_lnd    = ", rcuc_lnd, ","
    write(13, 131) " rcuc_ocn    = ", rcuc_ocn, ","
    write(13, 131) " rcpec_lnd   = ", rcpec_lnd, ","
    write(13, 131) " rcpec_ocn   = ", rcpec_ocn, ","
    write(13, 131) " rhebc_lnd   = ", rhebc_lnd, ","
    write(13, 131) " rhebc_ocn   = ", rhebc_ocn, ","
    write(13, 131) " rprc_lnd    = ", rprc_lnd, ","
    write(13, 131) " rprc_ocn    = ", rprc_ocn, ","
    write(13, 131) " revap_lnd   = ", revap_lnd, ","
    write(13, 131) " revap_ocn   = ", revap_ocn, ","
    write(13, *) " cmtcape     = ", cmtcape, ","
    write(13, *) " lmfpen      = ", lmfpen, ","
    write(13, *) " lmfmid      = ", lmfmid, ","
    write(13, *) " lmfdd       = ", lmfdd, ","
    write(13, *) " lepcld      = ", lepcld, ","
    write(13, *) " lmfdudv     = ", lmfdudv, ","
    write(13, *) " lmfscv      = ", lmfscv, ","
    write(13, *) " lmfuvdis    = ", lmfuvdis, ","
    write(13, *) " lmftrac     = ", lmftrac, ","
    write(13, *) " lmfsmooth   = ", lmfsmooth, ","
    write(13, *) " lmfwstar    = ", lmfwstar, ","
    write(13, *) "/"

131 format(1x, A, e10.3, A)

    ! CLM4.5 parameters
    write(13, *) "&clm_inparm"
    write(13, *) " fpftcon       = 'pft-physiology.c130503.nc',"
    write(13, *) " fsnowoptics   = 'snicar_optics_5bnd_c090915.nc',"
    write(13, *) " fsnowaging    = 'snicar_drdt_bst_fit_60_c070416.nc',"
    write(13, *) " urban_hac     = 'OFF', ! Valid values: OFF, ON, ON_WASTEHEAT"
    write(13, *) " urban_traffic = .false.,"
    write(13, *) "/"

    write(13, *) "&clm_soilhydrology_inparm"
    write(13, *) " h2osfcflag = 1,"
    write(13, *) " origflag   = 0,"
    write(13, *) "/"

    write(13, *) "&clm_hydrology1_inparm"
    write(13, *) " oldfflag = 0,"
    write(13, *) "/"

    write(13, *) "&clm_regcm"
    write(13, *) " enable_megan_emission = .false.,"
    write(13, *) " enable_urban_landunit = .true.,"
    write(13, *) " enable_more_crop_pft  = .false.,"
    write(13, *) " enable_dv_baresoil    = .false.,"
    write(13, *) " enable_cru_precip     = .false.,"
    write(13, *) "/"

  close (13)

!stop

  return
end subroutine regcm_namelist


! ##############################################################################
subroutine regcm_run(i, j)
  implicit none
  integer, intent(in) :: i, j
  character(len=100)  :: ci, cj, cidom, machine, ncores, ctmp, hostname
  character(len=300)  :: command
  integer             :: idom, ismodelrun, k
 
  write(ci, '(I3.3)') i
  write(cj, '(I2.2)') j
  
  command = "awk -F\: '{print $1}' mpihost."//trim(cj)//" > /dev/shm/__host."//trim(cj)
  call system(command)
  command = "awk -F\: '{print $2}' mpihost."//trim(cj)//" >> /dev/shm/__host."//trim(cj)
  call system(command)
  open(unit=88, file="/dev/shm/__host."//trim(cj), status="old")
    read(88, *) hostname
    read(88, *) ncores
  close(88)
 
  open(unit=99, file="param.sh", status="unknown")
    write(99, *) "GEN="//trim(ci)
    write(99, *) "IDV="//trim(cj)
    do idom = 1, ndom
      write(cidom, '(I1)') idom
      write(99, 999) "IDVcode=", (iparent(k, j), k=1, nchrome)
      write(99,   *) "DOM"//trim(cidom)//"="//trim(domname(idom))
      write(99,   *) "NML"//trim(cidom)//"="//trim(fnml(idom))
      write(99, *) "NCORES="//trim(ncores)
    enddo
  close(99)
 999 format(1x, A, <nchrome>i1)

  ! Call bash script for model submiting 
  command = "ssh -o StrictHostKeyChecking=no "//trim(hostname)//" 'cd {your_working_dir} ; ./{your_fitness_function}.sh'"



  !print*, command
  ! a key to check if previous submit job is waiting (in case of long queue)
  call system("[[ -f is.modelrun ]] && rm -f is.modelrun") 
  call system(command)
  do while (1)
    inquire(file="is.modelrun", exist=ismodelrun)
    if (ismodelrun) then
      exit
    else
      call sleep(5) ! sleeping 5 second to make sure the param.sh is read correctly
    endif
  enddo

  return
end subroutine regcm_run

!!! EOF
