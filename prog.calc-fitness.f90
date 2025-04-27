program calc_fitness
  use netcdf
  implicit none
  integer, parameter  :: r4 = kind(1.0)
  integer, parameter  :: r8 = selected_real_kind(2*precision(1.0_r4))
  real(r8), parameter :: miss = -99.0_r8
  character(len=200)  :: fmdl, fobs, fout
  character(len=30)   :: dimsNM(3)
  integer :: dimsSZ(3), NLON, NLAT, NT, ilon, ilat, it
  real(r8), allocatable, dimension(:)       :: lon, lat, time, &
                                               fldavemdl, fldaveobs
  real(r8), allocatable, dimension(:, :)    :: timavemdl, timaveobs
  real(r8), allocatable, dimension(:, :, :) :: datmdl, datobs
  real(r8) :: fitness, fldrmse, timrmse, &
              ofldmean, otimmean, ofldstd, otimstd,&
              rtmp, ritmp

  !! ///////////////////////////////////////////////////////////////////////////
  !! Reading input arguments
  call getarg(1, fobs)
  call getarg(2, fmdl)
  call getarg(3, fout)

  
  !! ///////////////////////////////////////////////////////////////////////////
  !! Reading input data
  dimsNM(1) = "lon" ; dimsNM(2) = "lat" ; dimsNM(3) = "time"
  call rd_ncdf_1d ( fobs, dimsNM(1), lon,  dimsSZ(1:1) )
  call rd_ncdf_1d ( fobs, dimsNM(2), lat,  dimsSZ(2:2) )
  call rd_ncdf_1d ( fobs, dimsNM(3), time, dimsSZ(3:3) )

  call rd_ncdf_3d ( fmdl, "rain", datmdl, dimsSZ(1:3) )
  call rd_ncdf_3d ( fobs, "pr",   datobs, dimsSZ(1:3) )

  NLON = dimsSZ(1) ; NLAT = dimsSZ(2) ; NT = dimsSZ(3)

  allocate( fldavemdl(NT), timavemdl(NLON, NLAT),&
            fldaveobs(NT), timaveobs(NLON, NLAT) )
  fldavemdl = 0.0_r8
  timavemdl = 0.0_r8
  fldaveobs = 0.0_r8
  timaveobs = 0.0_r8


  !! ///////////////////////////////////////////////////////////////////////////
  !! Calculating fldave
  do it = 1, NT
    ritmp = 0.0_r8

    do ilon = 1, NLON
      do ilat = 1, NLAT

        if (datmdl(ilon, ilat, it) .ge. 0.0_r8 .and.&
            datobs(ilon, ilat, it) .ge. 0.0_r8) then

          fldavemdl(it) = fldavemdl(it) + datmdl(ilon, ilat, it)
          fldaveobs(it) = fldaveobs(it) + datobs(ilon, ilat, it)
          ritmp = ritmp + 1.0_r8
        endif

      enddo
    enddo

    if (ritmp .eq. 0.0_r8) then
      fldavemdl(it) = miss
      fldaveobs(it) = miss
    else
      fldavemdl(it) = fldavemdl(it) / ritmp
      fldaveobs(it) = fldaveobs(it) / ritmp
    endif
  enddo


  !! ///////////////////////////////////////////////////////////////////////////
  !! Calculating timave
  do ilon = 1, NLON
    do ilat = 1, NLAT

      ritmp = 0.0_r8
      do it = 1, NT
        if (datmdl(ilon, ilat, it) .ge. 0.0_r8 .and.&
            datobs(ilon, ilat, it) .ge. 0.0_r8) then
          timavemdl(ilon, ilat) = timavemdl(ilon, ilat) + datmdl(ilon, ilat, it)
          timaveobs(ilon, ilat) = timaveobs(ilon, ilat) + datobs(ilon, ilat, it)
          ritmp = ritmp + 1.0_r8
        endif
      enddo

      if (ritmp .eq. 0.0_r8) then
        timavemdl(ilon, ilat) = miss
        timaveobs(ilon, ilat) = miss
      else
        timavemdl(ilon, ilat) = timavemdl(ilon, ilat) / ritmp
        timaveobs(ilon, ilat) = timaveobs(ilon, ilat) / ritmp
      endif
    enddo
  enddo

  
  !! ///////////////////////////////////////////////////////////////////////////
  !! calculate fldRMSE
  fldrmse = 0.0_r8
  ofldmean= 0.0_r8
  ritmp   = 0.0_r8
  do it = 1, NT
    if (fldavemdl(it) .ne. miss .and.&
        fldaveobs(it) .ne. miss) then
      fldrmse = fldrmse + (fldavemdl(it) - fldaveobs(it))**2.0_r8
      ofldmean= ofldmean + fldaveobs(it)

      ritmp = ritmp + 1.0_r8
    endif
  enddo

  fldrmse = sqrt(fldrmse / ritmp)
  ofldmean= ofldmean / ritmp


  !! ///////////////////////////////////////////////////////////////////////////
  !! calculate timRMSE
  timrmse = 0.0_r8
  otimmean= 0.0_r8
  ritmp   = 0.0_r8
  do ilon = 1, NLON
    do ilat = 1, NLAT
      if (timavemdl(ilon, ilat) .ne. miss .and.&
          timaveobs(ilon, ilat) .ne. miss) then
        timrmse = timrmse + (timavemdl(ilon, ilat) - timaveobs(ilon, ilat))**2.0_r8
        otimmean= otimmean + timaveobs(ilon, ilat)
        ritmp = ritmp + 1.0_r8
      endif
    enddo
  enddo


  timrmse = sqrt(timrmse / ritmp)
  otimmean= otimmean / ritmp


  !! ///////////////////////////////////////////////////////////////////////////
  !! Calculate observation fldstd
  ofldstd = 0.0_r8
  ritmp   = 0.0_r8
  do it = 1, NT
    if (fldavemdl(it) .ne. miss .and.&
        fldaveobs(it) .ne. miss) then
      ofldstd = ofldstd + (fldaveobs(it) - ofldmean)**2.0_r8
      ritmp   = ritmp + 1.0_r8
    endif
  enddo

  ofldstd = sqrt(ofldstd / ritmp)

  !! Calculate observation timstd
  otimstd = 0.0_r8
  ritmp   = 0.0_r8
  do ilon = 1, NLON
    do ilat = 1, NLAT
      if (timavemdl(ilon, ilat) .ne. miss .and.&
          timaveobs(ilon, ilat) .ne. miss) then
        otimstd = otimstd + (timaveobs(ilon, ilat) - otimmean)**2.0_r8
        ritmp   = ritmp + 1.0_r8
      endif
    enddo
  enddo

  otimstd = sqrt(otimstd / ritmp)


  !! ///////////////////////////////////////////////////////////////////////////
  !! Calculate fitness
  fitness = fldrmse/ofldstd + timrmse/otimstd
  open (unit=99, file=trim(fout), status="unknown")
    write(99, *) "New fitness is"
    write(99, *) fitness
  close(99)


contains
include "prog.mod-rd-wr-ncdf.f90"
end
