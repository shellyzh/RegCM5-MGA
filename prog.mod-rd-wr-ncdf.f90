
!! /////////////////////////////////////////////////////////////////////////////
!! Read netcdf 1-D variable
Subroutine rd_ncdf_1d(fileNM, varNM, tdat, dimsSZ)
  implicit none
  character(*),          intent(in)    :: fileNM, varNM
  real(r8), allocatable, intent(inout) :: tdat(:)
  integer,               intent(inout) :: dimsSZ(1)
  character(15)                        :: dimsNM(1)
  integer                              :: dimsID(1), ncID, varID

  print*, "> Reading file ", trim(fileNM)
  print*, "  ... Reading var ", trim(varNM)

  call cerr( nf90_open(fileNM, nf90_NOwrite, ncID) )
  call cerr( nf90_inq_varid(ncID, varNM, varID) )
  call cerr( nf90_inquire_variable(ncID, varID, dimids = dimsID(1:1)) )
  call cerr( nf90_inquire_dimension(ncID, dimsID(1), dimsNM(1), dimsSZ(1)) )
  print*, dimsNM(1), dimsSZ(1)

  allocate(tdat(dimsSZ(1)))

  call cerr( nf90_get_var(ncID, varID, tdat))

  call cerr( nf90_close(ncID) )

End Subroutine rd_ncdf_1d


!! /////////////////////////////////////////////////////////////////////////////
!! Read netcdf 2-D variable
Subroutine rd_ncdf_2d(fileNM, varNM, tdat, dimsSZ)
  implicit none
  character(*),          intent(in)    :: fileNM, varNM
  real(r8), allocatable, intent(inout) :: tdat(:, :)
  integer,               intent(inout) :: dimsSZ(2)
  character(15)                        :: dimsNM(2)
  integer                              :: dimsID(2), ncID, varID

  print*, "> Reading file ", trim(fileNM)
  print*, "  ... Reading var ", trim(varNM)

  call cerr( nf90_open(fileNM, nf90_NOwrite, ncID) )
  call cerr( nf90_inq_varid(ncID, varNM, varID) )
  call cerr( nf90_inquire_variable(ncID, varID, dimids = dimsID(1:2)) )
  call cerr( nf90_inquire_dimension(ncID, dimsID(1), dimsNM(1), dimsSZ(1)) )
  call cerr( nf90_inquire_dimension(ncID, dimsID(2), dimsNM(2), dimsSZ(2)) )
  print*, dimsNM(1), dimsSZ(1)
  print*, dimsNM(2), dimsSZ(2)

  allocate(tdat(dimsSZ(1), dimsSZ(2)))

  call cerr( nf90_get_var(ncID, varID, tdat))

  call cerr( nf90_close(ncID) )

End Subroutine rd_ncdf_2d


!! /////////////////////////////////////////////////////////////////////////////
!! Read netcdf 3-D variable
Subroutine rd_ncdf_3d(fileNM, varNM, tdat, dimsSZ)
  implicit none
  character(*),          intent(in)    :: fileNM, varNM
  real(r8), allocatable, intent(inout) :: tdat(:, :, :)
  integer,               intent(inout) :: dimsSZ(3)
  character(15)                        :: dimsNM(3)
  integer                              :: dimsID(3), ncID, varID

  print*, "> Reading file ", trim(fileNM)
  print*, "  ... Reading var ", trim(varNM)

  call cerr( nf90_open(fileNM, nf90_NOwrite, ncID) )
  call cerr( nf90_inq_varid(ncID, varNM, varID) )
  call cerr( nf90_inquire_variable(ncID, varID, dimids = dimsID(1:3)) )
  call cerr( nf90_inquire_dimension(ncID, dimsID(1), dimsNM(1), dimsSZ(1)) )
  call cerr( nf90_inquire_dimension(ncID, dimsID(2), dimsNM(2), dimsSZ(2)) )
  call cerr( nf90_inquire_dimension(ncID, dimsID(3), dimsNM(3), dimsSZ(3)) )
  print*, dimsNM(1), dimsSZ(1)
  print*, dimsNM(2), dimsSZ(2)
  print*, dimsNM(3), dimsSZ(3)

  allocate(tdat(dimsSZ(1), dimsSZ(2), dimsSZ(3)))

  call cerr( nf90_get_var(ncID, varID, tdat))

  call cerr( nf90_close(ncID) )

End Subroutine rd_ncdf_3d


!! /////////////////////////////////////////////////////////////////////////////
!! Read netcdf 4-D variable
Subroutine rd_ncdf_4d(fileNM, varNM, tdat, dimsSZ)
  implicit none
  character(*),          intent(in)    :: fileNM, varNM
  real(r8), allocatable, intent(inout) :: tdat(:, :, :, :)
  integer,               intent(inout) :: dimsSZ(4)
  character(15)                        :: dimsNM(4)
  integer                              :: dimsID(4), ncID, varID

  print*, "> Reading file ", trim(fileNM)
  print*, "  ... Reading var ", trim(varNM)

  call cerr( nf90_open(fileNM, nf90_NOwrite, ncID) )
  call cerr( nf90_inq_varid(ncID, varNM, varID) )
  call cerr( nf90_inquire_variable(ncID, varID, dimids = dimsID(1:4)) )
  call cerr( nf90_inquire_dimension(ncID, dimsID(1), dimsNM(1), dimsSZ(1)) )
  call cerr( nf90_inquire_dimension(ncID, dimsID(2), dimsNM(2), dimsSZ(2)) )
  call cerr( nf90_inquire_dimension(ncID, dimsID(3), dimsNM(3), dimsSZ(3)) )
  call cerr( nf90_inquire_dimension(ncID, dimsID(4), dimsNM(4), dimsSZ(4)) )
  print*, dimsNM(1), dimsSZ(1)
  print*, dimsNM(2), dimsSZ(2)
  print*, dimsNM(3), dimsSZ(3)
  print*, dimsNM(4), dimsSZ(4)

  allocate(tdat(dimsSZ(1), dimsSZ(2), dimsSZ(3), dimsSZ(4)))

  call cerr( nf90_get_var(ncID, varID, tdat))

  call cerr( nf90_close(ncID) )

End Subroutine rd_ncdf_4d


!! /////////////////////////////////////////////////////////////////////////////
!! Write out 1-D variable
Subroutine wr_ncdf_1d(fileNM, var, varNM, dimsNM, dimsSZ, units, miss, append)
  implicit none
  character(*), intent(in) :: fileNM, varNM, dimsNM(1), units
  integer,      intent(in) :: dimsSZ(1), append
  real(r8),     intent(in) :: var(:)
  real(r8),     intent(in) :: miss
  integer                  :: ncID, varID, dimsID(1)
  

  if (append .ne. 1) then
    print*, "> Creating new file ", trim(fileNM)
    print*, "  ... Putting var ", trim(varNM)
    call cerr( nf90_create(fileNM, nf90_64bit_offset, ncID) )
    call cerr( nf90_def_dim(ncID, dimsNM(1), dimsSZ(1), dimsID(1)) )
  else
    print*, "> Append writing to file ", trim(fileNM)
    call cerr( nf90_open(fileNM, nf90_write, ncID) )
    call cerr( nf90_redef(ncID) )
 
    if ((varNM .eq. dimsNM(1)) .or. &
        (dimsNM(1) .ne. "lon" .and. dimsNM(1) .ne. "lat" .and. dimsNM(1) .ne. "time")) then
      call cerr( nf90_def_dim(ncID, dimsNM(1), dimsSZ(1), dimsID(1)) )
    else
      call cerr( nf90_inq_dimid(ncID, dimsNM(1), dimsID(1)) ) 
    endif
    
  endif

  call cerr( nf90_def_var(ncID, varNM, nf90_float, dimsID(1), varID) )
  
  if (varNM .eq. "lon" .or. varNM .eq. "longitude") then
    call cerr( nf90_put_att(ncID, varID, "long_name", "Longitude") )
    call cerr( nf90_put_att(ncID, varID, "units",     "degree_east") )
    call cerr( nf90_put_att(ncID, varID, "axis",      "X") )
  else if (varNM .eq. "lat" .or. varNM .eq. "latitude") then
    call cerr( nf90_put_att(ncID, varID, "long_name", "Latitude") )
    call cerr( nf90_put_att(ncID, varID, "units",     "degree_north") )
    call cerr( nf90_put_att(ncID, varID, "axis",      "Y") )
  else if (varNM .eq. "time") then
    call cerr( nf90_put_att(ncID, varID, "long_name", "Time") )
    call cerr( nf90_put_att(ncID, varID, "units",     units) )
    call cerr( nf90_put_att(ncID, varID, "axis",      "T") )
    call cerr( nf90_put_att(ncID, varID, "calendar",  "standard") )
  else if (varNM .eq. "jx") then
    call cerr( nf90_put_att(ncID, varID, "long_name", "x-coordinate in Cartesian system") )
    call cerr( nf90_put_att(ncID, varID, "units",     "m") )
    call cerr( nf90_put_att(ncID, varID, "axis",      "X") )
  else if (varNM .eq. "iy") then
    call cerr( nf90_put_att(ncID, varID, "long_name", "y-coordinate in Cartesian system") )
    call cerr( nf90_put_att(ncID, varID, "units",     "m") )
    call cerr( nf90_put_att(ncID, varID, "axis",      "Y") )
  endif

  call cerr( nf90_enddef(ncID) )

  call cerr( nf90_put_var(ncID, varID, var))
  call cerr( nf90_close(ncID) )


End Subroutine wr_ncdf_1d


!! ///////////////////////////////////////////////////////////////////////////////////////////////////
!! Write out 2-D variable
Subroutine wr_ncdf_2d(fileNM, var, varNM, dimsNM, dimsSZ, varLN, miss, append)
  implicit none
  character(*), intent(in) :: fileNM, varNM, dimsNM(2), varLN
  integer,      intent(in) :: dimsSZ(2), append
  real(r8),     intent(in) :: miss
  real(r8),     intent(in) :: var(:, :)
  integer                  :: ncID, dimsID(2), varID

  if (append .ne. 1) then
    print*, "> Creating file ", trim(fileNM)
    print*, "  ... Putting var ", trim(varNM)
    call cerr( nf90_create(fileNM, nf90_64bit_offset, ncID) )
    call cerr( nf90_def_dim(ncID, dimsNM(1), dimsSZ(1), dimsID(1)) )
    call cerr( nf90_def_dim(ncID, dimsNM(2), dimsSZ(2), dimsID(2)) )
  else
    print*, "> Append writing to file ", trim(fileNM)
    call cerr( nf90_open(fileNM, nf90_write, ncID) )
    call cerr( nf90_redef(ncID) )
 
    call cerr( nf90_inq_dimid(ncID, dimsNM(1), dimsID(1)) ) 
    call cerr( nf90_inq_dimid(ncID, dimsNM(2), dimsID(2)) ) 
  endif

  call cerr( nf90_def_var(ncID, varNM, nf90_float, dimsID(1:2), varID) )
  call cerr( nf90_put_att(ncID, varID, "long_name", varLN) )

  if (varNM .eq. "xlon") then
    call cerr( nf90_put_att(ncID, varID, "long_name", "Longitude on Cross Points") )
    call cerr( nf90_put_att(ncID, varID, "units",     "degrees_east") )
    call cerr( nf90_put_att(ncID, varID, "_CoordinateAxisType", "Lon") )
  else if (varnM .eq. "xlat") then
    call cerr( nf90_put_att(ncID, varID, "long_name", "Latitude on Cross Points") )
    call cerr( nf90_put_att(ncID, varID, "units",     "degrees_north") )
    call cerr( nf90_put_att(ncID, varID, "_CoordinateAxisType", "Lat") )
  endif

  !call cerr( nf90_put_att(ncID, varID, "_FillValue", miss) )
  call cerr( nf90_put_att(ncID, varID, "missing_value", miss) ) 
  call cerr( nf90_enddef(ncID) )

  call cerr( nf90_put_var(ncID, varID, var))
  call cerr( nf90_close(ncID) )
  

End Subroutine wr_ncdf_2d


!! ///////////////////////////////////////////////////////////////////////////////////////////////////
!! Write out 3-D variable
Subroutine wr_ncdf_3d(fileNM, var, varNM, dimsNM, dimsSZ, varLN, miss, append)
  implicit none
  character(*), intent(in) :: fileNM, varNM, dimsNM(3), varLN
  integer,      intent(in) :: dimsSZ(3), append
  real(r8),     intent(in) :: miss
  real(r8),     intent(in) :: var(:, :, :)
  integer                  :: ncID, dimsID(3), varID

  if (append .ne. 1) then
    print*, "> Creating file ", trim(fileNM)
    print*, "  ... Putting var ", trim(varNM)
    call cerr( nf90_create(fileNM, nf90_64bit_offset, ncID) )
    call cerr( nf90_def_dim(ncID, dimsNM(1), dimsSZ(1), dimsID(1)) )
    call cerr( nf90_def_dim(ncID, dimsNM(2), dimsSZ(2), dimsID(2)) )
    call cerr( nf90_def_dim(ncID, dimsNM(3), dimsSZ(3), dimsID(3)) )
  else
    print*, "> Append writing to file ", trim(fileNM)
    call cerr( nf90_open(fileNM, nf90_write, ncID) )
    call cerr( nf90_redef(ncID) )
 
    call cerr( nf90_inq_dimid(ncID, dimsNM(1), dimsID(1)) ) 
    call cerr( nf90_inq_dimid(ncID, dimsNM(2), dimsID(2)) ) 
    call cerr( nf90_inq_dimid(ncID, dimsNM(3), dimsID(3)) ) 
  endif

  call cerr( nf90_def_var(ncID, varNM, nf90_float, dimsID(1:3), varID) )
  call cerr( nf90_put_att(ncID, varID, "long_name", varLN) )

  !call cerr( nf90_put_att(ncID, varID, "_FillValue", miss) )
  call cerr( nf90_put_att(ncID, varID, "missing_value", miss) ) 
  call cerr( nf90_enddef(ncID) )

  call cerr( nf90_put_var(ncID, varID, var))
  call cerr( nf90_close(ncID) )
  

End Subroutine wr_ncdf_3d


!! ///////////////////////////////////////////////////////////////////////////////////////////////////
!! Write out 4-D variable
Subroutine wr_ncdf_4d(fileNM, var, varNM, dimsNM, dimsSZ, varLN, miss, append)
  implicit none
  character(*), intent(in) :: fileNM, varNM, dimsNM(4), varLN
  integer,      intent(in) :: dimsSZ(4), append
  real(r8),     intent(in) :: miss
  real(r8),     intent(in) :: var(:, :, :, :)
  integer                  :: ncID,  dimsID(4), varID

  if (append .ne. 1) then
    print*, "> Creating file ", trim(fileNM)
    print*, "  ... Putting var ", trim(varNM)
    call cerr( nf90_create(fileNM, nf90_64bit_offset, ncID) )
    call cerr( nf90_def_dim(ncID, dimsNM(1), dimsSZ(1), dimsID(1)) )
    call cerr( nf90_def_dim(ncID, dimsNM(2), dimsSZ(2), dimsID(2)) )
    call cerr( nf90_def_dim(ncID, dimsNM(3), dimsSZ(3), dimsID(3)) )
    call cerr( nf90_def_dim(ncID, dimsNM(4), dimsSZ(4), dimsID(4)) )
  else
    print*, "> Writing file ", trim(fileNM)
    call cerr( nf90_open(fileNM, nf90_write, ncID) )
    call cerr( nf90_redef(ncID) )
 
    call cerr( nf90_inq_dimid(ncID, dimsNM(1), dimsID(1)) ) 
    call cerr( nf90_inq_dimid(ncID, dimsNM(2), dimsID(2)) ) 
    call cerr( nf90_inq_dimid(ncID, dimsNM(3), dimsID(3)) ) 
    call cerr( nf90_inq_dimid(ncID, dimsNM(4), dimsID(4)) ) 
  endif

  call cerr( nf90_def_var(ncID, varNM, nf90_float, dimsID(1:4), varID) )
  call cerr( nf90_put_att(ncID, varID, "long_name", varLN) )

  !call cerr( nf90_put_att(ncID, varID, "_FillValue", miss) )
  call cerr( nf90_put_att(ncID, varID, "missing_value", miss) ) 
  call cerr( nf90_enddef(ncID) )

  call cerr( nf90_put_var(ncID, varID, var))
  call cerr( nf90_close(ncID) )
  

End Subroutine wr_ncdf_4d


!! ///////////////////////////////////////////////////////////////////////////////////////////////////
!! NCDF debugging
Subroutine cerr(istat)
  integer, intent(in) :: istat
  
  if (istat .ne. nf90_noerr) then
    print*, trim(nf90_strerror(istat))
    stop
  endif

End Subroutine cerr
