! ##############################################################################
! This subroutine inputs information from the ga.inp (gafort.in) file.
subroutine input
  implicit none
  integer :: i  

  kountmx = 5
  irestrt = 0
  iunifrm = 0
  iskip   = 0
  iend    = 0

  open (unit=24, file=ga_out, status='unknown')
    rewind 24

  open (unit=23, file=ga_nml, status='old')
    read (23, nml = gaparam)
  close (23)
  
  if (npopsiz .gt. indmax) then
    write(6, 1600) npopsiz
    write(24,1600) npopsiz
    close(24)
    stop
  endif

  if (nparam .gt. nparmax) then
    write(6, 1700) nparam
    write(24,1700) nparam
    close(24)
    stop
  endif

  ! If using the microga option, reset some input variables
  itourny = 1
  ielite  = 1
  nchild  = 1
  if (iunifrm .eq. 0) then
     pcross = 1.0d0
  else
     pcross = 0.5d0
  endif

 1600 format(1x, 'ERROR: npopsiz > indmax.  Set indmax = ', i6)
 1700 format(1x, 'ERROR: nparam > nparmax.  Set nparmax = ', i6)

  return
end subroutine input


! ##############################################################################
! This subroutine sets up the program by generating the g0, g1 and
! ig2 arrays, and counting the number of chromosomes required for the
! specified input.  The subroutine also initializes the random number
! generator, parent and iparent arrays (reads the ga.restart file).
subroutine initial(istart, npossum, ig2sum)
  implicit none
  integer, intent(inout) :: istart, npossum, ig2sum
  integer :: i, ids, j, k, l, n2j

  do i = 1, nparam
    g0(i)     = parmin(i)
    pardel(i) = parmax(i) - parmin(i)
    g1(i)     = pardel(i) / dble(nposibl(i) - 1)

  enddo

  do i = 1, nparam
    do j = 1, 30
      n2j = 2**j
  
      if (n2j .ge. nposibl(i)) then
        ig2(i) = j
        exit
      endif

      if (j .ge. 30) then
        write(6,  2000)
        write(24, 2000)
        close(24)
        stop
      endif
    enddo
  enddo

  ! Count the total number of chromosomes (bits) required
  nchrome = 0
  npossum = 0
  ig2sum  = 0

  do i = 1, nparam
    nchrome = nchrome + ig2(i)
    npossum = npossum + nposibl(i)
    ig2sum  = ig2sum  + (2**ig2(i))
  enddo

  ! write(*,*) nchrome, npossum, ig2sum

  if (nchrome .gt. nchrmax) then
    write(6,  1800) nchrome
    write(24, 1800) nchrome
    close(24)
    stop
  endif

  if (npossum .lt. ig2sum) then
    write(6,  2100)
    write(24, 2100)
  endif

  ! Initialize random number generator
  call ran3(idum, rand)

  if (irestrt .eq. 0) then
    ! Initialize the random distribution of parameters in the individual
    ! parents when irestrt=0.
    istart = 1
    do i = 1, npopsiz
      do j = 1, nchrome
        call ran3(1, rand)

        !TNX: write(*,*)  rand

        iparent(j, i) = 1
        if (rand .lt. 0.5d0) then
          iparent(j, i) = 0
        endif
      enddo
    enddo

    !TNX write(*,*) iparent

    if (npossum .lt. ig2sum) then
      call possibl(parent, iparent)
    endif
  else
    ! If irestrt.ne.0, read from restart file.
    open (unit=25, file=ga_res, status='OLD')
      rewind 25
      read(25, *) istart, npopsiz
      do j = 1, npopsiz
        read(25, *) k, (iparent(l, j), l=1, nchrome)
      enddo
    close (25)
  endif

  if (irestrt .ne. 0) then
    ! TNX: added to fixed problem of INOUT. Same problem as new-added i1
    ids = idum - istart
    call ran3(ids, rand)
  endif

 1800 format(1x,'ERROR: nchrome > nchrmax.  Set nchrmax = ', i6)
 2000 format(1x,'ERROR: You have a parameter with a number of '/&
             1x,'   possibilities > 2**30!  If you really desire this,'/&
             1x,'   change the DO loop 7 statement and recompile.'//&
             1x,'   You may also need to alter the code to work with'/&
             1x,'   REAL numbers rather than INTEGER numbers; Fortran'/&
             1x,'   does not like to compute 2**j when j>30.')
 2100 format(1x,'WARNING: for some cases, a considerable performance'/&
             1x,'   reduction has been observed when running a non-'/&
             1x,'   optimal number of bits with the micro-GA.'/&
             1x,'   If possible, use values for nposibl of 2**n,'/&
             1x,'   e.g. 2, 4, 8, 16, 32, 64, etc.  See ReadMe file.')

  return
end subroutine initial


! ##############################################################################
!!! EOF
