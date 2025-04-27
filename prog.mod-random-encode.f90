! ##############################################################################
! Returns a uniform random deviate between 0.0 and 1.0.  Set idum to
! any negative value to initialize or reinitialize the sequence.
! This function is taken from W.H. Press', "Numerical Recipes" p. 199.
subroutine ran3(idum, rand)
  implicit none
  !integer, intent(inout) :: idum
  integer :: idum
  real*8,  intent(inout) :: rand
  real*8, parameter :: mbig  = 4000000.
  real*8, parameter :: mseed = 1618033.
  real*8, parameter :: mz    = 0.
  real*8, parameter :: fac   = 1. / mbig
  ! According to Knuth, any large mbig, and any smaller (but still large)
  ! mseed can be substituted for the above values.
  integer :: iff, i, ii, k
  real*8  :: mj, mk
  real*8, dimension(55) :: ma

  data iff /0/
 
  if (idum .lt. 0 .or. iff .eq. 0) then
    iff = 1
    mj = mseed - dble(iabs(idum))
    mj = dmod(mj, mbig)
    ma(55) = mj
    mk = 1

    do i = 1, 54
      ii = mod(21*i, 55)
      ma(ii) = mk
      mk = mj - mk
      if (mk .lt. mz) then
        mk = mk + mbig
      endif
      mj = ma(ii)
    enddo

    do k = 1, 4
      do i = 1, 55
        ma(i) = ma(i) - ma(1 + mod(i+30, 55))
        if (ma(i) .lt. mz) then
          ma(i) = ma(i) + mbig
        endif
      enddo
    enddo

    inext  = 0
    inextp = 31
    idum   = 1
  endif

  inext = inext + 1
  if (inext .eq. 56) then
    inext = 1
  endif
  
  inextp = inextp + 1
  if (inextp .eq. 56) then
    inextp = 1
  endif

  mj = ma(inext) - ma(inextp)
  if (mj .lt. mz) then
    mj = mj + mbig
  endif

  ma(inext) = mj
  
  rand = mj*fac

  return
end subroutine ran3


! ##############################################################################
! This subroutine determines whether or not all parameters are within
! the specified range of possibility.  If not, the parameter is
! randomly reassigned within the range.  This subroutine is only
! necessary when the number of possibilities per parameter is not
! optimized to be 2**n, i.e. if npossum < ig2sum.
subroutine possibl(array, iarray)
  implicit none
  real*8,  dimension(nparmax, indmax), intent(inout) :: array
  integer, dimension(nchrmax, indmax), intent(inout) :: iarray
  integer :: i, irand, j, n2ig2j

  do i = 1, npopsiz
    call decode(i, array, iarray)

    do j = 1, nparam
      n2ig2j=2**ig2(j)

      if (nposibl(j) .ne. n2ig2j .and. array(j,i) .gt. parmax(j)) then
        call ran3(1, rand)

        irand      = dint(dble(nposibl(j))*rand)
        array(j,i) = g0(j) + dble(irand)*g1(j)

        call code(i, j, array, iarray)

        if (nowrite .eq. 0) then
          write(6, 1000) i, j
        endif

        if (nowrite .eq. 0) then
          write(24, 1000) i, j
        endif

      endif
    enddo

  enddo

 1000 format('*** Parameter adjustment to individual     ', i4, &
             ', parameter  ', i3, ' ***')

  return
end subroutine possibl


! ##############################################################################
! This routine codes a parameter into a binary string.
subroutine code(j, k, array, iarray)
  implicit none
  integer, intent(in) :: j, k
  real*8,  dimension(nparmax, indmax), intent(inout) :: array
  integer, dimension(nchrmax, indmax), intent(inout) :: iarray
  integer :: i, iparam, istart, m

  ! First, establish the beginning location of the parameter string of interest.
  istart=1
  do i = 1, k-1
    istart = istart + ig2(i)
  enddo

  ! Find the equivalent coded parameter value, and back out the binary
  ! string by factors of two.
  m = ig2(k) - 1
  if (g1(k) .eq. 0.0d0) then
    return
  endif

  iparam = nint((array(k,j) - g0(k)) / g1(k))

  do i = istart, istart+ig2(k)-1
    iarray(i, j) = 0

    if ((iparam+1) .gt. (2**m)) then
      iarray(i,j) = 1
      iparam      = iparam - 2**m
    endif

    m = m - 1
  enddo
  
  return
end subroutine code


! ##############################################################################
! This routine decodes a binary string to a real number.
subroutine decode(i, array, iarray)
  implicit none
  integer, intent(in) :: i
  real*8,  dimension(nparmax, indmax), intent(inout) :: array
  integer, dimension(nchrmax, indmax), intent(inout) :: iarray
  integer :: iparam, j, k, l, m

  l = 1
  do k = 1, nparam
    iparam = 0
    m = l

    do j = m, m+ig2(k)-1
      l = l + 1
      iparam = iparam + iarray(j,i)*(2**(m+ig2(k)-1-j))
    enddo

    array(k,i) = g0(k) + g1(k)*dble(iparam)
  enddo

  return
end subroutine decode


! ##############################################################################
!!! EOF
