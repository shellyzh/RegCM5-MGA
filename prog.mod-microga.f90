! ##############################################################################
! Subroutine for selection operator.  Presently, tournament selection
! is the only option available.
subroutine selectn(ipick, j, mate1, mate2)
  implicit none
  integer, intent(in)    :: j
  integer, intent(inout) :: ipick, mate1, mate2
  integer :: n

  ! If tournament selection is chosen (i.e. itourny=1), then
  ! implement "tournament" selection for selection of new population.
  if (itourny .eq. 1) then
!print*,"-----------------------------"
!print*,"TNX-1: ", ipick, mate1, mate2
    call select(mate1, ipick)
!print*,"TNX-2: ", ipick, mate1, mate2
    call select(mate2, ipick)
!print*,"TNX-3: ", ipick, mate1, mate2
!print*, "TNX: ", mate1,mate2,fitness(mate1),fitness(mate2)
    do n = 1, nchrome
      ichild(n, j) = iparent(n, mate1)
      if (nchild .eq. 2) then
        ichild(n, j+1) = iparent(n, mate2)
      endif
    enddo

  endif

  return
end subroutine selectn


! ##############################################################################
! This routine selects the better of two possible parents for mating.
subroutine select(mate, ipick)
  implicit none
  integer, intent(inout) :: ipick, mate
  integer :: ifirst, isecond

  if (ipick+1 .gt. npopsiz) then
    call shuffle(ipick)
  endif

  ifirst  = ipick
  isecond = ipick + 1
  ipick   = ipick + 2
  !TNX: change here for RMSE: if(fitness(ifirst) .gt. fitness(isecond)) then
  if(fitness(ifirst) .lt. fitness(isecond)) then
    mate = ifirst
  else
    mate = isecond
  endif

  ! write(3,*)'select',ifirst,isecond,fitness(ifirst),fitness(isecond)

  return
end subroutine select


! ##############################################################################
! This routine shuffles the parent array and its corresponding fitness
subroutine shuffle(ipick)
  implicit none
  integer, intent(inout) :: ipick
  integer :: iother, itemp, j, n
  real*8  :: temp

  ipick = 1
  do j = 1, npopsiz-1
    call ran3(1, rand)

    iother = j + 1 + dint(dble(npopsiz-j) * rand)

    do n = 1, nchrome
      itemp = iparent(n, iother)
      iparent(n, iother) = iparent(n, j)
      iparent(n, j)      = itemp
    enddo

    temp            = fitness(iother)
    fitness(iother) = fitness(j)
    fitness(j)      = temp
  enddo

  return
end subroutine shuffle


! ##############################################################################
! Subroutine for crossover between the randomly selected pair.
subroutine crosovr(ncross, j, mate1, mate2)
  implicit none
  integer, intent(inout) :: ncross
  integer, intent(in)    :: j, mate1, mate2
  integer :: icross, n

  if (iunifrm .eq. 0) then
    ! Single-point crossover at a random chromosome point.
    call ran3(1, rand)
!print*, "TNX rand = ", rand
    if (rand .gt. pcross) then
      goto 69
    endif

    ncross = ncross + 1
    call ran3(1, rand)
!print*, "TNX rand = ", rand

    icross = 2 + dint(dble(nchrome-1)*rand)
print*, "TNX icross = ", icross
    do n = icross, nchrome
      ichild(n, j) = iparent(n, mate2)
      if (nchild .eq. 2) then
        ichild(n, j+1) = iparent(n, mate1)
      endif
    enddo
  else
    ! Perform uniform crossover between the randomly selected pair.
    do n = 1, nchrome
      call ran3(1, rand)

      if (rand .le. pcross) then
        ncross = ncross + 1
        ichild(n, j) = iparent(n, mate2)
        if(nchild .eq. 2) then
          ichild(n, j+1) = iparent(n, mate1)
        endif
      endif
    enddo
  endif

69 continue

  return
end subroutine crosovr


! ##############################################################################
! Write child array back into parent array for new generation.  Check
! to see if the best parent was replicated; if not, and if ielite=1,
! then reproduce the best parent into a random slot.
subroutine newgen(ielite, npossum, ig2sum, ibest)
  implicit none
  integer, intent(in) :: ielite, npossum, ig2sum
  integer, dimension(nchrmax), intent(in) :: ibest
  integer :: irand, j, jelite, kelite, n
  
  if (npossum .lt. ig2sum) then
    call possibl(child, ichild)
  endif

  kelite = 0
  do j = 1, npopsiz
    jelite=0

    do n = 1, nchrome
      iparent(n, j) = ichild(n, j)

      if (iparent(n, j) .eq. ibest(n)) then
        jelite = jelite + 1
      endif

      if (jelite .eq. nchrome) then
        kelite = 1
      endif
    enddo

  enddo

  if (ielite .ne. 0 .and. kelite .eq. 0) then
    call ran3(1, rand)

    irand = 1d0 + dint(dble(npopsiz)*rand)
    do n = 1, nchrome
      iparent(n, irand) = ibest(n)
    enddo

    write(24, 1260) irand
  endif

 1260 format('  Elitist Reproduction on Individual ', i4)

  return
end subroutine newgen


! ##############################################################################
! Micro-GA implementation subroutine
subroutine gamicro(i, npossum, ig2sum, ibest)
  implicit none
  integer, intent(in)                     :: i, npossum, ig2sum
  integer, dimension(nchrmax), intent(in) :: ibest

  integer :: icount, j, n
  real*8  :: diffrac

  ! First, check for convergence of micro population.
  ! If converged, start a new generation with best individual and fill
  ! the remainder of the population with new randomly generated parents.
  ! 
  ! Count number of different bits from best member in micro-population
  icount = 0
  do j = 1, npopsiz
    do n = 1, nchrome
      if (iparent(n, j) .ne. ibest(n)) then
        icount = icount + 1
      endif
    enddo
  enddo

  ! If icount less than 5% of number of bits, then consider population
  ! to be converged.  Restart with best individual and random others.
  diffrac = dble(icount) / dble((npopsiz-1)*nchrome)

  if (diffrac .lt. 0.05d0) then

    do n = 1, nchrome
      iparent(n, 1) = ibest(n)
    enddo

    do j = 2, npopsiz
      do n = 1, nchrome
        call ran3(1, rand)

        iparent(n, j) = 1
        if (rand .lt. 0.5d0) then
          iparent(n, j) = 0
        endif
      enddo
    enddo

    if (npossum .lt. ig2sum) then
      call possibl(parent, iparent)
    endif

    write(6,  1375) i
    write(24, 1375) i
  endif

 1375 format(//'%%%%%%%  Restart micro-population at generation', &
             i5,'  %%%%%%%')

  return
end subroutine gamicro


! ##############################################################################
subroutine restart(i, istart, kount)
  ! This subroutine writes restart information to the ga.restart file.
  implicit none
  integer, intent(in)    :: i, istart
  integer, intent(inout) :: kount
  integer :: l

  kount = kount + 1
  
  if (i .eq. maxgen+istart-1 .or. kount .eq. kountmx) then
    open (unit=25, file=ga_res, status='unknown')
      rewind 25
      write(25, *) i+1, npopsiz
      do j = 1, npopsiz
        write(25, 1500) j, (iparent(l, j), l=1, nchrome)
      enddo
    close (25)

    kount = 0
  endif

 1500 format(i5, 3x, <nchrmax>i2)

  return
end subroutine restart


! ##############################################################################
!!! EOF
