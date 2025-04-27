! ##############################################################################
! This subroutine evaluates the population, assigns fitness,
! establishes the best individual, and outputs information.
subroutine evalout(iskip, iend, ibest, fbar, best)
  implicit none
  integer, intent(in)                        :: iskip, iend
  integer, dimension(nchrmax), intent(inout) :: ibest
  real*8,  intent(inout)                     :: fbar, best
  integer :: j, jstart, jend, k, kk, n, idom
  real*8  :: fitsum, funcval, jbest
  real*8, dimension(nparmax) :: paramsm, paramav

  fitsum = 0.0d0
  !best   = -1.0d10
  best   = 99.0d10

  do n = 1, nparam
    paramsm(n) = 0.0d0
  enddo

  jstart = 1
  jend   = npopsiz

  if (iskip .ne. 0) then
    jstart = iskip
  endif

  if(iend .ne. 0) then
    jend = iend
  endif

  ! TNX: there was a very long loop end in 'TNX-END'
  ! TNX: It is not efficient, because 5 individuals are not simulated parallely
  ! TNX: thus, the long loop is broken into 2 smaller loops
  do j = jstart, jend
    call decode(j, parent, iparent)

    if (iskip .ne. 0 .and. iend .ne. 0 .and. iskip .eq. iend) then
      write(6, 1075) j, (iparent(k, j), k=1, nchrome), &
                        (parent (kk, j), kk=1, nparam), 0.0
    endif

    ! //////////////////////////////////////////////////////////////////////////
    ! TNX: call regcm GA Encoding
    call regcm_GAparEncode(j)

    ! TNX: Write out namelist of each domain (currently maximum is 3)
    do idom = 1, ndom
      ! TNX: Have to double-check if 'i' is taken from global env
      call regcm_namelist(i, j, idom)
    enddo

    ! TNX: Call a bash file to run RegCM simulation and wait until finished
    !      A quick prog to calculate fitness is also added here.
    ! TNX: Have to double-check if 'i' is taken from global env
    call regcm_run(i, j)
  enddo

  do j = jstart, jend
    ! Call function evaluator, write out individual and fitness, and add
    ! to the summation for later averaging.
    ! TNX: fitness is adjusted to read from the output of upper part
    ! TNX: change to read fitness of individual
    ! TNX: Have to double-check if 'i' is taken from global env
    call func(i, j, funcval)

    fitness(j) = funcval
    write(24, 1075) j, (iparent(k, j), k=1, nchrome), &
                       (parent (kk, j), kk=1, nparam), fitness(j)

    fitsum = fitsum + fitness(j)

    do n = 1, nparam
      paramsm(n) = paramsm(n) + parent(n, j)
    enddo

    ! Check to see if fitness of individual j is the best fitness.
    ! TNX: invert .gt. to .lt., since we need smaller bias (i.e., ME, MAE, RMSE)
    !      if fitness is calculated based on CORR -> change back to origin
    ! orig: if (fitness(j) .gt. best) then
    if (fitness(j) .lt. best) then
      best  = fitness(j)
      jbest = j
      do k = 1, nchrome
        ibest(k) = iparent(k, j)
      enddo
    endif
  enddo
  !TNX-END


  ! Compute parameter and fitness averages.
  fbar = fitsum / dble(npopsiz)
  do n = 1, nparam
    paramav(n) = paramsm(n) / dble(npopsiz)
  enddo

  ! Write output information
  if (npopsiz .eq. 1) then
    write(24, 1075) 1, (iparent(k, 1), k=1, nchrome), &
                       (parent (k, 1), k=1, nparam), fitness(1)
    write(24,    *) ' Average Values:'
    write(24, 1275) (parent(k, 1), k=1, nparam), fbar
  else
    write(24, 1275) (paramav(k), k=1, nparam), fbar
  endif

  write(6,  1100) fbar
  write(24, 1100) fbar
  write(6,  1200) best
  write(24, 1200) best

!! 1075 format(i3, 1x, <nchrmax>i1, <nparmax>(1x, f8.4), 1x, f12.5)
 1075 format(i3, 1x, <nchrome>i1, <nparam>(1x, e10.3), 1x, f12.5)
 1100 format(1x, 'Average Function Value of Generation=', f12.5)
 1200 format(1x, 'Best Function Value                 =', f12.5/)
 1275 format(/' Average Values:', 18x, <nparam>(1x, e10.3), 1x, f12.5/)

  return
end subroutine evalout


! ##############################################################################
! This is an N-dimensional version of the multimodal function with
! decreasing peaks used by Goldberg and Richardson (1987, see ReadMe
! file for complete reference).  In N dimensions, this function has
! (nvalley-1)^nparam peaks, but only one global maximum.  It is a
! reasonably tough problem for the GA, especially for higher dimensions
! and larger values of nvalley.
subroutine func(i, j, funcval)
  implicit none
  integer, intent(in)    :: i, j
  real*8,  intent(inout) :: funcval
  character(len=100)     :: ci, cj
  character(len=200)     :: ffitness
  integer                :: iffile
  !TNX real*8 :: nvalley, pi, f1, f2
  !TNX integer :: i

  !TNX nvalley = 6.
  !TNX pi = 4.0d0*datan(1.d0)
  !TNX funcval = 1.0d0
  !TNX do i = 1, nparam
  !TNX   f1 = (sin(5.1d0*pi*parent(i, j) + 0.5d0))**nvalley
  !TNX   f2 = exp(-4.0d0*log(2.0d0)*((parent(i, j)-0.0667d0)**2)/0.64d0)

  !TNX   funcval = funcval*f1*f2
  !TNX enddo

  write(ci, '(I3.3)') i
  write(cj, '(I2.2)') j

  ! TNX: Have to check the file name to be same as in scr.0.rcm.sh
  ffitness = "regcm5-gafitness/fitness_gen"//trim(ci)//"_idv"//trim(cj)//".txt"
  do while (1)
    inquire(file=ffitness, exist=iffile)
    if (iffile) then
      exit
    else
      call sleep(60)
    endif
  enddo
  call sleep(60)
  open(unit=31, file=ffitness, status="old")
    read(31, *) ! Skip the first line (should be: '#   value')
    read(31, *) funcval
  close(31)

  !
  !  As mentioned in the ReadMe file, The arrays have been rearranged
  !  to enable a more efficient caching of system memory.  If this causes
  !  interface problems with existing functions used with previous 
  !  versions of my code, then you can use some temporary arrays to bridge
  !  this version with older versions.  I've named the temporary arrays
  !  parent2 and iparent2.  If you want to use these arrays, uncomment the
  !  dimension statement above as well as the following do loop lines.
  !
  !      do 11 i=1,nparam
  !         parent2(j,i)=parent(i,j)
  ! 11   continue
  !      do 12 k=1,nchrome
  !         iparent2(j,k)=iparent(k,j)
  ! 12   continue
  !
      return
end subroutine func


! ##############################################################################
!!! EOF
