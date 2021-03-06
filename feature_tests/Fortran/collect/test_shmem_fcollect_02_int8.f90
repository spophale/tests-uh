!
!
! Copyright (c) 2011, 2012
!   University of Houston System and Oak Ridge National Laboratory.
! 
! All rights reserved.
! 
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions
! are met:
! 
! o Redistributions of source code must retain the above copyright notice,
!   this list of conditions and the following disclaimer.
! 
! o Redistributions in binary form must reproduce the above copyright
!   notice, this list of conditions and the following disclaimer in the
!   documentation and/or other materials provided with the distribution.
! 
! o Neither the name of the University of Houston System, Oak Ridge
!   National Laboratory nor the names of its contributors may be used to
!   endorse or promote products derived from this software without specific
!   prior written permission.
! 
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
! "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
! LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
! A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
! HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
! SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
! TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
! PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
! LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
! NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
! SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!
!

program test_shmem_collects
  implicit none
  include 'shmem.fh'
 

  integer,        save :: pSync(SHMEM_COLLECT_SYNC_SIZE)

  integer,   parameter :: min_npes = 2
  integer,   parameter :: nelems = 4 
  integer,   parameter :: dest_nelems = nelems * min_npes ! assuming 2 pes ( 2 x 4 elements)

  integer*8             :: src(nelems)
  integer*8             :: dest(dest_nelems)
  integer*8             :: dest_expected(dest_nelems)

  integer, save        :: flag
  integer              :: npes, me
  integer              :: i, pe, k
  logical              :: success
  integer              :: collect_nelems
  integer              :: errcode, abort

! Function definitions
  integer              :: my_pe, num_pes
  

  common /globalvars/ src, dest

  call start_pes(0)

  npes = num_pes()
  me   = my_pe()

  pSync(:) = SHMEM_SYNC_VALUE

  if(npes .ge. min_npes) then

    success = .TRUE.
    flag = 0


    do i = 1, dest_nelems, 1
      dest(i) = -9
    end do

    do i = 1, nelems, 1
      src(i) = i * 100 + me
    end do
    
    k = 1
    do pe = 0, 2, 1
      do i = 1, nelems, 1
        dest_expected(k) = i * 100 + pe  
        k = k + 1
      end do
    end do
    
    call shmem_barrier_all()

    if(me.lt.2) then
      call shmem_fcollect64(dest, src, nelems, &
      0, 0, 2, &
      pSync)

      do i = 1, dest_nelems * npes, 1
        if(dest(i) .ne. dest_expected(i)) then
          if(me .ne. 0) then
            call shmem_int4_inc(flag, 0)
          end if
        end if
      end do

      call shmem_barrier_all()

      if(me .eq. 0) then
        if(flag .ne. 0) then
          success = .FALSE.
        end if

        if(success .eqv. .TRUE.) then
          write(*,*) "Test shmem_fcollect64 with common variables: Passed"
        else
          write(*,*) "Test shmem_fcollect64 with common variables: Failed"
        end if
      end if 
    end if

    call shmem_barrier_all()

  else
    write (*,*) "This test requires ", min_npes, " or more PEs." 
  end if

end program test_shmem_collects
