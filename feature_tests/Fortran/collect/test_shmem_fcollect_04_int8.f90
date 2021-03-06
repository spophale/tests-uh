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
 
  integer              :: npes

! Function definitions
  integer              :: num_pes
  
  call start_pes(0)
  npes = num_pes()
  if(npes .ge. 2) then
    call sub1(npes)
  else
    write (*,*) "This test requires 2 or more PEs." 
  end if

end program test_shmem_collects

subroutine sub1(npes)
  implicit none
  include 'shmem.fh'

! Function definitions
  integer              :: my_pe

  integer              :: npes, me
  integer,        save :: pSync(SHMEM_COLLECT_SYNC_SIZE)

  integer*8             :: src
  integer*8            :: src_addr
  pointer              (src_addr, src)

  integer*8             :: dest(npes)
  integer*8            :: dest_addr
  pointer              (dest_addr, dest)

  integer, save        :: flag
  integer              :: i
  integer              :: errcode, abort
  logical              :: success
  
  me   = my_pe()

  pSync(:) = SHMEM_SYNC_VALUE


    success = .TRUE.
    flag = 0

    call shpalloc (dest_addr, npes, errcode, abort)
    call shpalloc(src_addr, 1, errcode, abort) !every PE contributes one element


    do i = 1,npes,1
      dest(i) = -9
    end do

    src =  101 + me     !0 contributes 101, 1 contributes 102..
    
    call shmem_barrier_all()
  

    !Checking correctness if collect over Active set of all even PEs
    if(mod(me,2).eq.0) then
      if(mod(npes,2).eq.0) then
        call shmem_fcollect64(dest, src,1,0,1, npes/2, &
      pSync)
      else
        call shmem_fcollect64(dest, src,1,0,1, npes/2+1, &
      pSync)
      end if
      do i = 1, npes, 1
        if(dest(i) .ne. (100+i)) then
            call shmem_int4_inc(flag, 0)
        end if
      end do
  
    end if

    call shmem_barrier_all()

    if(me .eq. 0) then
      if(flag .eq. 0) then
        write(*,*) "Test shmem_fcollect64: Passed"
      else
        write(*,*) "Test shmem_fcollect64: Failed"
      end if
    end if 

    call shmem_barrier_all()

    call shpdeallc (dest_addr, errcode, abort)
    call shpdeallc(src_addr, errcode, abort)


end subroutine sub1


