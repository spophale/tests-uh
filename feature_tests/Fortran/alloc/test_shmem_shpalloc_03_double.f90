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

program test_shmem_shpalloc
  implicit none
  include 'shmem.fh'

  ! 1024000000 words is 4096 MB,
  ! symmetric heap is set via env variable SMA_SYMMETRIC_SIZE
  ! there should not be a random size constraint imposed on the heap
  ! integer, parameter :: nelems = 1024000000
  integer, parameter :: nelems 

  integer            :: symm_size=0
  character*50            :: symm_s


  integer*8          :: array_addr
  double precision           :: array(1)    
  pointer            (array_addr, array)
  
  integer            :: errcode, abort, me, npes
  character*(*), parameter :: TEST_NAME='shpalloc'

  ! Function return value types
  integer            :: my_pe, num_pes

  call start_pes(0)

  me = my_pe()
  npes = num_pes()

  ! determine size of nelem, getenv is a non-standard 
  ! BUT the only function available for f90 

  call getenv("SMA_SYMMETRIC_SIZE",symm_s)
  if(symm_s!="") then
    read(symm_s,*) symm_size
    nelems = symm_size/4 !bytes to word
  else
    nelems = 10 
    ! Assumption here is that symmetric heap is atleast 40 bytes 
  end if

  ! allocate remotely accessible block
  call shpalloc(array_addr, nelems, errcode, abort)

  ! Not checking for all error conditions
  ! -1 = length < 0,-2 = no memory

  if(me .eq. 0) then
    if((errcode .ne. -1).and.(errcode.ne.-2)) then
      write (*,*) TEST_NAME, ': Passed'
    else
      write (*,*) TEST_NAME, ': Failed'
    end if
  end if

  ! All PEs wait until PE 0 has finished.
  call shmem_barrier_all()

  !call shpdeallc(array_addr, errcode, abort)
  
end program
