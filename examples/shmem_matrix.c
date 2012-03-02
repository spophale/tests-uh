/*
 *
 * Copyright (c) 2011, University of Houston System and Oak Ridge National
 * Laboratory.
 * 
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 * o Redistributions of source code must retain the above copyright notice,
 *   this list of conditions and the following disclaimer.
 * 
 * o Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 * 
 * o Neither the name of the University of Houston System, Oak Ridge
 *   National Laboratory nor the names of its contributors may be used to
 *   endorse or promote products derived from this software without specific
 *   prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */  
  /* Program to calculate the product of 2 matrices A and B based on block distribution.
   * Adopted from the mpi implementation of matrix muliplication based on 1D block-column distribution.
   */ 
  
#include <stdio.h>
#include <stdlib.h>
#include <mpp/shmem.h>
#include <sys/time.h>
#include <unistd.h>
  
gettime () 
{
  
  
  


dt (double *tv1, double *tv2) 
{
  



/* set the number of rows and column here */ 
#define ROWS 800 
#define COLUMNS 800 
  
// routine to print the partial array
  void
print_array (double **array, int blocksize) 
{
  
  
    {
      
	{
	  
	
      printf ("\n");
    
  printf ("\n");
  



// needed for reduction operation
long pSync[_SHMEM_BCAST_SYNC_SIZE];


// global shmem_accesible
double maxtime;


main (int argc, char **argv) 
{
  
  
  
  
  double **a_local, **b_local;
  
  
  
    
  
  
  
  
  
  blocksize = COLUMNS / np;	// block size
  B_matrix_displacement = rank * blocksize;
  
    // initialize the input arrays
    shmem_barrier_all ();
  
  
  
  
    {
      
      
      
      
	{
	  
	  b_local[i][j] = i + 2 * j + 2 * rank + 1;	// random values
	  c_local[i][j] = 0.0;
	
    
  
  
#ifdef DEBUG			// print the input arrays from root process if DEBUG enabled
    if (rank == 0)
    {
      
      
      
      
    
  
#endif	/* 
    
  
    // start the matrix multiplication
    for (i = 0; i < ROWS; i++)
    {
      
	{
	  
	    // compute the partial product of c[i][j]
	    for (k = 0; k < blocksize; k++)
	    {
	      
		{
		  
		    *b_local[k + B_matrix_displacement][j];
		
	    
	  
	    // send a block of matrix A to the adjacent PE
	    shmem_barrier_all ();
	  
	    
	  
	  else
	    
			       rank + 1);
	  
	  
	    // reset the displacement of matrix B to the next block
	    if (B_matrix_displacement == 0)
	    
	  
	  else
	    
	
    
  
  
  
  
#if DEBUG
    printf ("Process %d runtime: %4.2f Sec\n", rank, t / 1000000.0);
  
#endif	/* 
    
    // Determine the maximum of the execution time for individual PEs
    shmem_double_max_to_all (&maxtime, &t, 1, 0, 0, size, pWrk, pSync);
  
#if DEBUG			// print the resultant array from root process if DEBUG enabled
    if (rank == 0)
    {
      
      
    
  
#endif	/* 
    
    {
      
      
    
  


