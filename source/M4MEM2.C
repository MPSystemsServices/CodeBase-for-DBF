/* *********************************************************************************************** */
/* Copyright (C) 1999-2015 by Sequiter, Inc., 9644-54 Ave, NW, Suite 209, Edmonton, Alberta Canada.*/
/* This program is free software: you can redistribute it and/or modify it under the terms of      */
/* the GNU Lesser General Public License as published by the Free Software Foundation, version     */
/* 3 of the License.                                                                               */
/*                                                                                                 */
/* This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;       */
/* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.       */
/* See the GNU Lesser General Public License for more details.                                     */
/*                                                                                                 */
/* You should have received a copy of the GNU Lesser General Public License along with this        */
/* program. If not, see <https://www.gnu.org/licenses/>.                                           */
/* *********************************************************************************************** */

/* m4mem2.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "oledb5.hpp"

#ifdef OLEDB5BUILD

int single4find( Single4 *lst, Single4 *item );
/*
Mem5low::Mem5low( short unitSizeIn, size_t numStartUnits, short numExpandUnitsIn )
{

}
*/



Mem5low::Mem5low( Mem5allocator *memAllocator, size_t unitSizeIn, short numStartUnits, short numExpandUnitsIn )
{
   //
   // the allocator passed in is used to allocate the large chunks of memory which are then
   // divided into the smaller units which are returned to the user.
   //
   // error handling - throws Err5memory if valid error, Err5internal if debug error
   //

   assert5( unitSizeIn > 0 ) ;
   assert5( numStartUnits > 0 ) ;
   assert5( numExpandUnitsIn > 0 ) ;

   chunkList.init() ;
   availList.init() ;
   chunkAllocator = memAllocator ;

   numExpandUnits = numExpandUnitsIn ;
   if ( unitSizeIn < sizeof( Single4 ) )  // we need to have enough to track the memory
      allocationSize = sizeof( Single4 ) ;
   else
      allocationSize = unitSizeIn ;

   addMoreMemoryToAvail( numStartUnits ) ;
}



Mem5low::~Mem5low()
{
   //
   // error handling - throws Err5memory if valid error, Err5internal if debug error
   //
   Single4distant chunkIterator ;

   // free up all the chunks of memory we allocated via the chunkAllocator

   chunkIterator.initIterate( &chunkList ) ;
   for( ;; )
   {
      Mem5piece *currentChunk = (Mem5piece *)chunkIterator.toItem() ;
      if ( currentChunk == 0 )
         break ;
      chunkIterator.remove() ;
      chunkAllocator->free( currentChunk ) ;
   }

   // clear memory out just in case object incorrectly gets re-used
   allocationSize = 0 ;
   numExpandUnits = 0 ;
   availList.init() ;
   chunkAllocator = 0 ;
}



void Mem5low::addMoreMemoryToAvail( short numUnitsToAdd )
{
   //
   // uses the chunkAllocator to allocate a chunk of memory of size numUnitsToAdd * allocationSize
   //
   // error handling - throws Err5memory if valid error, Err5internal if debug error
   //

   long amtAlloc = allocationSize * numUnitsToAdd ;

   Mem5piece *memoryChunk = chunkAllocator->alloc( amtAlloc ) ;

   if ( memoryChunk == 0 )
      throw Err5memory() ;

   // so we can free the memory memoryChunk later, keep a handle to it...
   chunkList.add( memoryChunk ) ;

   // split the memoryChunk into allocationSize units and add each unit to the avail list

   // We coerce each unit to be a Single4 for the purposes of being on the list.  This
   // avoids a seperately allocated item for each piece to keep track of this.  Note that
   // we have guaranteed that the allocationSize is >= sizeof( Single4 ) during the constructor
   // of the Mem5low object.

   assert5( allocationSize >= sizeof( Single4 ) ) ;

   Single4 *currentUnit = (Single4 *)(memoryChunk->ptr()) ;
   for ( short unitIndex = 0 ; unitIndex < numUnitsToAdd ; unitIndex++ )
   {
      availList.add( currentUnit ) ;
      // the next unit is allocationSize bytes away from the current unit
      currentUnit = (Single4 *)(((char *)currentUnit) + allocationSize ) ;
   }
}



void *Mem5low::alloc()
{
   //
   // if avail list empty add more memory to the avail list
   //
   // then just pop a link off the chain and pass the handle back.
   //
   //
   // error handling - throws Err5memory if valid error, Err5internal if debug error
   //

   Single4distant iterator ;
   iterator.initIterate( &availList ) ;

   void *ptr = iterator.toItem() ;

   if ( ptr == 0 )
   {
      addMoreMemoryToAvail( numExpandUnits ) ;
      iterator.initIterate( &availList ) ;
      ptr = iterator.toItem() ;
      assert5( ptr != 0 ) ;
   }

   iterator.remove() ;

   return ptr ;
}



Mem5debug::Mem5debug( Mem5allocator *memAllocator, size_t unitSizeIn, short numStartUnits, short numExpandUnitsIn ) :
   // initialize Mem5low with input size plus room for the checking bytes
   Mem5low( memAllocator, unitSizeIn + MEM5NUM_PRE_CHARS + MEM5NUM_POST_CHARS, numStartUnits, numExpandUnitsIn )
{
   //
   // error handling - throws Err5memory if valid error, Err5internal if debug error
   //

   // verify that the arrays are the same size as the extra characters
   assert5( sizeof(preArrayCheck ) == MEM5NUM_PRE_CHARS ) ;
   assert5( sizeof(postArrayCheck ) == MEM5NUM_POST_CHARS ) ;

   // we track the number of units allocated to check for memory leaks at the end
   // since the Mem5low has already allocated numStartUnits, start our counter there.
   // we cannot track by overriding the addMoreMemory because it is called by the Mem5low
   // constructor, so we cannot catch that call
   numAllocatedUnits = numStartUnits ;
   numAvailUnits = numAllocatedUnits ;

   // these arrays are used to quickly check that the mem4 allocations haven't been tampered with
   // initialize them to the debug/check character.
   memset( preArrayCheck, MEM5CHECK_PRE_CHAR, MEM5NUM_PRE_CHARS ) ;
   memset( postArrayCheck, MEM5CHECK_POST_CHAR, MEM5NUM_POST_CHARS ) ;
}



Mem5debug::~Mem5debug()
{
   // just make sure that the comparison arrays have not been tampered with (i.e. corrupted)
   //
   // error handling - throws Err5memory if valid error, Err5internal if debug error
   //

   // verify the pre-loop not tampered with
   for ( int preLoop = 0 ; preLoop < MEM5NUM_PRE_CHARS ; preLoop++ )
   {
      if ( preArrayCheck[preLoop] != MEM5CHECK_PRE_CHAR )
         throw Err5internal(0) ;
   }

   // verify the post-loop not tampered with
   for ( int postLoop = 0 ; postLoop < MEM5NUM_POST_CHARS ; postLoop++ )
   {
      if ( postArrayCheck[postLoop] != MEM5CHECK_POST_CHAR )
         throw Err5internal(0) ;
   }

   // verify no memory leakage -- i.e. that all allocated blocks are on the avail chain
   Single4distant leakIterator ;

   leakIterator.initIterate( &availList ) ;

   for( long numAvail = 0 ;; numAvail++ )
   {
      // done list
      if ( leakIterator.toItem() == 0 )
      {
         // if numAvail != numAllocatedUnits then we have a memory leak because some items
         // were allocated and then not put back onto the avail list
         if ( numAvail != numAllocatedUnits )
            throw Err5internal(0) ;
         break ;
      }

      // verify that we haven't had more units freed than were actually allocated
      if ( numAvail >= numAllocatedUnits )
         throw Err5internal(0) ;

      leakIterator.next() ;
   }

   numAllocatedUnits = 0 ;
}



void *Mem5debug::alloc()
{
   // for the debug version, we assign the debug characters and return a pointer offset into
   // the memory.  When free() is called, the characters are verfified to make sure they
   // haven't been modified.
   //
   // error handling - throws Err5memory if valid error, Err5internal if debug error
   //

   Single4distant iterator ;
   iterator.initIterate( &availList ) ;

   int addedItems = 0 ;
   if ( iterator.toItem() == 0 )  // means we will run out of units, so note that we are going to add if successful
      addedItems = 1 ;

   char *ptr = (char *)Mem5low::alloc() ;

   // should never return 0, instead should have thrown an Err5memory exception
   if ( ptr == 0 )
      throw Err5internal(0) ;

   // items were added and pointer not 0, so increment counter
   if ( addedItems )
   {
      // just do a back check to make sure items on list now...
      iterator.initIterate( &availList ) ;
      assert5( iterator.toItem() != 0 ) ;

      numAllocatedUnits += numExpandUnits ;
      numAvailUnits += numExpandUnits ;
   }

   memset( ptr, MEM5CHECK_PRE_CHAR, MEM5NUM_PRE_CHARS ) ;
   memset( ptr + allocationSize - MEM5NUM_POST_CHARS, MEM5CHECK_POST_CHAR, MEM5NUM_POST_CHARS ) ;

   // and now fill the allocation with a filler character to verify runs ok when non-zero memory
   memset( ptr + MEM5NUM_PRE_CHARS, MEM5DEBUG_FILLER_CHAR, allocationSize - MEM5NUM_POST_CHARS - MEM5NUM_PRE_CHARS ) ;

   numAvailUnits-- ;
   return ptr + MEM5NUM_PRE_CHARS ;
}



void Mem5debug::free( void *p )
{
   // verifies that the debug characters have not been modified

   // should never call with p 0
   if ( p == 0 )
      throw Err5internal(0) ;

   char *ptr = (char *)p - MEM5NUM_PRE_CHARS ;

   if ( memcmp( ptr, preArrayCheck, MEM5NUM_PRE_CHARS ) != 0 )
   {
      // the characters are not the same as when the alloc occurred.  This means they have
      // been changed, indicating a memory underwrite occurred.
      throw Err5internal(0) ;
   }

   if ( memcmp( ptr + allocationSize - MEM5NUM_POST_CHARS, postArrayCheck, MEM5NUM_POST_CHARS ) != 0 )
   {
      // the characters are not the same as when the alloc occurred.  This means they have
      // been changed, indicating a memory overwrite occurred.
      throw Err5internal(0) ;
   }

   // search the list of pointers to ensure that this item is not already there (i.e. a second free of same item)
   if ( single4find( &availList, (Single4 *)ptr ) == 1 )  // already freed before...
      throw Err5internal(0) ;

   numAvailUnits++ ;
   if ( numAvailUnits > numAllocatedUnits )  // would mean our # of frees > # of allocations
      throw Err5internal(0) ;

   Mem5low::free( ptr ) ;

   if ( single4find( &availList, (Single4 *)ptr ) != 1 )  // should have been put on the list
      throw Err5internal(0) ;
}



int single4find( Single4 *lst, Single4 *item )
{
   Single4distant distant ;
   distant.initIterate( lst ) ;

   for ( ;; )
   {
      if ( distant.toItem() == 0 )
         return 0 ;
      if ( distant.toItem() == item )
         return 1 ;
      distant.next() ;
   }
}



void *Mem5flexLow::allocZero()
{
   void *ptr = Mem5::alloc() ;

   memset( ptr, 0, effectiveUnitSize ) ;

   return ptr ;
}



void *Mem5zeroDebug::alloc()
{
   // counteractor for debugging with filler characters
   void *ptr = Mem5debug::alloc() ;

   memset( ptr, 0, effectiveUnitSizeDeb ) ;

   return ptr ;
}


#endif /* OLEDB5BUILD */
