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

/* m4mem2.h   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

/*

   New, more simple, memory module.

   At the lowest level, contains a very low-level re-useable memory container.

   Higher level containers featuring shared memory (via semeaphores, etc.)
   can be built.  Also, debug containers can be built overtop.

   Basically the classes work like this:

   Mem5chunk is a single piece of allocated memory.  It can be put into a linked list.
   For example, to keep a list of chunks of allocated memory.

   Mem5low is a simple re-useable and expandable memory allocater.  When it is
   initialized, it allocates a Mem5chunk of memory and divides that allocation into
   smaller pieces which it puts on the availList and makes available to users via
   the alloc() function.  If the available memory runs out, another chunk is allocated:
   the chunk is added to the chunkList, and the chunk is divided up and each piece
   added to the availList.

   Error Handling:

   Throws Err5memory() errors.

   The main advantage of this is that we don't need to check null returns on allocations.
   This significantly improves the performance of the allocations.
*/

class Mem5piece : public Single4
{
   // a simple piece of memory which includes a Single4 - used for placeholding on alloc
   // functions

   // cannot be virtual - we don't want to set up a virtual jump table.
public:
   void *ptr() { return (void *)p ; }
private:
   char p[1] ;  // array of unknown length (use 1 as placeholder) starts here
} ;



class Mem5allocator
{
public:
   // a simple system-level allocator

   // allocate the size required + size of a Single4, which we permanently put at the top
   // of the Mem5piece - i.e. we need space for the Mem5piece and the allocation

   // this code is not as clear as 2 seperate allocations : 1 for the Mem5piece and 1 for
   // the pointer to memory, but it halves the number of allocations required, so is much
   // more efficient, and thus the more complicated coding.
   inline virtual Mem5piece *alloc( size_t sizeRequired ) { return (Mem5piece *)malloc( (size_t)actualMemorySize( sizeRequired )) ; }
   inline virtual void free( Mem5piece *ptr ) { ::free( ptr ) ; }
   inline virtual void release() { ; }  // not required for this allocator

   // the actual size to allocate, which includes a Single4 object
   /* LY 00/09/20 : from long sizeRequested to size_t (win64) */
   inline size_t actualMemorySize( size_t sizeRequested ) { return sizeRequested + sizeof( Single4 ) ; }
} ;



class Mem5zeroAllocator : public Mem5allocator
{
public:
   virtual Mem5piece *alloc( size_t sizeRequired )
   {
      Mem5piece *ptr = Mem5allocator::alloc( sizeRequired ) ;

      // memset to zero the data part of the Mem5piece only, don't worry about any extra
      // maintenance memory
      memset( ptr->ptr(), 0, (size_t)sizeRequired ) ;
      return ptr ;
   }
} ;



/*
class Mem5chunk : public Single4
{
   // a simple chunk of memory
public:
   void *ptr() { return chunk ; }
private:
   void *chunk ;
   size_t actualSize ;      // the size as known to the chunk administrator - actual allocation size.
   size_t effectiveSize ;   // the size as known to the user -- == size of memory user desires.
} ;



class Mem5chunkAllocator::Mem5allocator
{
   // a more sophisticated allocator of memory - can re-use pieces of memory - doesn't
   // return them to the system.

public:
   Mem5chunkAdministrator() { single4init( availChunks ) ; }
   ~Mem5chunkAdministrator() { releaseChunks() ; }
   Mem5piece *alloc( size_t sizeRequired ) ;  // returns a chunk at least as large as requested
   void free( Mem5piece *chunk ) ;
   void release() ;  // frees up all 'avail' chunks
private:
   Single4 availChunks ;
} ;
*/



class Mem5low
{
   // a simple re-useable memory module
protected:
   Mem5low( Mem5allocator *memAllocator, size_t unitSizeIn, short numStartUnits, short numExpandUnitsIn ) ;
   ~Mem5low() ;

   virtual void *alloc() ;
   virtual inline void free( void *ptr ) { availList.add( (Single4 *)ptr ) ; }
protected:
//   Mem5low() { ; }  // constructor used for derived classes with their own initializers
   // this function cannot be made virtual because we use it on the construction, so
   // it wouldn't get called properly on the construction if over-ridden
   void addMoreMemoryToAvail( short numUnitsToAdd ) ;
   virtual void init() { ; }  // allows initialization prior to calling virtual functions

   Single4 chunkList ;
   Single4 availList ;

   short allocationSize ;
   short numExpandUnits ;    // # additional units of memory to pre-allocate when run out of available units
   Mem5allocator *chunkAllocator ;
} ;



#define MEM5CHECK_PRE_CHAR 0x55
#define MEM5CHECK_POST_CHAR 0x56
#define MEM5NUM_PRE_CHARS  16
#define MEM5NUM_POST_CHARS  20

// character used to fill allocated memory with to ensure that applications work with
// non-zero memory when using a non-zero allocator
#define MEM5DEBUG_FILLER_CHAR 0x54



class Mem5debug : public Mem5low
{
   // adds debugging code to the memory library.  In particular:
   //    - tests for memory overwrites on every free
   //    - tests for memory leakage
   //    - tests that inputs are non-null on free
protected:
   Mem5debug( Mem5allocator *memAllocator, size_t unitSizeIn, short numStartUnits, short numExpandUnitsIn ) ;
   ~Mem5debug() ;

   virtual void *alloc() ;
   virtual void free( void *ptr ) ;
private:
   char preArrayCheck[MEM5NUM_PRE_CHARS] ;   // used for fast comparison of pre-character bytes
   char postArrayCheck[MEM5NUM_POST_CHARS] ; // used for fast comparison of post-character bytes
   long numAllocatedUnits ;  // track the number of units allocated to check for memory leaks
   long numAvailUnits ;  // number of available allocations - can never exceed numAllocated (i.e. free twice)
} ;



class Mem5zeroDebug : public Mem5debug
{
protected:
   Mem5zeroDebug( Mem5zeroAllocator *memAllocator, size_t unitSizeIn, short numStartUnits, short numExpandUnitsIn ) :
      Mem5debug( memAllocator, unitSizeIn, numStartUnits, numExpandUnitsIn )
   { effectiveUnitSizeDeb = unitSizeIn ; }  // the actual quantity for the user
   // normally the debug allocator uses a filler character.  We need to counteract this with
   // a Mem5zeroDebug allocator.
   ~Mem5zeroDebug() { effectiveUnitSizeDeb = 0 ; }

   virtual void *alloc() ;

   size_t effectiveUnitSizeDeb ;  // size less the extra bytes needed for the Single4 link
} ;



#if defined(E4DEBUG)
   // for E4DEBUG, use the Mem5debug class in place of the Mem5 class
   class Mem5 : public Mem5debug
   {
   public:
      Mem5( Mem5allocator *memAllocator, size_t unitSizeIn, short numStartUnits, short numExpandUnitsIn ) :
         Mem5debug( memAllocator, unitSizeIn, numStartUnits, numExpandUnitsIn ) { ; }
      inline virtual void *alloc() { return Mem5debug::alloc() ; }
      inline virtual void free( void *ptr ) { Mem5debug::free( ptr ) ; }
   } ;


   class Mem5zeroInstance : public Mem5zeroDebug
   {
      // just a filler
   public:
      Mem5zeroInstance( Mem5zeroAllocator *memAllocator, size_t unitSizeIn, short numStartUnits, short numExpandUnitsIn ) :
         Mem5zeroDebug( memAllocator, unitSizeIn, numStartUnits, numExpandUnitsIn ) { ; }
   } ;



#else /* not E4DEBUG */

   // for not E4DEBUG, use the Mem5low class in place of the Mem5 class
   class Mem5 : public Mem5low
   {
   public:
      Mem5( Mem5allocator *memAllocator, size_t unitSizeIn, short numStartUnits, short numExpandUnitsIn ) :
         Mem5low( memAllocator, unitSizeIn, numStartUnits, numExpandUnitsIn ) { ; }
      inline virtual void *alloc() { return Mem5low::alloc() ; }
      inline virtual void free( void *ptr ) { Mem5low::free( ptr ) ; }
   } ;



   class Mem5zeroInstance : public Mem5
   {
      // just a filler
   public:
      Mem5zeroInstance( Mem5allocator *memAllocator, size_t unitSizeIn, short numStartUnits, short numExpandUnitsIn ) :
         Mem5( memAllocator, unitSizeIn, numStartUnits, numExpandUnitsIn ) { ; }
   } ;

#endif /* E4DEBUG else */



class Mem5zeroLow : public Mem5zeroInstance
{
   // adds 'init to zero' support to the Mem5low class.
   // it does this very efficiently as follows:
   //    it allocates extra bytes to handle a single4 'link'. the purpose of this is as follows:
   //    when the allocation of memory originally occurs, everything is pre-set to zero.
   //    then, when memory is freed, the freed memory it is set to zero.
   //    when allocating memory, since the memory is already zero, it is just popped off.
   //    we use the extra link outside of the normal memory because we don't want to have to
   //    set the link area to zero on every allocation since this would take more time.
protected:
   // note that the Mem5lowZere requires a zeroing allocator to work properly - i.e. all
   // allocations of low-level memory must zero out their memory when allocating.

   // initialize the Mem5low with input size pluss the size to handle an independent link
   Mem5zeroLow( Mem5zeroAllocator *memAllocator, size_t unitSizeIn, short numStartUnits, short numExpandUnitsIn ) :
      Mem5zeroInstance( memAllocator, unitSizeIn + sizeof( Single4 ), numStartUnits, numExpandUnitsIn )
   { effectiveUnitSize = unitSizeIn ; }  // the actual quantity for the user

   ~Mem5zeroLow() { effectiveUnitSize = 0 ; }

   // throws exception of alloc fails, so don't need to check for NULL
   virtual inline void *alloc() { return ( ((char *)Mem5zeroInstance::alloc())  + sizeof( Single4 ) ) ; }

   virtual inline void free( void *ptr )
   {
      // memset the pointer to zero on free, so we don't need to do on allocate.  Means we
      // can set up allocations at the beginning earlier for an overall performance boost.
      // the only downside is that we memset to zero memory which might not be used.
      memset( ptr, 0, effectiveUnitSize ) ;
      // the link is stored before the ptr paramater, so subtract that first before free
      Mem5zeroInstance::free( ((char *)ptr) - sizeof( Single4 ) ) ;
   }

   size_t effectiveUnitSize ;  // size less the extra bytes needed for the Single4 link
} ;



class Mem5zero : public Mem5zeroLow
{
public:
   Mem5zero( Mem5zeroAllocator *memAllocator, size_t unitSizeIn, short numStartUnits, short numExpandUnitsIn ) :
      Mem5zeroLow( memAllocator, unitSizeIn, numStartUnits, numExpandUnitsIn ) { ; }
   virtual inline void *alloc() { return Mem5zeroLow::alloc() ; }
   virtual inline void free( void *ptr ) { Mem5zeroLow::free( ptr ) ; }
} ;


class Mem5flexLow : public Mem5
{
   // similar to Mem5 but adds the ability to perform a zero-ing allocation by calling
   // ->allocZero().
   // not as efficient as Mem5flex if allocations are always to be initialized to zero,
   // but is more efficient if zero allocations are not always required.
protected:
   // note that the Mem5lowZere requires a zeroing allocator to work properly - i.e. all
   // allocations of low-level memory must zero out their memory when allocating.
   Mem5flexLow( Mem5allocator *memAllocator, size_t unitSizeIn, short numStartUnits, short numExpandUnitsIn ) :
      Mem5( memAllocator, unitSizeIn, numStartUnits, numExpandUnitsIn )
   {
      effectiveUnitSize = unitSizeIn ;
   }
   ~Mem5flexLow() { effectiveUnitSize = 0 ; }

   // throws exception of alloc fails, so don't need to check for NULL
   virtual inline void *alloc() { return Mem5::alloc() ; }
   virtual inline void free( void *ptr ) { Mem5::free( ptr ) ; }

   virtual void *allocZero() ;

protected:
//   virtual void init() { ; }  // allows initialization prior to calling virtual functions

   size_t effectiveUnitSize ;  // size less the extra bytes needed for the Single4 link
} ;



class Mem5flex : public Mem5flexLow
{
   // in non-E4DEBUG, we wan't to use the Mem5 class
public:
   Mem5flex( Mem5allocator *memAllocator, size_t unitSizeIn, short numStartUnits, short numExpandUnitsIn ) :
      Mem5flexLow( memAllocator, unitSizeIn, numStartUnits, numExpandUnitsIn ) { ; }
   virtual inline void *alloc() { return Mem5flexLow::alloc() ; }
   virtual inline void free( void *ptr ) { Mem5flexLow::free( ptr ) ; }
   virtual inline void *allocZero() { return Mem5flexLow::allocZero() ; }
} ;



// other classes to build:
// Mem5debug, Mem5shared (?)
