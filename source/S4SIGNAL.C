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

#include "d4all.h"
#ifndef S4STAND_ALONE
#ifndef S4OFF_THREAD

static int signal4arrayWaitHere(SIGNAL4ROUTINE_ARRAY *, unsigned long, int, int, SIGNAL4ROUTINE ** ) ;


int signal4routineInit(SIGNAL4ROUTINE *signalRoutine, HANDLE handle, COMPLETION4ROUTINE *routine, void *signalStructure, void *data )
{
/* ERRORS

   In case of failure, call error4 with a null CODE4 (since none is
     available) and return the appropriate error code.

   PARAMATERS

   semaphore is the SIGNAL4ROUTINE to initialize
   handle is the handle which, when signalled, will result in the routine
     being executed.
   routine is the routine to call when the handle is signalled
   signalStructure is either an EVENT4 or a SEMAPHORE4, either of which are
     associated with the signal structure
   data is the data which will be assigned to SIGNAL4ROUTINE.data.

   NOTES

   This function assigns the paramaters to the appropriate parts of the
     structure

   RETURNS

   r4success
   < 0 error
*/

   #ifdef E4PARM_LOW
       if ( (signalRoutine == NULL )||(signalStructure == NULL )  )
          return (error4(NULL, e4parmNull, E96938 )) ;
   #endif

   signalRoutine->handle = handle ;
   signalRoutine->routine = routine ;
   signalRoutine->data = data ;
   signalRoutine->signalStructure = signalStructure ;
   return r4success ;
}



int signal4arrayAdd(SIGNAL4ROUTINE_ARRAY *signalArray, SIGNAL4ROUTINE *signal, int permenantItem )
{
/* ERRORS

   In case of error, call error4() with a NULL CODE4 (since one is not
     available), and an error code of e4result.  This severe error
     indicates an unrecoverable failure, and indicates a major failure.
     Return NULL.

   NOTES

   Just adds the handle and signal to the next available array entry.  If
     there are no entries left, the array is re-allocated to a larger size
     (say 100 more elements (cost is 800 bytes), and copied over, then the
     previous one is freed).

   The HANDLE is set to SIGNAL4ROUTINE.signal

   SIGNAL4ROUTINE_ARRAY.numActiveElements is incremented by 1.

   Sets signal.inArray to 1
*/

   int newSize, current ;
   void *mem1, *mem2 ;

   #ifdef E4PARM_LOW
      if ( (signalArray == NULL) || (signal == NULL) )
         return ( error4(NULL, e4parmNull, E96939 ) ) ;
      if (signalArray->numActiveElements > signalArray->maxElements )
         return ( error4(NULL, e4parm, E96939 ) ) ;
   #endif

   if ( signalArray->numActiveElements == signalArray->maxElements )
   {
   /* We need to allocate more space */
      newSize = signalArray->maxElements + 100 ;
      mem1 = u4allocFree(NULL, newSize*sizeof(SIGNAL4ROUTINE)  ) ;
      if (mem1 == NULL )
         return e4memory ;
      mem2 = u4allocFree(NULL, newSize*sizeof(HANDLE)  ) ;
      if (mem2 == NULL )
      {
         u4free(mem1) ;
         return e4memory ;
      }
      memcpy(mem1, signalArray->signalRoutineArray, signalArray->maxElements*sizeof(SIGNAL4ROUTINE) ) ;
      u4free(signalArray->signalRoutineArray) ;
      signalArray->signalRoutineArray  = (struct SIGNAL4ROUTINESt **)mem1 ;

      memcpy(mem2, signalArray->handleArray, signalArray->maxElements*sizeof(HANDLE) ) ;
      u4free(signalArray->handleArray);
      signalArray->handleArray = (void **)mem2 ;

      signalArray->maxElements = newSize ;
   }

   for(current = signalArray->numActiveElements; current > signalArray->permenants; current--)
   {
      signalArray->handleArray[current] = signalArray->handleArray[current-1] ;
      signalArray->signalRoutineArray[current] = signalArray->signalRoutineArray[current-1] ;
      signalArray->signalRoutineArray[current]->arrayPosition = current ;
   }
   /* We've now created a hole between the permenants and the non */
   signalArray->numActiveElements++ ;
   signalArray->signalRoutineArray[signalArray->permenants] = signal ;
   signalArray->handleArray[signalArray->permenants] = signal->handle ;
   signal->inArray = 1 ;
   signal->arrayPosition = signalArray->permenants ;
   if (permenantItem)
      signalArray->permenants++ ;
   return r4success ;
}

int signal4arrayInit(SIGNAL4ROUTINE_ARRAY *signalArray, CODE4 *c4, int numArrayElements )
{
/* PARAMATERS

   numArrayElements is the number of elements to initially allocate for the
     arrays.  It is most efficient if we do not ever have to re-allocate the
     arrays in order to increase its size, but there is a memory cost.
     If -1, then allocates for 100 signals.

   CODE4 is input in order for the u4allocFree() memory allocation to
     free up memory if required and cause a CodeBase failure if no memory
     could be allocated.

   ERRORS

   Call u4allocFree() for the allocation, which will generate an error
     if the allocation fails.  In that case, return e4memory (do not
     need to call error4() since u4allocFree() already does)
   In case of any other failure, call error4() with a severe failure
     (either e4result or e4info), and return the error code.

   NOTES

   Both the handleArray and signalArray are allocated based on the input
     numArrayElements.

   SIGNAL4ROUTINE_ARRAY.maxElements, which representds the number of
     elements allocated for, is set to numArrayElements.
*/

   int size ;

   #ifdef E4PARM_LOW
      if (signalArray == NULL || c4 == NULL || numArrayElements == 0 )
         return ( error4(c4, e4parmNull, E96940 ) ) ;
   #endif

   if (numArrayElements < 0 )
      size = 100 ;
   else
      size = numArrayElements ;

   signalArray->signalRoutineArray = (struct SIGNAL4ROUTINESt **)u4allocFree(c4, size*sizeof(SIGNAL4ROUTINE)  ) ;
   if (signalArray->signalRoutineArray == NULL )
      return e4memory ;
   signalArray->handleArray = (void **)u4allocFree(c4, size*sizeof(HANDLE)  ) ;
   if (signalArray->handleArray == NULL )
   {
      u4free(signalArray->signalRoutineArray) ;
      return e4memory ;
   }
   signalArray->maxElements = size ;
   signalArray->numActiveElements = 0 ;
   return r4success ;
}

void signal4arrayInitUndo(SIGNAL4ROUTINE_ARRAY *signalArray )
{
/* ERRORS

   ignore any errors and continue with uninitialization

   NOTES

   uninitializes the signal array structure:
     u4free( handleArray )
     u4free( signalRoutineArray )
     memset structure to zero
*/


   #ifdef E4PARM_LOW
      if (signalArray == NULL )
      {
         error4(NULL, e4parmNull, E96941 ) ;
         return ;
      }
   #endif

   u4free(signalArray->signalRoutineArray) ;
   u4free(signalArray->handleArray);
}

void signal4arrayRemoveSignal(SIGNAL4ROUTINE_ARRAY *signalArray,  SIGNAL4ROUTINE *signal )
{
/* ERRORS

   In case of error, call error4() with a NULL CODE4 (since one is not
     available), and an error code of e4result.  This severe error
     indicates an unrecoverable failure, and indicates a major failure.
     Return NULL.

   NOTES

   This function checks the signal.inArray paramater.  If it is false, it
     simply returns (i.e. the signal is not in the array)

   Otherwise, this function removes the signalArray and corresponding
     handle from the array.

   This function should first check SIGNAL4ROUTINE.arrayPosition to see
     if the SIGNAL4ROUTINE is in that position.  If it is not, then the
     entire signalArray must be scanned to find the entry which is equal
     to signal.

   Once the location within the array is discovered, the signal and its
     corresponding handle are removed from their respective arrays, and
     the numActiveElements is decremented by 1
*/

   SIGNAL4ROUTINE **ptr ;
   int pos, last, current ;

   #ifdef E4PARM_LOW
      if ( (signalArray == NULL) || (signal == NULL) )
      {
         error4(NULL, e4parmNull, E96942 ) ;
         return ;
      }
   #endif

   if (!signal->inArray)
      return;

   if (signalArray->signalRoutineArray[signal->arrayPosition] == signal )
      pos = signal->arrayPosition ;
   else
   {
      /* got to find the position */
      for (ptr = signalArray->signalRoutineArray, pos = 0; (pos < signalArray->numActiveElements)&&(signal != *ptr)  ; pos++, ptr++)
         ;
      if (pos == signalArray->numActiveElements)
      {
         error4(NULL, e4signal, E96942 ) ;
         return ;
      }
   }
   /* AS 12/09/97 array must be ordered in order for multiply satisfied read operations to remain ordered for completion */

   last = --signalArray->numActiveElements ;

   for ( current = pos ; current < last ; current++ )
   {
      signalArray->handleArray[current] = signalArray->handleArray[current+1] ;
      signalArray->signalRoutineArray[current] = signalArray->signalRoutineArray[current+1] ;
      signalArray->signalRoutineArray[current]->arrayPosition = current ;

   }
/*
   signalArray->handleArray[pos] = signalArray->handleArray[last] ;
   signalArray->signalRoutineArray[pos] = signalArray->signalRoutineArray[last] ;
   signalArray->signalRoutineArray[pos]->arrayPosition = pos ;
*/
   signal->inArray = 0 ;
}

SIGNAL4ROUTINE *signal4arrayGet(SIGNAL4ROUTINE_ARRAY *signalArray, int index )
{
/* ERRORS

   In case of error, call error4() with a NULL CODE4 (since one is not
     available), and an error code of e4result.  This severe error
     indicates an unrecoverable failure, and indicates a major failure.
     Return NULL.

   NOTES

   This function is used to obtain the SIGNAL4ROUTINE structure based on
     the input array index.

   This function sets SIGNAL4ROUTINE.arrayPosition to index (which
     is used later to speed up array removal)

   This function returns SIGNAL4ROUTINE_ARRAY.signalArray[index].
*/

   #ifdef E4PARM_LOW
      if (signalArray == NULL )
      {
         error4(NULL, e4parmNull, E96943 ) ;
         return NULL ;
      }
   #endif

   signalArray->signalRoutineArray[index]->arrayPosition = index ;
   return( signalArray->signalRoutineArray[index] ) ;
}

int signal4arrayWait(SIGNAL4ROUTINE_ARRAY *signalArray, SIGNAL4ROUTINE **signal, unsigned long waitTime )
{
/* RETURNS

   The SIGNAL4ROUTINE_ARRAY which was released causing the thread suspension
     to end

   NULL is returned if an error occurs.

   ERRORS

   In case of error, call error4() with a NULL CODE4 (since one is not
     available), and an error code of e4result.  This severe error
     indicates an unrecoverable failure, and indicates a major failure.
     Return NULL.

   NOTES

   This function is used to suspend a thread until one of the signals in
     the SIGNAL4ROUTINE_ARRAY has been released

   In S4WIN32, this function performs a WaitForMultipleObjects() on
     SIGNAL4ROUTINE_ARRAY.handleArray
   This function waits infinitely until a signal is released

   ADDITIONAL

   While running this in Windows95, I frequently got the error
   ERROR_INVALID_HANDLE under certain conditions. This didn't  seem
   warranted, but a work around was found. If I get that error, then
   I  check again. The second time there is usually no problem. If there
   is, then we  display  an  error  message.

*/

   int rc ;
   int numWaitsEntry ;
   int offset ;
   int temp ;
   unsigned long i ;
   #ifdef E4PARM_LOW
      if (signalArray == NULL )
      {
         error4(NULL, e4parmNull, E96944 ) ;
         return NULL ;
      }
   #endif


   if (signalArray->numActiveElements < MAXIMUM_WAIT_OBJECTS )
   {
      rc = signal4arrayWaitHere(signalArray, waitTime, 0, signalArray->numActiveElements, signal ) ;
      return rc ;
   }
   else
   {
      for (i = 0; ( (i < waitTime) || (waitTime == INFINITE) ); i+= 11 ) /* 11, not 10, because we're doing a lot of other processing */
      {
         for (offset = 0; offset <= signalArray->numActiveElements; )
         {
            temp = signalArray->numActiveElements - offset ;
            /* numWaitsEntry = min(MAXIMUM_WAIT_OBJECTS, signalArray->numActiveElements-offset ) ; */
            numWaitsEntry = MAXIMUM_WAIT_OBJECTS > temp ? temp : MAXIMUM_WAIT_OBJECTS ;
            if (!numWaitsEntry)
               break ;
            rc = signal4arrayWaitHere(signalArray, 0, offset, numWaitsEntry, signal ) ;
            if (rc != e4timeOut )
               return rc ;
            offset += MAXIMUM_WAIT_OBJECTS ;
         }
         rc = signal4arrayWaitHere(signalArray, 10, 0, MAXIMUM_WAIT_OBJECTS, signal ) ;
         if ( rc != e4timeOut )
            return rc ;
      }
      *signal = NULL ;
      return e4timeOut ;
   }
}



static int signal4arrayWaitHere(SIGNAL4ROUTINE_ARRAY *signalArray, unsigned long waitTime, int offset, int elements, SIGNAL4ROUTINE **signal )
{
/* RETURNS

   ADDITIONAL

   While running this in Windows95, I frequently got the error
   ERROR_INVALID_HANDLE under certain conditions. This didn't  seem
   warranted, but a work around was found. If I get that error, then
   I  check again. The second time there is usually no problem. If there
   is, then we  display  an  error  message.

*/

   int rc,  rc2 ;

   #ifdef E4PARM_LOW
      if (signalArray == NULL )
      {
         error4(NULL, e4parmNull, E96944 ) ;
         return NULL ;
      }
   #endif

   rc = WaitForMultipleObjects(elements, &signalArray->handleArray[offset], FALSE, waitTime ) ;
   if (rc == WAIT_FAILED)
   {
      rc2 = GetLastError() ;
      if (rc2 == ERROR_INVALID_HANDLE)
         rc = WaitForMultipleObjects(elements, &signalArray->handleArray[offset], FALSE, 0 ) ;
      if (rc == WAIT_FAILED)
      {
         *signal = NULL ;
         error4(NULL, e4result, E96944 )  ;
         return e4result ;
      }
   }
   else if (rc == WAIT_TIMEOUT)
   {
      *signal = NULL ;
      return e4timeOut ;
   }
   rc -= WAIT_OBJECT_0 ;
   *signal = signalArray->signalRoutineArray[rc+offset];
   return r4success ;
}

#endif /*!S4OFF_THREAD  */
#endif /*!S4STAND_ALONE */
