/* e4event.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"

#if !defined( S4STAND_ALONE ) && !defined( S4OFF_THREAD )

#ifdef S4COM_PRINT
   void S4FUNCTION debug4display( const char *str ) ;
#endif

void code4freeEvent(CODE4 *c4, EVENT4 *event)
{
/* ERRORS

   No errors are possible in this function

   NOTES

   Used to make an EVENT4 available for use later
*/

   #ifdef E4PARM_LOW
      if ( c4 == NULL )
      {
         error4(NULL, e4parmNull, E96932 ) ;
         return ;
      }
   #endif

   l4add(&c4->eventsAvail, event ) ;
}


EVENT4 *code4getEvent(CODE4 *c4 )
{
/* ERRORS

   In case of failure, call error4 and return NULL

   NOTES

   This function is used to obtain an EVENT4 object

*/
   EVENT4 *event ;

   #ifdef E4PARM_LOW
      if (c4 == NULL )
      {
         error4(NULL, e4parmNull, E96933 ) ;
         return NULL ;
      }
   #endif

   event = (EVENT4 *)l4pop(&c4->eventsAvail ) ;
   if (event != NULL )
      return event ;
   // AS May 31/02 - Don't pass in a c4 here.  The reason is because if CODE4 is in an error
   // state prior to calling this function event will end up null.  However, this coding
   // needs to run independent of the main CodeBase engine (for example with communications
   // threads), that still need to operate even if CodeBase is in a so-called error state.
   // event = (EVENT4 *)mem4createAllocNoZero(c4, &c4->eventMemory, MEMORY4START_EVENT, sizeof(EVENT4), MEMORY4EXPAND_EVENT, 0 ) ;
   event = (EVENT4 *)mem4createAllocNoZero( 0, &c4->eventMemory, MEMORY4START_EVENT, sizeof(EVENT4), MEMORY4EXPAND_EVENT, 0 ) ;
   event4init(event) ;
   return event ;
}

void code4clearEvents(CODE4 *c4 )
{
   EVENT4 *event ;

   for(;;)
   {
      event = (EVENT4 *)l4pop(&c4->eventsAvail ) ;
      if (event == NULL )
         break ;
      event4initUndo(event) ;
      mem4free(c4->eventMemory, event) ;
   }
}

int event4init(EVENT4 *event)
{
/* PARAMATERS

   event is the event to initialize

   NOTES

   This function does whatever intialization is required to create a valid
     event.

   In S4WIN32, this function calls the WIN32 API function CreateEvent
     to create a valid event handle, and stores that handle in the
     EVENT4 structure.

   The event should be created as follows:
     no security paramaters
     manual reset, which allows us to wait again in the GetOverlappedResult
       function for the same event.  ** Note that this means that the
       event must be to nonsignalled everytime before it is used in a
       pending operation.
     initial set is non-signalled because no event has occurred.
     event Name is not applicable (i.e. set to NULL)

   RETURNS

   r4success
   < 0 error
*/

   #ifdef E4PARM_LOW
      if (event == NULL )
         return e4parmNull ;
   #endif

   event->handle = CreateEvent( NULL, TRUE, FALSE, NULL ) ;
   if ( event->handle == NULL )
      return e4event ;
   #ifdef S4COM_PRINT
      char buffer[128] ;
      sprintf(buffer, "event4init: %ld", (long)(event->handle) ) ;
      debug4display(buffer) ;
   #endif
   return r4success ;
}

int event4initUndo(EVENT4 *event)
{
/* PARAMATERS

   event is the event to uninitialize

   NOTES

   This function does whatever unintialization is required to destroy a
     valid event

   In S4WIN32, this function calls the WIN32 API function CloseHandle on
     EVENT4.handle.

   RETURNS

   r4success
   < 0 error
*/

   int rc ;

   #ifdef E4PARM_LOW
      if (event == NULL )
         return e4parmNull ;
   #endif

   #ifdef S4COM_PRINT
      char buffer[128] ;
      sprintf(buffer, "event4initUndo: %ld", (long)(event->handle) ) ;
      debug4display(buffer) ;
   #endif
   rc = CloseHandle(event->handle) ;
   if (!rc)
   {
      #ifdef S4COM_PRINT
         sprintf(buffer, "*** event4initUndo FAILED: %ld", (long)rc ) ;
         debug4display(buffer) ;
      #endif
      return e4event ;
   }
   return r4success  ;
}

int event4reset(EVENT4 *event)
{
/* NOTES

   This function resets the event to non-signalled
*/
   int rc ;

   #ifdef E4PARM_LOW
      if (event == NULL )
         return e4parmNull ;
   #endif

   rc = ResetEvent(event->handle) ;
   if (!rc)
      return e4event ;
   return r4success ;
}


/* NOTES

   This function is used to set an event, which indirectly causes any
     thread waiting on the EVENT4 to be activated and able to respond to
     this event.

   In S4WIN32, this function calls the WIN32 API function setEvent
     on the event handle to set the event.
*/
/*
int event4set(EVENT4 *event)
{
   int rc ;

   #ifdef E4PARM_LOW
      if (event == NULL )
         return e4parmNull ;
   #endif

   rc = SetEvent(event->handle) ;
   if (!rc)
      return e4event ;
   return r4success ;
}
*/

int event4wait(EVENT4 *event, int doWait )
{
/* PARAMATERS

   event is the event to wait on
   doWait, if true, results in this routine not returning until the event
     has been setd by another thread.  If false, this function returns
     immediately with the results as to whether or not an event was
     setd.

   RETURNS

   > 0 means the event was setd
   0 means the event was not setd and doWait was false

   NOTES

   This function is used to suspend a thread until the given EVENT4 has
     been seted

   in S4WIN32, this function calls the WIN32 API function
     WaitForSingleObject() on EVENT4.handle
*/

   int rc ;

   #ifdef E4PARM_LOW
      if (event == NULL )
         return e4parmNull ;
   #endif

   if (doWait)
      rc = WaitForSingleObject(event->handle, INFINITE ) ;
   else
      rc = WaitForSingleObject(event->handle, 0) ;

   if (rc == WAIT_TIMEOUT )
      return 0 ;
   if (rc == WAIT_FAILED )
       return -1 ;
   return 1 ;
}

#endif /* #if !defined( S4STAND_ALONE ) && !defined( S4OFF_THREAD ) */
