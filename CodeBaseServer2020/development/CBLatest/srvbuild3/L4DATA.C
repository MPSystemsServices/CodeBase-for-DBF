/* l4data.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"

#ifdef S4SERVER
#ifdef S4DEBUG_LOG

static FIELD4INFO data4logFields[] =
{
   { "ACTION",     'N',   5, 0 },
   { "EXTRA",      'M',  10, 0 },
   { 0, 0, 0, 0 },
} ;

int S4FUNCTION data4logAdd( DATA4LOG *log, long action, void *data, int voidLen )
{
   if ( log->data == 0 )
      return -1 ;
   if ( d4appendStart( log->data, 0 ) < 0 )
      return -1 ;

   f4assignLong( log->actionFld, action ) ;
   f4memoAssignN( log->extraFld, (char *)data, voidLen ) ;

   if ( d4append( log->data ) < 0 )
      return -1 ;
   return 0 ;
}

int S4FUNCTION data4logOpen( DATA4LOG *log, CODE4 *c4, char *name )
{
   #ifdef E4PARM_LOW
      if ( ( log == 0 || c4 == 0 || name == 0 ) )
         return error4( c4, e4parm_null, E95801 ) ;
   #endif

   log->data = d4open( c4, name ) ;

   if ( log->data == 0 )
      return error4( c4, e4open, E85802 ) ;

   log->actionFld = d4field( log->data, "ACTION" ) ;
   log->extraFld = d4field( log->data, "EXTRA" ) ;

   if ( log->actionFld == 0 || log->extraFld == 0 )
      return error4( c4, e4cat, E85801 ) ;
   return 0 ;
}

int S4FUNCTION data4logCreate( DATA4LOG *log, CODE4 *c4, char *name )
{
   log->data = d4create( c4, name, data4logFields, 0 ) ;

   if ( log->data == 0 )
      return -1 ;

   d4close( log->data ) ;

   return data4logOpen( log, c4, name ) ;
}

long S4FUNCTION data4logAction( DATA4LOG *log )
{
   return f4long( log->actionFld ) ;
}

long S4FUNCTION data4logNext( DATA4LOG *log )
{
   int rc ;

   rc = d4skip( log->data, 1L ) ;
   if ( rc != 0 )
      return rc ;

   return data4logAction( log ) ;
}

long S4FUNCTION data4logTop( DATA4LOG *log )
{
   int rc ;

   rc = d4top( log->data ) ;
   if ( rc != 0 )
      return rc ;

   return data4logAction( log ) ;
}

/*
void *S4FUNCTION data4logExtra( DATA4LOG *log )
{
   return f4str( log->extraFld ) ;
}
*/
#endif /* S4DEBUG_LOG */
#endif /* S4SERVER */
