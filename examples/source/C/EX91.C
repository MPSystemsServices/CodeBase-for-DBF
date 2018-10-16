/*ex91.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( )
{
   CODE4 settings ;
   DATA4 *db ;
   EXPR4 *fullName ;
   char *result, *name ;

   code4init( &settings ) ;
   db = d4open( &settings, "DATA" ) ;

   d4top( db ) ;
   fullName = expr4parse( db, "TRIM( LNAME )+', '+FNAME" ) ;
   name = (char * )malloc( expr4len( fullName ) ) ;
   expr4vary( fullName, &result ) ;
   strcpy( name, result );
   /* make copy of result which is copied over the next time expr4vary is called. */
   d4skip( db, 1 ) ;
   expr4vary( fullName, &result ) ;
   /* For illustration purposes only:
       Avoid using the expression module when the field functions will suffice */

   printf("%s is the first person in the data file\n", name ) ;
   printf("%s is the second person in the data file\n", result ) ;
   free( name ) ;
   code4initUndo( &settings ) ;
}
