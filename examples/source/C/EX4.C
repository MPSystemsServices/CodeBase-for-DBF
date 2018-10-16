#include "d4all.h"


#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( )
{
   CODE4 cb ;
	DATA4 *data ;
	int rc ;

	code4init ( &cb ) ;
   /* Do not add duplicate records to unique tags or the data file and
      return r4unique when attempted.*/
   cb.errDefaultUnique = r4unique ;

   data = d4open( &cb, "INFO" ) ;

   d4top( data ) ;
   d4appendStart( data, 0 ) ;
   rc = d4append( data ) ; /* append a duplicate copy of the top record*/

   if( rc == r4unique )
      printf( "Attempt to add a duplicate record failed.\n") ;
   else
   {
      printf("Attempt to add a duplicate record succeeded\n");
      printf("Record in both data and index file\n") ;
   }

   d4close( data ) ;
   code4initUndo( &cb ) ;
}

