//ex59.cpp
// Copy records from one database to another.
#include "d4all.hpp"

extern unsigned _stklen = 10000;

void main()
{
   Code4  cb ;
   Data4 from( cb, "DATA1" ) ;
   cb.optStart( ) ;

   Data4 to(cb, "DATA2" ) ;

   /* Database 'DATA2' has an identical database
      structure as database 'DATA1'. */

   if ( from.recWidth( ) != to.recWidth( ) )
   {
      cb.error( e4result, 0,  "Structures not identical" ) ;
      cb.exit( ) ;
   }

   cb.exitTest(  ) ;
   for ( long i_rec = 1L; i_rec <= from.recCount( ); i_rec++ )
   {
      /* Read the database record. */
      from.go( i_rec ) ;

      /* Copy the database buffer. */
      to.appendStart( 0 ) ;
      memcpy( to.record( ), from.record( ), to.recWidth( ) ) ;

      /* Append the database record. */
      to.append( ) ;
   }
   cb.closeAll( ) ;
   cb.initUndo( ) ;
}
