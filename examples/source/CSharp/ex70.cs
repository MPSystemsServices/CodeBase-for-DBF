using System;
using CodeBase;

namespace c4ap
{
   class ex70
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4( ) ;
         // open exclusively to avoid multiuser corruption
         cb.accessMode = Code4.OPEN4DENY_RW ;
 
         Data4 data = new Data4( cb, "INFO" ) ;
         data.go( 1 ) ;
 
         // Make all of the records in the data file the same as the first record
 
         for( int numRecs = data.recCount( ) ; numRecs-1 != 0 ; numRecs -- )
            if( data.write( numRecs ) != 0 )
               break ;
 
         data.close( ) ;
         cb.initUndo( ) ;
      }
   }
}