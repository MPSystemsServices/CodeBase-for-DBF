using System;
using CodeBase;

namespace c4ap
{
   class ex68
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 settings = new Code4() ;
         Data4 info = new Data4( settings, "INFO" ) ; // automatically open data & index file
         Tag4 firstTag = new Tag4() ;
         firstTag.initFirst( info ) ;
 
         info.select( firstTag ) ; // Select first tag of the first open index
 
         int count = 0 ;
         for( info.top( ) ; info.eof( ) == 0 ; info.skip( ) )
            count++ ;
 
         Console.WriteLine( "{0} records in tag {1}\n", count, firstTag.alias( ) ) ;
 
         info.close( ) ;
         settings.initUndo( ) ;
      }
   }
}