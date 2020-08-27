using System;
using CodeBase;
 
namespace c4ap
{
   class ex135
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4( ) ;
 
         // automatically open the INFO production index file
         Data4 data = new Data4( cb, "INFO" ) ;
 
         // Copy the Index4 object for the production index file
         Index4 info = data.index( "INFO" ) ;
 
         cb.errOpen = 0 ;
         Index4 names = new Index4( data, "NOT" ) ; // attempt a second index file
         if( names.isValid( ) == 0 )
            Console.WriteLine( "Index file could not be opened\n"); 
 
         cb.closeAll( ) ;
         cb.initUndo( ) ;
      }
   }
}