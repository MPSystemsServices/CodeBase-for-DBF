using System ;
using CodeBase ;

namespace c4ap
{
   class ex32
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4() ;
         Data4 data = new Data4( cb, "DATA1" ) ;
         data.lockAll( ) ;
         for( data.top( ) ; data.eof() == 0 ; data.skip() )
            data.blank( )  ;  // blank out all records in the data file

         cb.initUndo( ) ;    // close all files and release any memory
      }
   }
}