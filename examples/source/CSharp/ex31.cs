using System ;
using CodeBase ;

namespace c4ap
{
   class ex31
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4() ;
         Data4 data = new Data4( cb, "INFO" ) ;

         // Add 5 blank records
         for( int i = 5 ; i != 0 ; i-- )
            data.appendBlank( ) ;

         // Close the data file.
         data.close( ) ;
         cb.initUndo( ) ; // Free up any memory used.
      }
   }
}