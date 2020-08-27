using System ;
using CodeBase ;

namespace c4ap
{
   class ex19
   {
      static void openAFile( ref Code4 cb )
      {
         // 'd' falls out of scope.  Data file is still open
         Data4 d = new Data4( cb, "INFO" ) ;
      }

      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4() ;
         openAFile( ref cb ) ;

         Data4 d = cb.data( "INFO" ) ; // obtain a new Data4 object

         if( d.isValid( ) != 0 )
         {
            Console.WriteLine( "INFO has " + d.recCount( ) + " records." ) ;

            d.top( ) ;
            cb.data( "INFO" ).close( ) ; // an alternate way to close the file
         }
         cb.initUndo( ) ;
      }
   }
}