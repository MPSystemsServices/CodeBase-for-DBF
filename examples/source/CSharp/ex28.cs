using System ;
using CodeBase ;

namespace c4ap
{
   class ex28
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4() ;
         Data4 data = new Data4( cb, "INFO" ) ;

         if( cb.errorCode != 0 )
         {
            Console.WriteLine( "An error occurred in the Data4 constructor" ) ;
            cb.exit( ) ;
         }

         data.top() ;
         Console.Write( "Number of records in " + data.fileName( ) + ": " ) ;
         Console.WriteLine( data.recCount( ) ) ;

         data.close( ) ;
         cb.initUndo( ) ;
      }
   }
}