using System ;
using CodeBase ;

namespace c4ap
{
   class ex36
   {
      /* Check the validity of an index file. */
      [STAThread]
      static void Main(string[] args)
      {
         if ( args.Length > 1 )
         {
            Code4 cb = new Code4() ;
            cb.autoOpen = 0 ;  // open index file manually.
            cb.errOff = 1 ;

            Data4 checkData = new Data4( cb, args[0] ) ;

            // Demonstration of Index4::open instead of Index4::Index4
            Index4 testIndex = new Index4() ;
            testIndex.open( checkData, args[1] ) ;

            cb.exitTest( ) ;
            cb.optStart( ) ;

            cb.errorCode = 0;
            if ( checkData.checkIndex() == 0 )
               Console.WriteLine( "\nIndex is OK !!" ) ;
            else
               Console.WriteLine( "\nProblem with Index" ) ;
            cb.initUndo( ) ;
         }
         else
         {
            Console.WriteLine() ;
            Console.WriteLine( "PROGRAM  DataFile  IndexFile" ) ;
         }
      }
   }
}