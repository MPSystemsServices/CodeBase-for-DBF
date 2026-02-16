using System ;
using CodeBase ;

namespace c4ap
{
   class ex13
   {
      static int retry(  )
      {
         string rc ;
         Console.WriteLine( "Record locked by another user." ) ;
         Console.WriteLine( "Retry? (Y or N)" ) ;
         rc = Console.ReadLine() ;
         if ( rc == "N" )
            return 0 ;
         
         return 1 ;
      }

      static int lockARecord( Data4 d, int rec )
      {
         int rc ;
         while( ((rc = d.go( rec )) == Code4.r4locked) && (retry( ) == 1) ) ;

         if ( rc == Code4.r4locked )
            return 0 ;

         return 1 ;
      }

      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4() ;
         Data4 data = new Data4( cb, "INFO" ) ;
         lockARecord( data, 3 ) ;

         // other code

         cb.initUndo( ) ;
      }
   }
}