using System ;
using CodeBase ;

namespace c4ap
{
   class ex43
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4() ;
         Data4 data = new Data4( cb, "INFO" ) ;

         cb.lockAttempts = 0 ;  /* Do not wait when locking. */
         cb.readLock = 1 ;

         int rc =  data.go( 3 ) ;
         if ( rc == Code4.r4locked )
         {
            Console.WriteLine( "\nRecord 3 was locked by another user." ) ;

            cb.readLock = 0 ; // Turn automatic locking off.

            rc =  data.go( 3 ) ;
            if ( rc == Code4.r4locked )
            {
               Console.Write( "This will never happen because " ) ;
               Console.WriteLine( "'Code4::readLock' is false." ) ;
            }
         }

         data.close( ) ;
         cb.initUndo( ) ;
      }
   }
}