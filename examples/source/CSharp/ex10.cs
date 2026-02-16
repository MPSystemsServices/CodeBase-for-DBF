using System ;
using CodeBase ;

namespace c4ap
{
   class ex10
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4() ;
         Data4 data = new Data4( cb, "INFO" ) ;

         cb.readLock = 1 ;
         cb.lockAttempts = 3 ;

         if ( data.top( ) == Code4.r4locked ) 
         {
            Console.WriteLine( "Top record locked by another user" ) ;
            Console.WriteLine( "Lock attempted " + cb.lockAttempts + " time(s)" );
         }
         cb.initUndo( ) ;
      }
   }
}