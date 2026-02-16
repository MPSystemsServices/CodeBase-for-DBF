using System ;
using CodeBase ;

namespace c4ap
{
   class ex25
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4() ;
         Data4 data = new Data4( cb, "INFO" ) ;

         data.lockAll( ) ;
         data.optimizeWrite( 1 ) ;
         cb.optStart( ) ;
         // .. some other code

         cb.optSuspend( ) ; // flush & free optimization memory.
         data.unlock( ) ; // let other users make modifications.

         //  ... some other code

         cb.optStart( ) ;

         // ... other code
         cb.initUndo( ) ;
      }
   }
}