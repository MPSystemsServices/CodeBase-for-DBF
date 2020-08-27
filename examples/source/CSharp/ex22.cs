using System ;
using CodeBase ;

namespace c4ap
{
   class ex22
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4() ;
         Data4 data = new Data4( cb, "FILE" ) ;
         cb.exitTest( ) ; // the application will exit if FILE cannot be opened

         // ... other code ...
         cb.initUndo( ) ;
      }
   }
}