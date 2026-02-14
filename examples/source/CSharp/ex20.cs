using System ;
using CodeBase ;

namespace c4ap
{
   class ex20
   {
      static int display( ref Code4 cb, string p )
      {
         if( p.Length == 0 )
            return cb.error( Code4.e4parm, 0, "Null display string", "", "" ) ;

         Console.WriteLine( p ) ;
         return 0 ;
      }

      [STAThread]
      static void Main(string[] args)
      {
         Code4 code = new Code4() ;
         string someString = "Hello There" ;
         display( ref code, someString );
         display( ref code, "" ) ;
         code.initUndo( ) ;
      }
   }
}