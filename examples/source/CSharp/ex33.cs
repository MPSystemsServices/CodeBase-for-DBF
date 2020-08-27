using System ;
using CodeBase ;

namespace c4ap
{
   class ex33
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4() ;
         Data4 data = new Data4( cb, "INFO" );
         Field4 field = new Field4( data, 1 ) ;

         // output the first field of every record in reverse sequential order.
         for( data.bottom() ; data.bof( ) == 0 ; data.skip( -1 ) )
            Console.WriteLine( field.str() ) ;
         data.close( ) ;
         cb.initUndo( ) ;
      }
   }
}