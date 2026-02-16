using System ;
using CodeBase ;

namespace c4ap
{
   class ex49
   {
      static void compressAll( Data4 d )
      {
         if ( d.pack( ) == 0 )
            Console.WriteLine( "Records marked for deletion are removed." ) ;

         if ( d.memoCompress( ) == 0 )
            Console.WriteLine( "Memo entries are compressed." ) ;
      }

      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4() ;
         Data4 data = new Data4( cb, "INFO" ) ;

         compressAll( data ) ;
         cb.initUndo( ) ;
      }
   }
}