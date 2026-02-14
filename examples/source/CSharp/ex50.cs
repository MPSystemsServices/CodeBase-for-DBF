using System ;
using CodeBase ;

namespace c4ap
{
   class ex50
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4() ;
         Data4 data = new Data4( cb, "INFO" ) ;

         cb.optStart( ) ;

         for ( int rc = data.top( ) ; rc != Code4.r4eof ; rc = data.skip( 1 ) )
         {
            short fieldNum ;
            for( fieldNum = 1 ; fieldNum <= data.numFields( ) ; fieldNum++ )
               Console.Write( new Field4(data, fieldNum).str( ) + "   " ) ;
            Console.WriteLine() ;
         }
         cb.initUndo( ) ;
      }
   }
}