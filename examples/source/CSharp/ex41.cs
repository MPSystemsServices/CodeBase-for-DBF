using System ;
using CodeBase ;

namespace c4ap
{
   class ex41
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 settings = new Code4() ;
         Data4 infoFile = new Data4( settings, "INFO" );

         for( short num = (short) infoFile.numFields( ) ; num != 0 ; num-- )
         {
            Field4 field = new Field4( infoFile, num ) ;
            if( num == infoFile.fieldNumber( field.name( ) ) )
               Console.WriteLine( "This is always true." ) ;
         }
         infoFile.close( ) ;
         settings.initUndo( ) ;
      }
   }
}