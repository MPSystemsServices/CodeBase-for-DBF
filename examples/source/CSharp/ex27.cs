using System ;
using CodeBase ;

namespace c4ap
{
   class ex27
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 settings = new Code4() ;

         Data4 info = new Data4( settings, "DATA2.DBF" ) ;

         settings.optStart() ;

         Field4 field = new Field4( info, "NAME" ) ;
         info.lockAll( ) ; //the record must be locked before a field can be changed
         for( int iRec = 1 ; iRec <= info.recCount() ; iRec++ )
         {
            info.go( iRec ) ;
            field.assign( "New Data" ) ;
         }

         info.close( ) ;
         settings.initUndo( ) ;
         settings.exit( ) ;
      }
   }
}