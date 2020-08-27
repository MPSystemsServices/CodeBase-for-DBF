using System ;
using CodeBase ;

namespace c4ap
{
   class ex53
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4() ;
         Data4 data = new Data4( cb, "INFO" ) ;

         data.optimizeWrite( Code4.OPT4ALL ) ;
         // when doing write optimization on shared files, it is necessary to
         // lock the file, preferably with Data4::lockAll( )
         data.lockAll( ) ;

         cb.optStart( ) ; // begin optimization

         int age = 20 ;
         Field4 ageField = new Field4( data, "AGE" ) ;

         for( data.top( ) ; age < 65 ; data.append( ) )
         {
            data.appendStart( 0 ) ;
            ageField.assignInt( age++ ) ;
         }

         cb.initUndo( ) ; // flushes, closes, and unlocks
      }
   }
}