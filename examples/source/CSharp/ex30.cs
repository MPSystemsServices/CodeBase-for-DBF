using System ;
using CodeBase ;

namespace c4ap
{
   class ex30
   {
      static Code4 cb = new Code4() ; // Code4 may be constructed globally.
      static Data4 data = new Data4( cb, "INFO" ) ;

      [STAThread]
      static void Main(string[] args)
      {
         data.lockAll( ) ;
         cb.optStart( ) ;
         data.appendBlank( ) ;

         // Append a copy of record two.  (Assume record two exists.)
         data.go( 2 ) ;
         data.appendStart( ) ;  // use_memo_entries defaults to zero
         data.append( ) ;

         // Append a copy of record 2 including existing memo entries.
         data.go( 2 ) ;
         data.appendStart( 1 ) ; // a true parameter means use memo entries
         data.append( ) ;

         // Set the record buffer to blank, change a field's value, and append
         // the resulting record.
         data.appendStart( ) ;
         data.blank( ) ;
         Field4 field = new Field4( data, "NAME" ) ;
         field.assign( "New field value" ) ;
         data.append( ) ;

         // close all open files and release any allocated memory
         cb.initUndo( ) ;
      }
   }
}