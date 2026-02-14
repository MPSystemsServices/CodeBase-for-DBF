using System;
using CodeBase;

namespace c4ap
{
	class RELATE2
	{
		static Code4 codeBase = new Code4() ;
		static Data4 student = new Data4() ;

		static void openDataFiles( )
		{
			student.open( ref codeBase, "student" ) ;

			codeBase.exitTest( ) ;
		}

		static void printRecord( Data4 dataFile )
		{
			for( short j=1; j <= dataFile.numFields( ); j++ )
			{
				Field4memo field = new Field4memo( dataFile, j ) ;
				Console.WriteLine( field.str( ) ) ;
			}

			Console.WriteLine( "\n" ) ;
		}

		static void query(Data4 dataFile
			, string expr
			, string order )
		{
			Relate4set relation = new Relate4set( dataFile ) ;
			if( relation.isValid( ) == 0 ) codeBase.exit( ) ;

			relation.querySet( expr ) ;
			relation.sortSet( order ) ;

			for( int rc = relation.top( ); rc != Code4.r4eof; rc = relation.skip( 1 ) )
				printRecord( dataFile ) ;

			Console.WriteLine("\n") ;

			codeBase.unlock( ) ;
			relation.free( ) ;
		}

		[STAThread]
		static void Main(string[] args)
		{
			openDataFiles( ) ;

			query( student, "AGE > 30", "" ) ;

			query( student, "UPPER(L_NAME) = 'MILLER'", "L_NAME + F_NAME" ) ;

			codeBase.closeAll( ) ;
			codeBase.initUndo( ) ;
		}
	}
}