using System;
using CodeBase;

namespace c4ap
{
	class SHOWDAT2
	{
		static Code4    codeBase  = new Code4() ;
		static Data4    dataFile = new Data4() ;
		static Tag4     tag = new Tag4() ;

		static void printRecords( )
		{
			for(int rc = dataFile.top( ); rc == Code4.r4success
				; rc = dataFile.skip( ))
			{
				for(short j = 1;j <= dataFile.numFields( ); j ++)
				{
					Field4memo field = new Field4memo( dataFile, j ) ;
					Console.WriteLine( field.str( ) ) ;
				}
				Console.Write( "\n" ) ;
			}
		}

		[STAThread]
		static void Main(string[] args)
		{
			if (args.Length != 1)
			{
				Console.WriteLine( "USAGE: SHOWDAT2 <FILENAME.DBF>" ) ;
			}
			else
			{
				dataFile.open( ref codeBase, args.GetValue(0).ToString() ) ;
				codeBase.exitTest( ) ;

				Console.WriteLine( "Data File {0} in Natural Order\n", args.GetValue(0) ) ;
				printRecords( ) ;

				for( tag.initFirst( dataFile ); tag.isValid( ) != 0
					; tag.initNext( ))
				{
					Console.WriteLine( "Press ENTER to continue:" ) ;
					Console.ReadLine( ) ;

					Console.WriteLine( "Data File {0} sorted by Tag {1}\n",
						args.GetValue(0).ToString(), tag.alias() ) ;

					dataFile.select( tag ) ;
					printRecords( ) ;
				}

				dataFile.close( ) ;
				codeBase.initUndo( ) ;
			}
		}
	}
}