using System;
using CodeBase;

namespace c4ap
{
	class DATAINFO
	{
		[STAThread]
		static int Main(string[] args)
		{
			if(args.Length != 1)
			{
				Console.WriteLine( " USAGE: DATAINFO <FILENAME.DBF> \n" ) ;
			}
			else
			{
				Code4    codeBase  = new Code4() ;
				Data4    dataFile = new Data4( codeBase, args.GetValue(0).ToString() ) ;
				codeBase.exitTest( ) ;

				int recCount = dataFile.recCount( ) ;
				int numFields = dataFile.numFields( ) ;
				uint recWidth = dataFile.recWidth( ) ;
				string alias = dataFile.alias ;

				Console.WriteLine( "\t\t+---------------------------------+" ) ;
				Console.WriteLine( "\t\t¦ Data File: {0, 12}         ¦", args.GetValue(0) ) ;
				Console.WriteLine( "\t\t¦ Alias    : {0, 12}         ¦", alias ) ;
				Console.WriteLine( "\t\t¦                                 ¦" ) ;
				Console.WriteLine( "\t\t¦ Number of Records: {0, 7}      ¦", recCount ) ;
				Console.WriteLine( "\t\t¦ Length of Record : {0, 7}      ¦", recWidth ) ;
				Console.WriteLine( "\t\t¦ Number of Fields : {0, 7}      ¦", numFields ) ;
				Console.WriteLine( "\t\t¦                                 ¦" ) ;
				Console.WriteLine( "\t\t¦ Field Information :             ¦\n" ) ;
				Console.WriteLine( "\t\t¦---------------------------------¦\n");
				Console.WriteLine( "\t\t¦ Name       ¦ type ¦ len  ¦ dec  ¦\n");
				Console.WriteLine( "\t\t¦------------+------+------+------¦\n");

				for(short j = 1; j <= dataFile.numFields( ); j ++)
				{
					Field4 field = new Field4( dataFile, j ) ;
					string name = field.name( ) ;
					char type = (char ) field.type( ) ;
					int len = (int) field.len( ) ;
					int dec = field.decimals( ) ;

					Console.WriteLine( "\t\t¦ {0, 10} ¦   {1}  ¦ {2,4} ¦ {3,4} ¦",
						name, type, len, dec ) ;

				}
				Console.WriteLine( "\t\t+---------------------------------+" ) ;

				dataFile.close( ) ;
				codeBase.initUndo( ) ;
			}
			return 0;
		}
	}
}