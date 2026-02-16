using System;
using CodeBase;

namespace c4ap
{
	class CUSTLIST
	{
		[STAThread]
		static int Main(string[] args)
		{
			Code4    codeBase = new Code4() ;
			Data4    dataFile = new Data4(codeBase, "data1.dbf" ) ;
			Field4   fNameFld = new Field4( dataFile, "F_NAME" ) ;
			Field4	 lNameFld = new Field4( dataFile, "L_NAME" ) ;
			Field4	 addressFld = new Field4( dataFile, "ADDRESS" ) ;
			Field4	 ageFld = new Field4( dataFile, "AGE" ) ;
			Field4	 birthDateFld = new Field4( dataFile, "BIRTH_DATE" ) ;
			Field4	 marriedFld = new Field4( dataFile, "MARRIED" ) ;
			Field4	 amountFld = new Field4( dataFile, "AMOUNT" ) ;
			Field4memo	 commentFld = new Field4memo( dataFile, "COMMENT" );

			codeBase.exitTest( ) ;
			
			string birthDate ;
			string purchased ;
			string name ;
			
			for( int rc = dataFile.top( ); rc == Code4.r4success; rc = dataFile.skip( ))
			{
				purchased = amountFld.str() ;
				birthDate = birthDateFld.str( ) ;
				name = fNameFld.str( ) ;
				name = name.Trim( ) ;
				name = name + " " + lNameFld.str( ) ;

				Console.WriteLine("-------------------------------\nName     : {0}\n" + 
					"Address  : {1}\nAge : {2} Married : {3}\nBirth Date: {4}\n" +
					"Comment: {5}\nPurchased this year:${6}", name.Trim(), addressFld.str( ),
					ageFld.str( ), marriedFld.str( ), birthDateFld.str( ), 
					commentFld.str( ), purchased ) ;
			}

			dataFile.close( ) ;
			codeBase.initUndo( ) ;

			return 0 ;
		}
	}
}