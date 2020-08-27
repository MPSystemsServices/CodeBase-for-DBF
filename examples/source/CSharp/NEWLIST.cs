using System;
using CodeBase;

namespace c4ap
{
	class NEWLIST
	{
		static Code4      codeBase = new Code4() ;
		static Data4      dataFile = new Data4() ;
		static Field4     fName ;
		static Field4	  lName ;
		static Field4	  address ;
		static Field4	  age ;
		static Field4	  birthDate ;
		static Field4	  married ;
		static Field4	  amount ;
		static Field4memo comment ;

		static void OpenDataFile( )
		{
			Field4info fieldInfo = new Field4info( ref codeBase ) ;

			fieldInfo.add( "F_NAME", Code4.r4str, 10, 0, 0 ) ;
			fieldInfo.add( "L_NAME", Code4.r4str, 10, 0, 0 ) ;
			fieldInfo.add( "ADDRESS", Code4.r4str, 15, 0, 0 ) ;
			fieldInfo.add( "AGE", Code4.r4num, 2, 0, 0 ) ;
			fieldInfo.add( "BIRTH_DATE", Code4.r4date, 8, 0, 0 ) ;
			fieldInfo.add( "MARRIED", Code4.r4log, 1, 0, 0 ) ;
			fieldInfo.add( "AMOUNT", Code4.r4num, 7, 2, 0 ) ;
			fieldInfo.add( "COMMENT", Code4.r4memo, 10, 0, 0 ) ;

			dataFile.open( ref codeBase, "people.dbf" ) ;

			if( dataFile.isValid( ) == 0 )
				dataFile.create( ref codeBase,"people.dbf", ref fieldInfo ) ;

			fName = new Field4( dataFile, "F_NAME") ;
			lName = new Field4( dataFile, "L_NAME") ;
			address = new Field4( dataFile, "ADDRESS") ;
			age = new Field4( dataFile, "AGE") ;
			birthDate = new Field4( dataFile, "BIRTH_DATE") ;
			married = new Field4( dataFile, "MARRIED") ;
			amount = new Field4( dataFile, "AMOUNT") ;
			comment = new Field4memo( dataFile, "COMMENT") ;
		}

		static void PrintRecords( )
		{
			string bDate ;
			string purchased ;
			string name ;
			
			for( int rc = dataFile.top( ); rc == Code4.r4success; rc = dataFile.skip( ))
			{
				purchased = amount.str() ;
				bDate = birthDate.str( ) ;
				name = fName.str( ) ;
				name = name.Trim( ) ;
				name = name + " " + lName.str( ) ;

				Console.WriteLine("-------------------------------\nName     : {0}\n" + 
					"Address  : {1}\nAge : {2} Married : {3}\nBirth Date: {4}\n" +
					"Comment: {5}\nPurchased this year:${6}", name.Trim(), address.str( ),
					age.str( ), married.str( ), birthDate.str( ), 
					comment.str( ), purchased ) ;
			}
		}

		static void AddNewRecord(string fNameStr, string lNameStr, string addressStr
			,string dateStr, int marriedValue, double amountValue, string commentStr )
		{
			dataFile.lockAll( ) ;
			dataFile.appendStart( ) ;
			dataFile.blank( ) ;

			fName.assign( fNameStr ) ;
			lName.assign( lNameStr ) ;
			address.assign( addressStr ) ;

			DateTime bDate = DateTime.ParseExact(dateStr, "yyyyMMdd", 
				System.Globalization.CultureInfo.CurrentCulture) ;
			DateTime today = DateTime.Today ;
			
			TimeSpan span = today.Subtract( bDate ) ;
			// approximate age -- ignore leap year
			int ageValue = (int) span.Days / 365 ; 
			
			age.assignInt( ageValue ) ;
			birthDate.assign( bDate.ToString( "yyyyMMdd" ) ) ;

			if( marriedValue != 0 )
				married.assign( "T" ) ;
			else
				married.assign( "F" ) ;

			amount.assignDouble( amountValue ) ;
			if( commentStr.Length > 0 )
				comment.assign( commentStr ) ;

			dataFile.append( ) ;
			dataFile.unlock( ) ;
		}

		[STAThread]
		static int Main(string[] args)
		{
			codeBase.errOpen = 0;
			codeBase.safety = 0;
			codeBase.lockEnforce = 1 ;

			OpenDataFile( ) ;

			PrintRecords( ) ;

			AddNewRecord("Sarah", "Webber", "132-43 St.", "19600223", 1, 147.99, "New Customer");

			AddNewRecord("John", "Albridge", "1232-76 Ave.", "19581012", 0, 98.99, "" ) ;

			PrintRecords( ) ;

			codeBase.closeAll( ) ;
			codeBase.initUndo( ) ;
			return 0 ;
		}
	}
}