/* e4str2.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */
#ifndef E4ERROR_OFF
#ifndef S4LANGUAGE

/* LY 2003/07/08 : added static */
static S4CONST char *bad4data = "Invalid or Unknown Error Code" ;
ERROR4DATA e4errorData[] =
{
   /* General Disk Access Errors */
   { e4close,          "Closing File" },
   { e4create,         "Creating File" },
   { e4len,            "Determining File Length" },
   { e4lenSet,         "Setting File Length" },
   { e4lock,           "Locking File" },
   { e4open,           "Opening File" },
   { e4permiss,        "Permission Error Opening File" },
   { e4access,         "Access Error Opening File" },
   { e4numFiles,       "File Handle Count Overflow Error Opening File" },
   { e4fileFind,       "File Find Error Opening File" },
   { e4instance,       "Duplicate Instance Found Error Opening File" },
   { e4read,           "Reading File" },
   { e4remove,         "Removing File" },
   { e4rename,         "Renaming File" },
   { e4seek,           "Seeking to File Position" },
   { e4unlock,         "Unlocking File" },
   { e4write,          "Writing to File" },

   /* Database Specific Errors */
   { e4data,          "File is not a Data File" },
   { e4fieldName,     "Unrecognized or Invalid Field Name" },
   { e4fieldType,     "Unrecognized Field Type" },
   { e4recordLen,     "Record Length is too Large" },
   { e4append,        "Record Append Attempt Past End of File" },
   { e4seek,          "Attempt to perform a d4seekDouble() on non-numeric tag" },

   /* Index File Specific Errors */
   { e4entry,          "Tag Entry Missing" },
   { e4index,          "Not a Correct Index File" },
   { e4tagName,        "Tag Name not Found" },
   { e4unique,         "Unique Key Error" },
   { e4tagInfo,        "Tag Information Invalid" },
   { e4candidate,      "Candidate Key Error" },

   /* Expression Evaluation Errors */
   { e4commaExpected, "Comma or Bracket Expected" },
   { e4complete,      "Expression not Complete" },
   { e4dataName,      "Data File Name not Located" },
   { e4lengthErr,     "IIF() Needs Parameters of Same Length" },
   { e4notConstant,   "SUBSTR(), STR(), and RIGHT() need Constant Parameters" },
   { e4numParms,      "Number of Parameters is Wrong" },
   { e4overflow,      "Overflow while Evaluating Expression" },
   { e4rightMissing,  "Right Bracket Missing" },
   { e4typeSub,       "Sub-expression Type is Wrong" },
   { e4unrecFunction, "Unrecognized Function" },
   { e4unrecOperator, "Unrecognized Operator" },
   { e4unrecValue,    "Unrecognized Value"} ,
   { e4unterminated,  "Unterminated String"} ,
   { e4tagExpr,       "Expression Invalid for Tag"} ,

   /* Optimization Errors */
   { e4opt,            "Optimization Error"} ,
   { e4optSuspend,     "Optimization Removal Failure"} ,
   { e4optFlush,      "Optimization File Flushing Failure"} ,

   /* Relation Errors */
   { e4lookupErr,      "Matching Slave Record Not Located"} ,
   { e4relate,         "Relation Error"} ,
   { e4relateRefer,    "Relation Referred to Does Not Exist or is Not Initialized"} ,

   /* Report Errors */
   { e4report,         "Report Error"} ,
   { e4styleCreate,   "Error Creating Style"},
   { e4styleSelect,   "Error Selecting Style"},
   { e4styleIndex,    "Error Finding Style"},
   { e4areaCreate,    "Error Creating Area"},
   { e4groupCreate,   "Error Creating Group"},
   { e4groupExpr,     "Error Setting Group Reset-Expression"},
   { e4totalCreate,   "Error Creating Total"},
   { e4objCreate,     "Error Creating Object"},
   { e4repWin,        "Error In Windows Output"},
   { e4repOut,        "Error In Report Output"},
   { e4repSave,       "Error Saving Report"},
   { e4repRet,        "Error Retrieving Report"},
   { e4repData,       "Error In Sending Report to Data File"},

   /* Critical Errors */
   { e4info,           "Unexpected Information"} ,
   { e4memory,         "Out of Memory"} ,
   { e4parm,           "Unexpected Parameter"} ,
   { e4parm_null,      "Null Input Parameter unexpected"} ,
   { e4demo,           "CodeBase Demo Limitation\nContact Sequiter Software (www.sequiter.com) for purchase information."} ,
   { e4result,         "Unexpected Result"} ,
   { e4verify,         "Structure Verification Failure"} ,
   { e4struct,         "Data Structure Corrupt or not Initialized" },

   /* Not Supported Errors */
   { e4notIndex,       "Function unsupported: library compiled with S4OFF_INDEX" },
   { e4notMemo,        "Function unsupported: library compiled with S4OFF_MEMO" },
   { e4notRename,      "Function unsupported: library compiled with S4NO_RENAME" },
   { e4notWrite,       "Function unsupported: library compiled with S4OFF_WRITE" },
   { e4notClipper,     "Function unsupported: library not compiled with S4CLIPPER" },
   { e4notLock,        "Function unsupported: library not compiled with S4LOCK_HOOK" },
/*   { e4notHook,        "Function unsupported: library not compiled with E4HOOK" }, */
   { e4notSupported,   "Function unsupported" },
   { e4version,        "Application/Library version mismatch" },

   /* MEMO errors */
   { e4memoCorrupt,    "Memo File Corrupt" },
   { e4memoCreate,     "Error Creating Memo File" },

   /* transaction errors */
   { e4transViolation, "Transaction Violation Error" },
   { e4trans,          "Transaction Error" },
   { e4rollback,       "Transaction Rollback Failure" },
   { e4commit,         "Transaction Commit Failure" },
   { e4transAppend,    "Error Appending Information to Log File" },
   { e4transStatus,    "Transaction state confliction" },  /* eg. attempt to commit when no active transaction */

   /* communications errors */
   { e4corrupt,        "Communication Information Corrupt" },
   { e4connection,     "Connection Failure" },
   { e4socket,         "Socket Failure" },
   { e4net,            "Network Failure" },
   { e4loadlib,        "Failure Loading Communication DLL" },
   { e4timeOut,        "Network Timed Out" },
   { e4message,        "Communication Message Corrupt" },
   { e4packetLen,      "Communication Packet Length Mismatch" },
   { e4packet,         "Communication Packet Corrupt" },
   { e4connect,        "system-level communication failure" },
   { e4preprocessRequired, "Preprocess not enabled on client and server configuration requires preprocessed communications" },

   /* miscellaneous errors */
   { e4max,            "CodeBase Capabilities Exceeded (system maxed out)" },
//   { e4codeBase,       "CodeBase in an Unacknowledged Error State" },
   { e4name,           "Name not Found error" },
   { e4authorize,      "Authorization Error (access denied)" },
   { e4invalidUserId,  "Invalid User ID Authorization Error (access denied)" },
   { e4invalidPassword, "Invalid Password Authorization Error (access denied)" },
   { e4invalidTcpAddress, "Invalid TCP Address access Authorization Error (access denied)" },
   { e4connectDenied,  "Server is not accepting any new connections at this time - contact System Administrator for details" },
   { e4invalidLicence, "LICENSE NOTICE: " },

   /* all server-specific error >2100, not only e4server returned to client */
   { e4server,         "Server Failure" },
   { e4config,         "Server Configuration Failure" },
/*   { e4cat,            "Catalog Failure" },  -- not used currently */

   { 0, 0 },
} ;

#endif  /* not S4LANGUAGE */

#ifdef S4LANGUAGE
#ifdef S4GERMAN

S4CONST char *bad4data = "Invalid or Unknown Error Code" ;

ERROR4DATA e4errorData[] =
{
   /* Allgemeine Fehler beim Diskzugriff  (General Disk Access Errors) */
   { e4create,         "Anlegen einer Datei" },
   { e4open,           "™ffnen einer Datei" },
   { e4read,           "Lesen einer Datei" },
   { e4seek,           "Suchen einer Position in der Datei " },
   { e4write,          "Schreiben einer Datei" },
   { e4close,          "Schlieáen einer Datei" },
   { e4remove,         "L”schen einer Datei" },
   { e4lock,           "Locken einer Datei" },
   { e4unlock,         "Freigeben einer Datei" },
   { e4len,            "Festlegen der L„nge einer Datei" },
   { e4lenSet,        "Einstellen der L„nge einer Datei" },
   { e4rename,         "Umnennen einer Datei" },

   /* Datenbank spezifische Fehler (Database Specific Errors) */
   { e4data,           "Datei is keiner DatenBank" },
   { e4recordLen,     "Datensatzl„nge zu groá" },
   { e4fieldName,     "Unbekannter Feldname" },
   { e4fieldType,     "Feldtyp" },

   /* Indexdatei spezifische Fehler  (Index File Specific Errors) */
   { e4index,          "Datei is keine Indexdatei" },
   { e4entry,          "Indexdatei is veraltet" },
   { e4unique,         "Schulsel ist schon einmal vorhanden" },
   { e4tagName,       "Name des 'Tag'"},

   /* Fehler bei der Bewertung von Ausdrcken   (Expressions Evaluation Errors) */
   { e4commaExpected, "\",\" oder \")\" erwartet" },
   { e4complete,       "Ausdruck ist nich vollst„ndig" },
   { e4dataName,      "Keine offene Datenbank" },
   { e4numParms,      "Ungltige Anzahl von Parametern im Ausdruck"},
   { e4overflow,       "šberlauf bei der Auswertung eines Ausdrucks" },
   { e4rightMissing,  "Rechte Klammer im Ausdruck fehlt" },
   { e4unrecFunction, "Unbekannte Funktion im Ausdruck" },
   { e4unrecOperator, "Unbekannter Operator im Ausdruck" },
   { e4unrecValue,    "Unbekannter Wert im Ausdruck"} ,
   { e4unterminated,   "Nicht abgeschlossene Zeichenkette im Ausdruck"} ,

   /* Optimization Errors */
   { e4opt,            "Optimization Error"} ,   /*!!!GERMAN*/
   { e4optSuspend,     "Optimization Removal Failure"} ,      /*!!!GERMAN*/
   { e4optFlush,      "Optimization File Flushing Failure"} , /*!!!GERMAN*/

   /* Relation Errors */
   { e4lookupErr,     "Matching Slave Record Not Located"} ,

   /* Kritische Fehler  (Critical Errors) */
   { e4memory,         "Kein Speicher mehr verfgbar"} ,
   { e4info,           "Unerwartete Information" },
   { e4parm,           "Unerwarteter Parameter"},
   { e4parm_null,      "Null Input Parameter unexpected"} ,
   { e4demo,           "CodeBase Demo Limitation\nContact Sequiter Software (www.sequiter.com) for purchase information."} ,
   { e4result,         "Unerwartetes Ergebnis"},
   { 0, 0 },
} ;

#endif  /* S4GERMAN  */

#ifdef S4FRENCH

S4CONST char *bad4data = "Invalid or Unknown Error Code" ;

ERROR4DATA e4errorData[] =
{
   /* General Disk Access Errors */
   { e4create,         "En cr‚ant le fichier" },
   { e4open,           "En engageant le fichier" },
   { e4read,           "En lisant le fichier" },
   { e4seek,           "En se pla‡ant dans le fichier" },
   { e4write,          "En ‚crivant dans le fichier" },
   { e4close,          "En lib‚rant le fichier" },
   { e4remove,         "En effa‡ant le fichier" },
   { e4lock,           "En bloquant le fichier" },
   { e4unlock,         "En d‚bloquant le fichier" },
   { e4len,            "En d‚terminant la longueur du fichier" },
   { e4lenSet,        "Mise … jour de la longueur du fichier" },
   { e4rename,         "D‚nomination du fichier" },

   /* Database Specific Errors */
   { e4data,           "Le fichier n'est pas une base de donn‚es:" },
   { e4recordLen,     "La fiche est trop grande" },
   { e4fieldName,     "Champ inconnu" },
   { e4fieldType,     "Type de champ inconnu" },

   /* Index File Specific Errors */
   { e4index,          "Ce n'est pas un fichier d'indice" },
   { e4entry,          "Le fichier d'indice n'est pas … jour" },
   { e4unique,         "La clef n'est pas unique" },
   { e4tagName,       "L'article d‚sign‚ par l'indice n'existe pas" },

   /* Expression Evaluation Errors */
   { e4commaExpected, "\",\" ou \")\" manquant dans l'expression" },
   { e4complete,       "Expression incomplŠte" },
   { e4dataName,      "La base r‚f‚r‚e dans l'expression n'est pas pr‚sente" },
   { e4numParms,      "Nombre ill‚gal de critŠres dans l'expression"},
   { e4overflow,       "L'expression donne un r‚sultat trop grand" },
   { e4rightMissing,  "ParenthŠse manquante dans l'expression" },
   { e4typeSub,       "Un paramŠtre est de la mauvaise sorte" },
   { e4unrecFunction, "L'expression contient une fonction inconnue" },
   { e4unrecOperator, "L'expression contient un op‚rateur inconnu" },
   { e4unrecValue,    "L'expression contient une valeur inconnue"} ,
   { e4unterminated,   "Apostrophe manquante dans l'expression"} ,

   /* Optimization Errors */
   { e4opt,            "Optimization Error"} ,
   { e4optSuspend,     "Optimization Removal Failure"} ,
   { e4optFlush,      "Optimization File Flushing Failure"} ,

   /* Relation Errors */
   { e4lookupErr,     "Matching Slave Record Not Located"} ,

   /* Critical Errors */
   { e4memory,         "Plus de m‚moire disponible" } ,
   { e4info,           "Information inexpect‚e"} ,
   { e4parm,           "ParamŠtre inexpect‚"} ,
   { e4parm_null,      "Null Input Parameter unexpected"} ,
   { e4demo,           "CodeBase Demo Limitation\nContact Sequiter Software (www.sequiter.com) for purchase information."} ,
   { e4result,         "R‚sultat inexpect‚"} ,
   { 0, 0 },
} ;

#endif  /* S4FRENCH */

#ifdef S4SCANDINAVIAN

S4CONST char *bad4data = "Invalid or Unknown Error Code" ;

ERROR4DATA e4errorData[] =
{
   /* General Disk Access Errors */
   { e4create,         "Creating File" },
   { e4open,           "Opening File" },
   { e4read,           "Reading File" },
   { e4seek,           "Seeking to File Position" },
   { e4write,          "Writing to File" },
   { e4close,          "Closing File" },
   { e4remove,         "Removing File" },
   { e4lock,           "Locking File" },
   { e4unlock,         "Unlocking File" },
   { e4len,            "Determining File Length" },
   { e4lenSet,        "Setting File Length" },
   { e4rename,         "Renaming File" },

   /* Database Specific Errors */
   { e4data,           "File is not a Data File" },
   { e4recordLen,     "Record Length is too Large" },
   { e4fieldName,     "Unrecognized or Invalid Field Name" },
   { e4fieldType,     "Unrecognized Field Type" },

   /* Index File Specific Errors */
   { e4index,          "Not a Correct Index File" },
   { e4entry,          "Tag Entry Missing" },
   { e4unique,         "Unique Key Error" },
   { e4tagName,       "Tag Name not Found" },

   /* Expression Evaluation Errors */
   { e4commaExpected, "Comma or Bracket Expected" },
   { e4complete,       "Expression not Complete" },
   { e4dataName,      "Data File Name not Located" },
   { e4lengthErr,     "IIF() Needs Parameters of Same Length" },
   { e4notConstant,   "SUBSTR(), STR(), and RIGHT() need Constant Parameters" },
   { e4numParms,      "Number of Parameters is Wrong" },
   { e4overflow,       "Overflow while Evaluating Expression" },
   { e4rightMissing,  "Right Bracket Missing" },
   { e4typeSub,       "Sub-expression Type is Wrong" },
   { e4unrecFunction, "Unrecognized Function" },
   { e4unrecOperator, "Unrecognized Operator" },
   { e4unrecValue,    "Unrecognized Value"} ,
   { e4unterminated,   "Unterminated String"} ,

   /* Optimization Errors */
   { e4opt,            "Optimization Error"} ,
   { e4optSuspend,     "Optimization Removal Failure"} ,
   { e4optFlush,      "Optimization File Flushing Failure"} ,

   /* Relation Errors */
   { e4relate,         "Relation Error"} ,
   { e4lookupErr,     "Matching Slave Record Not Located"} ,

   /* Report Errors */
   { e4report,         "Report Error"} ,

   /* Critical Errors */
   { e4memory,         "Out of Memory"} ,
   { e4info,           "Unexpected Information"} ,
   { e4parm,           "Unexpected Parameter"} ,
   { e4parm_null,      "Null Input Parameter unexpected"} ,
   { e4demo,           "CodeBase Demo Limitation\nContact Sequiter Software (www.sequiter.com) for purchase information."} ,
   { e4result,         "Unexpected Result"} ,
   { 0, 0 },
} ;
#endif  /* S4SCANDINAVIAN */
#ifdef S4SWEDISH

S4CONST char *bad4data = "Invalid or Unknown Error Code" ;

ERROR4DATA e4errorData[] =
{
   /* General Disk Access Errors */
   { e4create,         "Creating File" },
   { e4open,           "Opening File" },
   { e4read,           "Reading File" },
   { e4seek,           "Seeking to File Position" },
   { e4write,          "Writing to File" },
   { e4close,          "Closing File" },
   { e4remove,         "Removing File" },
   { e4lock,           "Locking File" },
   { e4unlock,         "Unlocking File" },
   { e4len,            "Determining File Length" },
   { e4lenSet,        "Setting File Length" },
   { e4rename,         "Renaming File" },

   /* Database Specific Errors */
   { e4data,           "File is not a Data File" },
   { e4recordLen,     "Record Length is too Large" },
   { e4fieldName,     "Unrecognized or Invalid Field Name" },
   { e4fieldType,     "Unrecognized Field Type" },

   /* Index File Specific Errors */
   { e4index,          "Not a Correct Index File" },
   { e4entry,          "Tag Entry Missing" },
   { e4unique,         "Unique Key Error" },
   { e4tagName,       "Tag Name not Found" },

   /* Expression Evaluation Errors */
   { e4commaExpected, "Comma or Bracket Expected" },
   { e4complete,       "Expression not Complete" },
   { e4dataName,      "Data File Name not Located" },
   { e4lengthErr,     "IIF() Needs Parameters of Same Length" },
   { e4notConstant,   "SUBSTR(), STR(), and RIGHT() need Constant Parameters" },
   { e4numParms,      "Number of Parameters is Wrong" },
   { e4overflow,       "Overflow while Evaluating Expression" },
   { e4rightMissing,  "Right Bracket Missing" },
   { e4typeSub,       "Sub-expression Type is Wrong" },
   { e4unrecFunction, "Unrecognized Function" },
   { e4unrecOperator, "Unrecognized Operator" },
   { e4unrecValue,    "Unrecognized Value"} ,
   { e4unterminated,   "Unterminated String"} ,

   /* Optimization Errors */
   { e4opt,            "Optimization Error"} ,
   { e4optSuspend,     "Optimization Removal Failure"} ,
   { e4optFlush,      "Optimization File Flushing Failure"} ,

   /* Relation Errors */
   { e4relate,         "Relation Error"} ,
   { e4lookupErr,     "Matching Slave Record Not Located"} ,

   /* Report Errors */
   { e4report,         "Report Error"} ,

   /* Critical Errors */
   { e4memory,         "Out of Memory"} ,
   { e4info,           "Unexpected Information"} ,
   { e4parm,           "Unexpected Parameter"} ,
   { e4parm_null,      "Null Input Parameter unexpected"} ,
   { e4demo,           "CodeBase Demo Limitation\nContact Sequiter Software (www.sequiter.com) for purchase information."} ,
   { e4result,         "Unexpected Result"} ,
   { 0, 0 },
} ;
#endif
#endif  /* S4LANGUAGE */
#else
   S4CONST char *bad4data = "Invalid or Unknown Error Code" ;
#endif  /* S4ERROR_OFF */

#ifdef S4CB51
S4CONST char *S4FUNCTION e4text( const int errCode )
#else
S4CONST char *e4text( const int errCode )
#endif
{
   #ifndef E4ERROR_OFF
      int i ;

      for ( i = 0 ; (int)e4errorData[i].errorNum != 0 ; i++ )
         if ( e4errorData[i].errorNum == errCode )
            return e4errorData[i].errorData ;
   #endif

   return bad4data ;   /* errCode not matched */
}

#ifdef P4ARGS_USED
   #pragma argsused
#endif

const char *S4FUNCTION error4lastDescription(CODE4 *c4)
{
   return c4->lastErrorDescription;
}

const char *S4FUNCTION error4text( CODE4 *c4, const long errCode2 )
{
   #ifdef E4OFF
      return bad4data ;
   #else
      #ifndef E4OFF_STRING
         ERROR4INFO_ARRAY *array ;
         long errCode ;
         unsigned int szArray ;
      #endif

      if ( errCode2 < 0L )   /* 1st level error code */
         return e4text( (int)errCode2 ) ;
      #ifdef E4OFF_STRING
         return bad4data ;
      #else
         if ( errCode2 < 10000 )
            return bad4data ;

         switch( errCode2 / 10000 )
         {
            #ifdef E4VBASIC
               case 4:
                  array = error4array4 ;
                  szArray = sizeof( error4array4 ) ;
                  break ;
            #endif
            #ifdef S4TESTING
               case 5:
                  array = error4array5 ;
                  szArray = sizeof( error4array5 ) ;
                  break ;
            #endif
            #ifdef S4CBPP
               case 6:
                  array = error4array6 ;
                  szArray = sizeof( error4array6 ) ;
                  break ;
            #endif
            #ifdef S4SERVER
               case 7:
                  array = error4array7 ;
                  szArray = sizeof( error4array7 ) ;
                  break ;
            #endif
            case 8:
               array = error4array8 ;
               szArray = sizeof( error4array8 ) ;
               break ;
            case 9:
               array = error4array9 ;
               szArray = sizeof( error4array9 ) ;
               break ;
            default:
               return bad4data ;
         }
         errCode = (errCode2 % 10000) + 1;
         if ( errCode > ( (long)szArray / (long)sizeof( array[1] ) ) )
            return bad4data ;
         return array[(int)errCode].errorString ;
      #endif
   #endif
}

long S4FUNCTION error4number2( const long errCode2 )
{
   #ifndef E4OFF_STRING
      ERROR4INFO_ARRAY *array ;
      long errCode ;
      unsigned int szArray ;

      if ( errCode2 < 10000 )
         return 0 ;
      switch( errCode2 / 10000 )
      {
         #ifdef E4VBASIC
            case 4:
               array = error4array4 ;
               szArray = sizeof( error4array4 ) ;
               break ;
         #endif
         #ifdef S4TESTING
            case 5:
               array = error4array5 ;
               szArray = sizeof( error4array5 ) ;
               break ;
         #endif
         #ifdef S4CBPP
            case 6:
               array = error4array6 ;
               szArray = sizeof( error4array6 ) ;
               break ;
         #endif
         #ifdef S4SERVER
            case 7:
               array = error4array7 ;
               szArray = sizeof( error4array7 ) ;
               break ;
         #endif
         case 8:
            array = error4array8 ;
            szArray = sizeof( error4array8 ) ;
            break ;
         case 9:
            array = error4array9 ;
            szArray = sizeof( error4array9 ) ;
            break ;
         default:
            return 0 ;
      }
      errCode = (errCode2 % 10000) + 1;
      if ( errCode > ( (long)szArray / (long)sizeof( array[1] ) ) )
         return 0 ;
      return array[ errCode ].error_number ;
   #else
      return errCode2 ;
   #endif
}

#if !defined(E4OFF_STRING)
long error4seek( long errCode2 )
{
   long ePos, pos, sPos, nPos ;
   unsigned int szArray ;
   long arrayUsed ;
   ERROR4INFO_ARRAY *array ;

   if ( errCode2 < 10000 )
      return -1 ;
   arrayUsed = errCode2 / 10000 ;
   switch( arrayUsed )
   {
      #ifdef E4VBASIC
         case 4:
            array = error4array4 ;
            szArray = sizeof( error4array4 ) ;
            break ;
      #endif
      #ifdef S4TESTING
         case 5:
            array = error4array5 ;
            szArray = sizeof( error4array5 ) ;
            break ;
      #endif
      #ifdef S4CBPP
         case 6:
            array = error4array6 ;
            szArray = sizeof( error4array6 ) ;
            break ;
      #endif
      #ifdef S4SERVER
         case 7:
            array = error4array7 ;
            szArray = sizeof( error4array7 ) ;
            break ;
      #endif
      case 8:
         array = error4array8 ;
         szArray = sizeof( error4array8 ) ;
         break ;
      case 9:
         array = error4array9 ;
         szArray = sizeof( error4array9 ) ;
         break ;
      default:
         return -1 ;
   }

   ePos = szArray / sizeof( array[1] ) ;
   pos = ePos / 2 ;
   sPos = 0 ;

   for ( ;; )
   {
      if ( pos < 0L || pos >= ePos )  /* code not found */
         return 0 ;

      if ( array[pos].error_number == errCode2 )
         return (pos - 1 + 10000 * arrayUsed );

      if ( array[pos].error_number > errCode2 )
      {
         ePos = pos ;
         nPos = pos - ( pos - sPos ) / 2 ;
         if ( nPos == pos )
            pos-- ;
         else
            pos = nPos ;
      }
      else
      {
         sPos = pos ;
         nPos = pos + ( ePos - pos ) / 2 ;
         if ( nPos == pos )
            pos++ ;
         else
            pos = nPos ;
      }
   }
}
#endif /* !defined(E4OFF_STRING) || defined(OLEDB5ERROR) */
