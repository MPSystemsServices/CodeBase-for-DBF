/* e4error.h  (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */
/*            (c)Copyright Sequiter Software Inc.,1990-1991. Alle Rechte vorbehalten.  */
/*                German translation: Mgr. Gertruda TKACIKOVA, Jan. 1992.  */

/*   error messages  */

#ifndef S4LANGUAGE
   #define E4_MESSAG_EXI     "EXITING APPLICATION"
   #define E4_ERROR          "Error"
   #ifdef S4UNICODE
      #define E4_ERROR_COD   L"CODEBASE ERROR"
   #else
      #define E4_ERROR_COD   "CODEBASE ERROR"
   #endif
   #define E4_MEMORY_ERR     "Memory Error"
   #define E4_ERROR_NUM      "\n\nError Number"
   #define E4_ERROR_ENT      "Press ENTER to continue:"
#else
   #ifdef S4GERMAN
      #define E4_MESSAG_EXI     "Abbruch der Anwendung"
      #define E4_ERROR          "Fehler"
      #ifdef S4UNICODE
         #define E4_ERROR_COD   L"CODEBASE FEHLER"
      #else
         #define E4_ERROR_COD   "CODEBASE FEHLER"
      #endif
      #define E4_MEMORY_ERR     "Speicherverwaltungs-Fehler"
      #define E4_ERROR_NUM      "\n\nFehler Nummer"
   #else
      #define E4_MESSAG_EXI     "EXITING APPLICATION"
      #define E4_ERROR          "Error"
      #ifdef S4UNICODE
         #define E4_ERROR_COD   L"CODEBASE ERROR"
      #else
         #define E4_ERROR_COD   "CODEBASE ERROR"
      #endif
      #define E4_MEMORY_ERR     "Memory Error"
      #define E4_ERROR_NUM      "\n\nError Number"
   #endif
   #ifdef S4FRENCH
      #define  E4A_GRA   0x85
      #define  E4A_CIR   0x83
      #define  E4A_CI2   0x86
      #define  E4A_TRE   0x84
      #define  E4A_EGU   0xA0
      #define  E4C_CED   0x87
      #define  E4E_EGU   0x82
      #define  E4E_GRA   0x8A
      #define  E4E_CIR   0x88
      #define  E4E_TRE   0x89
      #define  E4I_TRE   0x8B
      #define  E4I_CIR   0x8C
      #define  E4I_EGU   0xA1
      #define  E4I_GRA   0x8D
      #define  E4N_ACC   0xA4
      #define  E4O_CIR   0x93
      #define  E4O_TRE   0x94
      #define  E4O_GRA   0x95
      #define  E4O_EGU   0xA2
      #define  E4U_CIR   0x96
      #define  E4U_GRA   0x97
      #define  E4U_TRE   0x81
      #define  E4U_EGU   0xA3
      #define  E4Y_TRE   0x98
      #define  E4CM_CED  0x80
      #define  E4AM_TRE  0x8E
      #define  E4AM_CIR  0x8F
      #define  E4EM_EGU  0x90
      #define  E4OM_TRE  0x89
      #define  E4UM_TRE  0x9A
   #endif   /* ifdef S4FRENCH  */
#endif

/* function names - language independent */

typedef struct ERROR4DATASt
{
   int errorNum ;
   char S4PTR *errorData ;
} ERROR4DATA ;

#ifdef E4FILE_LINE
   #define error4( a, b, c ) ( code4fileNameSet( __FILE__ ), code4lineNoSet( __LINE__ ), error4default( a, b, c ) )
   #define error4describe( a, b, c, d, e, f ) ( code4fileNameSet( __FILE__ ), code4lineNoSet( __LINE__ ), error4describeDefault( a, b, c, d, e, f ) )
   #ifdef S4CB51
      #define e4( a, b, c ) ( code4fileNameSet( __FILE__ ), code4lineNoSet( __LINE__ ), error4describeExecute( a, b, 0L, c, 0, 0 ) )
   #endif
   #ifdef E4STACK
      #define error4stack( a, b, c ) ( code4fileNameSet( __FILE__ ), code4lineNoSet( __LINE__ ), error4stackDefault( a, b, c ) )
   #else
      #define error4stack( a, b, c ) ( b )
   #endif
#else
   #define error4( a, b, c ) error4default( a, b, c )
   #define error4describe( a, b, c, d, e, f ) error4describeDefault( a, b, c, d, e, f )
   #ifdef S4CB51
      #define e4( a, b, c )  error4describeExecute( a, b, 0L, c, 0, 0 )
   #endif
   #ifdef E4STACK
      #define error4stack( a, b, c ) error4stackDefault( a, b, c )
   #else
      #define error4stack( a, b, c ) ( b )
   #endif
#endif /* E4FILE_LINE */
