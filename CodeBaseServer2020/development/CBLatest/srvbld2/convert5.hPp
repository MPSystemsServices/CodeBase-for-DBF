// convert5.hpp

#ifdef OLEDB5BUILD
   class Accessor5handler ;
   class Rowset5all ;
   class Row5bufControl ;
   class Row5bufField ;
   class Row5buf ;

   #ifndef S4SERVER
      class Fieldset5all ;
      class Convert5parm ;
      class Row5all ;
      class ByRef5 ;
      class Field5all ;
      class Accessor5all ;
   #endif /* S4SERVER */



   /* OLEDB5BUILD */
   enum Type5
   {
      /* an enumeration of all Type5 types.

         Type5's are used to represent all possible types in the raw buffer and the row buffer.
         It does not include OLE-DB types which are never directly represented in the row or
         raw buffers.
      */
      type5none = 0,

      /* dbf types */

      type5char,         /* ascii character field, not null ended - dbf format compatible - r4str */
      type5unicode,      /* unicode character filed, not null ended - CodeBase only field type */
      type5unicodeLen,   /* unicode character filed, not null ended - includes the length as part of the field CodeBase only field type */
      type5numeric,      /* ascii character field containing a numeric value  - dbf format compatible -- r4num */
      type5date,         /* ascii character field containing a date field  - dbf format compatible -- r4date */
      type5logical,      /* ascii character field length 1 containing a logical value (n,N,t,T,f,F,y,Y)  - dbf format compatible -- r4log */
      type5binary,       /* binary character field - FoxPro 3.x compatible - r4charBin, also used for generally binary information (DBTYPE_BYTES) */
      type5integer,      /* 4 byte 'long int' value field - FoxPro 3.x compatible - r4int */
      type5double,       /* 8 byte double value field - FoxPro 3.x compatible - r4double */
      type5currency,     /* 8 byte currency field (1/100th of a cent accuracy) - stored as a double long - FoxPro 3.x compatible - r4currency */
      type5dateTime,     /* 2 long values (date,time) value field - FoxPro 3.x compatible - r4dateTime */
      type5i2,           /* DBTYPE_I2 - 2 byte 'signed short int' value field */
      type5ui2,          /* DBTYPE_UI2 - 2 byte 'unsigned short int' value field */
      type5ui4,          /* DBTYPE_UI4 - 4 byte 'unsigned long int' value field */
      type5dateDouble,   /* DBTYPE_DATE - 8 byte 'double' version of the date/time */
      type5i8,           /* DBTYPE_I8 - 8 byte 'signed long long int' value field */
      type5ui8,          /* DBTYPE_UI8 - 8 byte 'unsigned long long int' value field */
      type5boolean,      /* DBTYPE_BOOL - 0 = FALSE, ~0 = TRUE - used internally by static schema tables */
      type5dbDate,       /* DBTYPE_DBDATE - 6 bytes (year)(month)(day) */
      type5dbTime,       /* DBTYPE_TIME - 6 bytes (hours)(minutes)(seconds) */
      type5dbTimeStamp,  /* DBTYPE_TIMESTAMP - 16 bytes (date)(time)(fractional seconds)*/
      type5guid,         /* DBTYPE_GUID */

      /* index types */

      type5dateMdx,      /* in Mdx date format (a double date with a special value for blank dates), used in mdx indexes */
                         // Conversions Supported: WSTR
      type5numBcd,       /* in Mdx bcd format, used in Mdx indexes */
                         // Conversions Supported: WSTR
      type5numClipper,   /* in 'clipper' 'clipped' format, used in Clipper indexes */
                         /* and in Fox ascending indexes */
                         // Conversions Supported: WSTR
      type5strFunction,  // result of STR( numeric ) function call.  Special format for index only
      type5logFox,       /* ordered logical field value, used in foxpro indexes (value of 'F' or 'T') */
                         // Conversions Supported: WSTR

      type5ascCurrency,  /* ordered currency value field, used in Fox indexes */
                         // Conversions Supported: WSTR, CY (exact match)
      type5ascDate,      /* date field in ordered double format (julian day in ascdouble format)
                         // Conversions Supported: WSTR */
      type5ascDateTime,  /* ordered date-time field, used in Fox indexes */
                         // Conversions Supported: WSTR
      type5ascDbDate,  /* ordered date field, used in Fox indexes */
      type5ascDbTime,  /* ordered time field, used in Fox indexes */
      type5ascDbTimeStamp,  /* ordered date-time field, used in Fox indexes */
                         // Conversions Supported: WSTR
      type5ascDouble,    /* ordered double value field, used in indexes */
                         // Conversions Supported: WSTR, R8 (exact match)
      type5ascInt,       /* ordered int value field, used in Fox indexes */
                         // Conversions Supported: WSTR, I4 (exact match)
      type5ascUnsignedLong,   /* ordered int value field, used in Fox indexes */
                         // Conversions Supported: WSTR, UI4 (exact match)
      type5ascI8,        /* ordered longlong value field, used in Fox indexes */
                         // Conversions Supported: WSTR, I8 (exact match)
      type5ascLog,       /* ordered logical field value, used in indexes with ascend(), (char value of '1' for true, '0' for false) */
                         // Conversions Supported: WSTR

      type5descBinary,   /* ordered descending character field, used in indexes with descend() */
                         // Conversions Supported: WSTR
      type5descChar,     /* ordered descending character field, used in indexes with descend() */
                         // Conversions Supported: WSTR
      type5descCurrency, /* ordered descending currency value field, used in Fox indexes with descend() */
                         // Conversions Supported: WSTR, CY (exact match)
      type5descDate,     /* ordered descending date field, used in indexes with descend() */
                         // Conversions Supported: WSTR
      type5descDateTime, /* ordered descending date-time field, used in Fox indexes with descend() */
                         // Conversions Supported: WSTR
      type5descDbDate,
      type5descDbTime,
      type5descDbTimeStamp,
      type5descDouble,   /* ordered descending double value field, used in indexes with descend() */
                         // Conversions Supported: WSTR, R8 (exact match)
      type5descInt,      /* ordered descending int value field, used in Fox indexes with descend() */
                         // Conversions Supported: WSTR, I4 (exact match)
      type5descUnsignedLong,   /* ordered int value field, used in Fox indexes */
                         // Conversions Supported: WSTR, UI4 (exact match)
      type5descI8,
      type5descLog,      /* ordered descending logical field value, used in indexes with descend() */
                         // Conversions Supported: WSTR
      type5descNumClipper,/* ordered descending character numeric field, used in Clipper indexes with descend() */
                         // Conversions Supported: WSTR
      type5descUnicode,  /* ordered descending unicode numeric field, used in indexes with descend() */
                         // Conversions Supported: WSTR
           type5unicodeCollated,  /* unicode collated by a given collation order... */
           type5unicodeMachine,   /* unicode collated in machine (reverse short byte) order */

      /* misc types */

      type5ISequential,  // Sequential interface in row buffer.  For BLOBs.  Not currently working.
      type5byRef = 0x1000,
      type5charByRef = type5char + type5byRef, // Corresponds to a character memo field.
      type5binaryByRef = type5binary + type5byRef, // Corresponds to a binary memo field.

      type5signalLast,   /* used internally to indicate ends of type arrays */
   } ;



   /* OLEDB5BUILD */
   typedef struct
   {
   // public:
      LPOLESTR name ;      // same as BSTR, unicode string, slightly different than a wide string (length addition at start)
      Bool5 isNullable ;
      unsigned long len ;  // max len of field
      unsigned short type ;  // range is actuall 0 - 255
      unsigned short descending ; // either 0 or r4descending
      int precision ;    // range 0-40, also (~0) is valid
      Type5 typeRaw ;
      unsigned long offsetRaw ;
      Bool5 isComplex ;      // used for index columns
      unsigned short decimalsRaw ;  // range 0-19 for CodeBase
      int scale ;            // range 0-18, also (~0) is valid
      int nullBitRawPosition ;  // range -1(means not nullable) to # of fields in table
      unsigned long rawLen ;   // for indexes, due to expr(), may differ from len if ASCEND or DESCEND (eg. numerics)
      unsigned long ordinal ;
      unsigned long order ;   // where is this column in the ordering of columns (for integrated indexes this may be different than the ordinal number)
      Bool5 lossOfInfo ;    // (for index) is this column convertable back to field data... - i.e. can we use as index rowset?
      #ifndef S4STAND_ALONE
         Bool5 isDeferred ;
      #endif
   }  COLUMN5INFO ;



//   COLUMN5INFO *i5columns( TAG4 *tag, short *nColumns, int isUnicode, Source5config *src, Bool5 hasLossOfData ) ;
   S5EXPORT void S4FUNCTION i5columnsFree( COLUMN5INFO S4PTR *, int ) ;



   /* OLEDB5BUILD */
   enum Type5len
   {
      /*
         Used as an auxilliary to field length information.
         Field lengths may be known constants (eg. a double field is 8 bytes), or it may be one
         of the following:
         - fixed non-constants - eg. (CodeBase 'char' field type) (i.e. character fields have a fixed
           length, but not all character fields have the same constant fixed length)
         - variable lengths - eg. CodeBase memo fields
         - both - eg. type5char is used internally in the row buffer to represent both CodeBase character
           fields and CodeBase memo fields.  Thus the type5char type is both a fixed length and a
           variable length field type (although when actually bound to a CodeBase field it is one or
           the other).
      */
      type5lenFixed = -1, // Length fixed at data file creation time.
      type5lenVariable = -2, // Length dependent on field contents.
      type5lenBoth = -3, // Could be either fixed at data file creation time or depending on field contents ( ie. memo)
   } ;



   #ifndef S4SERVER
      typedef void (*Convert5function)( char *to, const char *from, Convert5parm * ) ;
      typedef void (*Convert5lenToCalculate)( Convert5parm * ) ;

      int isByRef5( DBTYPE oleTo, Type5 rowTo ) ;


      /* not S4SERVER, OLEDB5BUILD */
      class Type5both
      {
         // A data type can be a CodeBase internal type only, a OLE DB type or both at the same time.
      public:
         DBTYPE bestMatch( Type5 typ ) ; // Find the best match OLE DB type which corresponds to the internal type with no loss of data.
         DBTYPE ole ; // This is with no DBTYPE_BYREF .. (ie. a pure oledb data type)
         Type5 cb ;
      } ;



      extern Type5both type5bestMatch[] ;  // array of best matches lookup



      /* not S4SERVER, OLEDB5BUILD */
      class Type5info
      {
         /* information about the type

            a type is EITHER an ole type (ole) or a CodeBase type (cb).  It is never both types
            at once.
         */
      public:
         Type5info *lookup( DBTYPE ole, Type5 cb ) ; // Assumes the object is the first in an array.

         // one of the following will be set.  If cb is set, ole is set to 0; if ole is set, cb will
         // be set to type5none
         DBTYPE ole ;   // This is with no DBTYPE_BYREF .. (ie. a pure oledb data type)
         Type5 cb ;     // The corresponding Type5 for the type

         long len ;      // An actual positive length or one of Type5len (which are negative)
         unsigned short lenNulls ; // The number of nulls at the end of the ole type not counted in the standard length.
      } ;



      extern Type5info type5info[] ;



      /* not S4SERVER, OLEDB5BUILD */
      enum Convert5flag
      {
         /*
            The main purpose of this class is to allow for optimizations in the conversion module.
            Basically, for a given conversion there are several length possibilities:
            1. a fixed length of a constant size (eg. a double is always 8 bytes) - failure to have
               the full size of 8 bytes in the consumer buffer is a conversion error (not a truncation
               error).
            2, a variable length - eg. wide strings, memo fields, etc. - flag5variableLength
            3. a fixed but not type-constant length.  Eg. in CodeBase our numeric field is a fixed
               length always, but varies in size depending on the length and num dec for the given
               field.

            Basically, the idea is that if the lengths are fixed, they don't need to be calculated and
            the conversion is much quicker.
         */
         flag5allOK = 0,
         flag5fixedLen, // Both the 'to' and the 'from' are fixed length
         flag5variableLength,
         flag5arrayEnd,
      } ;



      /* not S4SERVER, OLEDB5BUILD */
      class Convert5
      {
         /*
            A basic conversion.  Consists of:
               a 'from' type
               a 'to' type
               a function to perform the conversion
               a function to calculate the length (after conversion)
               a flag to indicate the conversion type (fixed len or variable len)
         */
      public:
         Convert5 *lookup( DBTYPE toOLE, Type5 toCB, DBTYPE fromOLE, Type5 fromCB, Convert5flag flagParm = flag5allOK ) ;
         Type5both to ;
         Type5both from ;
         Convert5function function ;
         Convert5lenToCalculate functionLenToCalculate ;  // Corresponds to conversion or other action that needs to be done.
         Convert5flag flag ;
      } ;



      /* not S4SERVER, OLEDB5BUILD */
      class5 Convert5parm
      {
         /* represents a paramater which is passed to a conversion function.

            contains all the information required to perform a conversion.
         */
      public:
         Convert5parm()
         {
            lenTo = lenFrom = lenMinimum = 0L ;
            lenToDecimals = lenToNulls = lenFromNulls = isByRefTo = isByRefFrom = codePage = 0 ;
            lenFillWithNulls = 0 ;
            collateName = collate4none ;
            //lenExtra = 0L ;
         }

         // All lengths are in bytes.

         LONG  lenTo ;    // input length in bytes of the 'to' buffer for the conversion excluding extra null bytes

         LONG  lenToOut ; // output length of the 'to' buffer excluding extra null bytes.
                          // The default value is -1.  If the conversion function
                          // sets this value it will be >= 0 and will be used to set
                          // the consumer buffer value at DBBINDINGS.obLength.
                          // -1 is used to indicate that lenToOut is the same as lenTo (i.e. the
                          // the length is the same as input by the buffer).

         LONG  lenFrom ;  // input length of the 'from' buffer excluding null bytes

         unsigned char lenFromDecimals ;   // input length of # decimals in the 'from' buffer for numeric types, range 0-19 for CodeBase
         unsigned char lenToDecimals ;     // input length of # decimals to convert to fro numeric types, range 0-19 for CodeBase

         unsigned char lenToNulls ;   // number of extra bytes used to indicate null in the 'out' buffer - range 0,1,2
         unsigned char lenFromNulls ; // number of extra bytes used to indicate null in the 'to' buffer - range 0,1,2

         unsigned char lenFillWithNulls ; // true(1)/false(0) flag as to whether or not should fill extra space with nulls on conversion

         Bool5 isByRefTo ;    // is the 'to' buffer by-ref?
         Bool5 isByRefFrom ;  // is the 'from' buffer by-ref?

         UINT  codePage ; // Code Pages are used as paramaters to conversion strings to wide-strings.
                          // (i.e.) what output code page do you want the output string in?

              Collate4name collateName ;  // used for index key conversions using various collations.

         // lenMinimum is used to optimize the conversion functions.  Basically, if both the to and the
         // from are fixed size buffers we only need to copy the minimum of the 2 buffers.
         LONG  lenMinimum ; // The minimum of lenTo and lenFrom
         //   LONG  lenExtra ; // Extra length in row buffer over that of the accessor buffer.  Only relevant for setting.

         Bool5 doTruncate ;  // Is it valid to truncate the converted result and throw a truncation error or is
                           // truncation a conversion error (disallowed)

         inline LONG lenToAlloc() { assert5( lenTo >= 0 ) ; return( lenTo + lenToNulls ) ; } // Allocation length on the 'to' side if allocation is necessary.
      } ;



      /* not S4SERVER, OLEDB5BUILD */
      class5 Convert5step : public Convert5parm
      {
         /*
            To use do the following:
               Convert5step step ;
               step.init( .. ) ; Use the byRef as appropriate as part of the type values passed in.
                                 An internal error is thrown if and only if a conversion function cannot
                                 be found.
                                 If this is a legitimate possibility, just catch the 'err5internal'.

               If a byRef type value is passed in this means that the corresponding length has to be
               filled in explicitly after it is known if it is a variable length type.
               step.fromLen = explictValue ;

               A byRef 'to' value should be calculated as follows once the 'fromLen' is present:
               step.functionLenToCalculate( &step ) ;

               When the actual conversion function is called use a pointer to the actual data buffers as parameters
               without any kind of redirection.  If the 'to' length value was 'byRef', the amount of memory
               'to' points to should be at least 'step.lenToAlloc' which is set by 'functionLenToCalculate'.
               step.function( to, from, &step ) ;

               Nulls are NOT handled by this module.
               If there is an attempt to convert to an Interface, an error is thrown with an integer parameter
               to indicate the interface requested.

               Note: You can access step.isByRefTo or step.isByRefFrom which are set from the init function.
      */
      public:
         Convert5step()
         {
            offsetTo = offsetFrom = 0 ;
            function = 0 ;
            functionLenToCalculate = 0 ;
            isVariableLengthBound = 0 ;
            isReadOnly = 0 ;
            doTruncate = 1 ;
            field = 0 ;
         }
         void init( Row5bufControl *rowset, DBTYPE oleTo, Type5 rowTo, long lenTo, short lenToDec, DBTYPE oleFrom, Type5 rowFrom, long lenFrom ) ; //, ULONG nullRowBitPosition ) ;
         Convert5function function ;  // Corresponds to conversion or other action that needs to be done.
         Convert5lenToCalculate functionLenToCalculate ;  // Corresponds to conversion or other action that needs to be done.

      //   ULONG nullBitRowPosition ; // ULONG(-1) if there cannot be a null.
         Bool5 isVariableLengthBound ; // Is the From value it a variable length type with a bound length?
         Bool5 isReadOnly ; // Applicable for DataSet (Consumer ->Row Buffer)
         Row5bufField *field ;
         unsigned int offsetTo ; // Byte offset in Row5 buffer
         unsigned int offsetFrom ; // Byte offset in consumer buffer
      private:
         void initConversionLengths( Type5info *typeToInfo, Type5info *typeFromInfo, Convert5flag *flag, short decLen, long convertFromLength, long convertToLength ) ;
      } ;

      // Given a list of OLE DB types in the row buffer for a given field ordinal, & given the CodeBase type,
      // which is the primary type(s) to convert into?  This would be a CodeBase type and possibly also
      // a corresponding OLE DB type.
      // We can use convert5 to answer this question.
      extern Convert5 convert5[] ;



      /* not S4SERVER, OLEDB5BUILD */
      class5 Convert5group
      {
      public:
         Convert5group()
         {
            nSteps = 0 ;
            steps = 0 ;
            nStepsByRef = 0 ;
            stepsByRef = 0 ;
            isRecNoCount = 0 ;
         }
         ~Convert5group() ;

         void initDeferredToZero( char *pRowBuffer ) ;
         void rawToRow( char *pRawBuf, char *pRowBuffer, ByRef5 *pByRefInput ) ;
         inline void rawToRowRecNo( char *pRowBuffer, ULONG recNo ) { if ( isRecNoCount > 0 ) *(ULONG *)pRowBuffer = recNo ; }
         void rawToRowInit( Accessor5handler *accessorHandler, int isDeferred, ULONG *offsetRowParm, int isIndex ) ;
         void rowToRaw( char *pRowBuffer, char *pRawBuf, ByRef5 *pByRefResult ) ;  //pByRefResult[i].ptr memory is allocated and must be freed by caller.
         void rowToRawInit( Accessor5handler *accessorHandler ) ;
         void rowToRawInitStep( Accessor5handler *accessorHandler, Convert5step *step, Field5all *field ) ;

         unsigned short nSteps ;                 // The # of non-by-ref conversion steps in the group
         Convert5step *steps ;        // the array of non-by-ref conversion steps

         unsigned short nStepsByRef ;            // the # of by-ref conversion setps in the group
         Convert5step *stepsByRef ;   // the array of by-ref conversion steps

         Bool5 isRecNoCount ;
      private:
         void rawToRowInitField( Accessor5handler *accessorHandler, Fieldset5all *fieldset, int fieldNumber, int isDeferred, ULONG *offsetRowParm, int isIndex ) ;
         void rawToRowInitStep( Accessor5handler *accessorHandler, Field5all *field, Convert5step **currentStep, Convert5step **currentStepByRef, ULONG *offsetRowParm, int isIndex, int isDeferred ) ;
      } ;



      /* not S4SERVER, OLEDB5BUILD */
      class5 Convert5consumer : public Convert5group
      {
      public:
         void init( Accessor5all *accessorParm, Row5bufControl *bufControlParm,
              int isToConsumerParm ) ; // True if 'Buffer' -> 'Consumer' conversion
         ULONG consumerToBuffer( char *buf, char *pConsumer, ULONG nFields ) ;
         HRESULT bufferToConsumer( char *buf, char *pConsumer, ULONG nFields ) ;

         Accessor5all *accessor ;
         Row5bufControl *bufControl ;
      private:
         void verifyInputStatus( DBSTATUS consumerBufferStatus, Convert5step *step, char *pBuffer, int hasLength, DBTYPE bindType ) ;
         void consumerToBufferConvertValue( Convert5step *step, char *consumerValueFrom, char *result, int lengthBound, ULONG *lengthPtr, ULONG maxLength, DBTYPE bindingType ) ;
         void verifyNoValueOk( Convert5step *step, DBSTATUS *consumerBufferStatus, int lengthBound, ULONG *lengthPtr, int bindingHasStatus ) ;
         void setConsumerFieldToNull( const DBBINDING *bind, Convert5step *step, char *pConsumer ) ;
         void convertFieldProviderOwnedByRef( const DBBINDING *bind, Convert5step *step, char *pRowBuffer, char *pConsumer ) ;
         void convertFieldClientOwnedByRef( const DBBINDING *bind, Convert5step *step, char *pRowBuffer, char *pConsumer ) ;
         void convertFieldInBuffer( const DBBINDING *bind, Convert5step *step, char *pRowBuffer, char *pConsumer, char getValue ) ;
         void convertField( const DBBINDING *bind, Convert5step *step, char *pRowBuffer, char *pConsumer, char getValue, ULONG *numErrors ) ;
      //   void setLengthForField( const DBBINDING *bind, Convert5step *step, char *pConsumer ) ;
      } ;



      /* Database */
      void convert5BOOL_logical( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5boolean_Unicode( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5BYTES_char( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5BYTES_Unicode( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5char_BYTES( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5char_UnicodeFixed( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5char_Unicode( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5copy( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5copyNoTruncate( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5currency_Unicode( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5currency_CY ( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5CY_currency ( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5date_char( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5date_DBDATE( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5date_Unicode( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5dateTime_TIMESTAMP( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5dateTime_Unicode( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5dbDate_Unicode( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5DBDATE_date( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5dbTime_Unicode( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5dbTimeStamp_Unicode( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5double_Unicode( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5guid_WSTR( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5integer_Unicode( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5ISequentialStream( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5i2_Unicode( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      #ifdef MSQL7COMPATABILITY
         void convert5I4_i2( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
         void convert5I4_ui2( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
         void convert5I4_ui4( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
         void convert5I4_ui8( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
         void convert5ui4_I4( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
         void convert5ui8_I4( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
         void convert5ui2_I4( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
         void convert5i2_I4( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      #endif
      void convert5i8_Unicode( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5I2_ui2( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5logical_BOOL( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5logical_char( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5logical_STR( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5logical_WSTR( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5numeric_char( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5numeric_numeric( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5numeric_NUMERIC( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5numeric_R8( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5numeric_Unicode( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5NUMERIC_numeric( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5R8_numeric( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5STR_char( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5char_charFixed( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5STR_date( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5STR_logical( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5STR_numeric( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5STR_Unicode( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5STR_UnicodeLen( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5TIMESTAMP_dateTime( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5ui2_Unicode( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5UI2_i2( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5ui4_Unicode( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5Unicode_char( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5unicode_UnicodeFixed( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5unicodeLen_UnicodeLen( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5unicodeLen_WSTR( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5unicodeLen_STR( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_boolean( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_BYTES( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_char( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_currency ( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_date( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_dateTime( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_dbDate( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_dbTime( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_dbTimeStamp( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_double( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_Generic( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_Guid( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_integer( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_i2( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_i8( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_logical( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_numeric( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_ui2( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_ui4( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_Unicode( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_UnicodeLen( char *toPtr, const char *fromPtr, Convert5parm *c ) ;


      /* Ascending Indexes */
      void convert5ascCurrency_CY( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5ascCurrency_Unicode( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5ascDate_Unicode( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5ascDate_DBDATE( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5ascDouble_DBDATE( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5ascDbDate_DBDATE( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5DBTIMESTAMP_descDbTimeStamp( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5DBTIMESTAMP_ascDbTimeStamp( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_ascDbTimeStamp( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_descDbTimeStamp( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5ascDbTimeStamp_WSTR( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5descDbTimeStamp_WSTR( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5ascDbTime_WSTR( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_ascDbTime( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5descDbTime_WSTR( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_descDbTime( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5DBTIME_ascDbTime( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5DBTIME_descDbTime( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5DBDATE_ascDbDate( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_ascDbDate( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5ascDbDate_WSTR( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_descDbDate( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5descDbDate_WSTR( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5DBDATE_descDbDate( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5descDbDate_DBDATE( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5descBinary_BYTES( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_descBinary( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5descBinary_WSTR( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5descChar_STR( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5ascDbTime_DBTIME( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5descDbTime_DBTIME( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5ascDbTimeStamp_DBTIMESTAMP( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5descDbTimeStamp_DBTIMESTAMP( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5ascDateTime_Unicode( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5ascDateTime_TIMESTAMP( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5ascDouble_NUMERIC( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5ascDouble_R8( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5ascDouble_Unicode( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5ascInt_integer( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5ascInt_i2( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5ascUnsignedLong_ui4( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5ui4_ascUnsignedLong( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_ascUnsignedLong( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5ascUnsignedLong_WSTR( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_descUnsignedLong( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5descUnsignedLong_WSTR( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5ui4_descUnsignedLong( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5ascUnsignedLong_ui2( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5ascI8_i8( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5ascI8_WSTR( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_ascI8( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5descI8_WSTR( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_descI8( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5i8_ascI8( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5i8_descI8( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5descI8_i8( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5ascInt_Unicode( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5ascLog_BOOL( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5ascLog_char( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5ascLog_Unicode( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5descUnsignedLong_ui4( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5descUnsignedLong_ui2( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5BOOL_ascLog( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5BOOL_logFox( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5CY_ascCurrency( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5dateMdx_DBDATE( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5dateMdx_Unicode( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5integer_ascInt( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5logFox_char( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5logFox_Unicode( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5numBcd_Unicode( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5numBcd_Numeric( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5strFunction_Unicode( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_strFunction( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5strFunction_NUMERIC( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5numClipper_Unicode( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5logFox_BOOL( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5R8_ascDouble( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5STR_ascLog( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5STR_logFox( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_ascCurrency( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_ascDate( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_ascDateTime( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_ascDouble( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_ascInt( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_ascLog( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_dateMdx( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_logFox( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_numBcd( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_numClipper( char *toPtr, const char *fromPtr, Convert5parm *c ) ;


      /* Descending Indexes */
      void convert5BOOL_descLog( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5CY_descCurrency( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5descChar_Unicode( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5descCurrency_CY( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5descCurrency_Unicode( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5descDate_DBDATE( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5descDate_Unicode( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5descDateTime_Unicode( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5descDateTime_DBTIMESTAMP( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5descDouble_R8( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5descDouble_Unicode( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5descInt_integer( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5descInt_i2( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5descInt_Unicode( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5descLog_BOOL( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5descLog_char( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5descLog_Unicode( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5numClipper_NUMERIC( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5descNumClipper_NUMERIC( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5descNumClipper_Unicode( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      // AS 08/19/99 unicodeMachine_Unicode is same conversion as unicode_UnicodeMachine (i.e. reverse bytes)
      // void convert5unicodeMachine_Unicode( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5unicodeCollated_Unicode( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5unicode_UnicodeMachine( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5descUnicode_Unicode( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5integer_descInt( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5R8_descDouble( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5STR_descLog( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_descChar( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_descCurrency( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_descDate( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_descDateTime( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_descDouble( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_descInt( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_descLog( char *toPtr, const char *fromPtr, Convert5parm *c ) ;
      void convert5WSTR_descNumClipper( char *toPtr, const char *fromPtr, Convert5parm *c ) ;

      void convert5dbDate_UnicodeVerifyInput( WSTR5 *dateWideString, int numChars ) ;
   #endif /* S4SERVER */
#endif /* OLEDB5BUILD */
