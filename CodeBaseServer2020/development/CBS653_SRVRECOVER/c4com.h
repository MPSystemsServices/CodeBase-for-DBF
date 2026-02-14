/* c4com.h   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#ifdef __cplusplus
   extern "C" {
#endif

#ifndef S4SERVER
   S4EXPORT long S4FUNCTION code4timeout( CODE4 S4PTR * ) ;
#endif

#if defined( S4DLL_BUILD ) || defined( S4LIB_BUILD ) || defined( S4SERVER )
   #define code4timeoutVal( c4 ) ( (c4)->timeout )
#else
   #define code4timeoutVal( c4 ) ( code4timeout( c4 ) )
#endif

S4EXPORT void S4FUNCTION code4timeoutSet( CODE4 S4PTR *, long ) ;

#ifdef __cplusplus
   }
#endif

#ifndef S4OFF_COMMUNICATIONS

#define htons5( a ) (htons(a))
#define htonl5( a ) (htonl(a))
#define ntohs5( a ) (ntohs(a))
#define ntohl5( a ) (ntohl(a))

/* for now, we do support non-intel byte ordering... */
/* #define htons5( a ) (a) */
/* #define htonl5( a ) (a) */
/* #define ntohs5( a ) (a) */
/* #define ntohl5( a ) (a) */

#define COM4DEFAULT_MESSAGE_LEN 1024

/* structures for d4open/d4create communication information between client/server */
typedef struct
{
   S4LONG serverId ;   /* for when referring to the data on the server */
   unsigned short headerLen ;
   unsigned short recWidth ;
   unsigned short infoLen ;
   unsigned short numTags ;
   short readOnly ;
   char version ;
   char blank ;
   /* char *info */
   /* void *tagInfo (for each tag...) */
} CONNECTION4OPEN_INFO_OUT ;



typedef struct
{
   unsigned short numTags ;
   /* void *tagInfo (for each tag...) */
} CONNECTION4INDEX_INFO_OUT ;



typedef struct
{
   char name[even4up( LEN4PATH + 1 )] ;
   short errDefaultUnique ;
   short accessMode ;
   short openForCreate ;
   short fileFlush ;           /* AS 07/21/99 - added for win 95/98 to avoid endless laze writes */
   unsigned short readOnly ;
   unsigned short singleOpen ;
   unsigned short log ;
   unsigned short compatibility ;            /* what type of data file to create (fox) */
} CONNECTION4OPEN_INFO_IN ;



typedef struct
{
   char password[even4up( LEN4DATA_ALIAS + 1 )] ;
} CONNECTION4PASSWORD_INFO_IN ;



typedef struct
{
   short accessMode ;
   BOOL4 readOnly ;
   short nameLen ;
   short errDefaultUnique ;
   short openForCreate ;
   short fileFlush ;           /* AS 07/21/99 - added for win 95/98 to avoid endless laze writes */
   BOOL4 safety ;    /* for catalog only */
   BOOL4 createTemp ;      /* temporary create only ( name null ) */
   /*   char accessName[nameLen] ;*/
} CONNECTION4OPEN_INDEX_INFO_IN ;



typedef struct
{
   unsigned short numTags ;
   /* void *tagInfo (for each tag...) */
} CONNECTION4OPEN_INDEX_INFO_OUT ;



typedef struct
{
   short accessMode ;
   BOOL4 readOnly ;
   short nameLen ;
   short errDefaultUnique ;
   short openForCreate ;
   short fileFlush ;           /* AS 07/21/99 - added for win 95/98 to avoid endless laze writes */
   BOOL4 safety ;    /* for catalog only */
   BOOL4 hasIndex ;
   char tagName[even4up( LEN4PATH + 1 )] ;
   char indexName[even4up( LEN4PATH + 1 )] ;  /* if t4open is deriving from i4open */
   /*   char indexName[nameLen] ;*/
} CONNECTION4OPEN_TAG_INFO_IN ;



typedef struct
{
   BOOL4 autoOpened ;   /* flag for whether or not server had to do the open */
   /*    void *tagInfo (for each tag...) */
} CONNECTION4OPEN_TAG_INFO_OUT ;



typedef union
{
   char S4PTR *ptr ;
   unsigned short offset ;
} CONNECTION4PTR_UNION ;



typedef struct
{
   CONNECTION4PTR_UNION name ;
   short type ;
   unsigned short len ;
   unsigned short dec ;
   unsigned short nulls ;
} CONNECTION4FIELD_INFO ;



typedef struct
{
   CONNECTION4PTR_UNION name ;
   CONNECTION4PTR_UNION expression ;
   CONNECTION4PTR_UNION filter ;
   short unique ;
   unsigned short descending ;
   /* void *data */
} CONNECTION4TAG_INFO ;


typedef struct
{
   CONNECTION4PTR_UNION name ;
   CONNECTION4PTR_UNION expression ;
   CONNECTION4PTR_UNION filter ;
   short unique ;
   unsigned short descending ;
   unsigned short keyLen ;
   char blank[2];          /* pad to 4 byte total */
   /* void *data */
} CONNECTION4TAG_INFO_FOR_I4INFO ;



typedef struct
{
   char     name[even4up( LEN4PATH + 1 )] ;
   unsigned short numFields ;
   unsigned short numTags ;
   unsigned short fieldInfoLen ;
   BOOL4 createTemp ;      /* temporary create only ( name null ) */
   short fileFlush ;       /* AS 07/21/99 - added for win 95/98 to avoid endless laze writes */
   BOOL4 safety ;
   BOOL4 readOnly ;        /* for catalog purposes */
   BOOL4 oledbSchemaCreate ;   /* used for testing only */
   unsigned short log ;
   short compatibility ;            /* what type of data file to create */
   short collatingSequence ;
   short codePage ;
   short collateName ;
   short collateNameUnicode ;
   double autoIncrementStart ;
   /*   unsigned short tagInfoLen ;*/
   /*   start of info... */
} CONNECTION4CREATE_INFO_IN ;



typedef struct
{
   short numFields ;
} CONNECTION4FIELDS_ADD_INFO_IN ;



typedef struct
{
   short catalogAdd ;
   short catalogStatus ;
} CONNECTION4CATALOG_SET_INFO_IN ;



typedef struct
{
   BOOL4 isProduction ;
   unsigned short numTags ;
//   BOOL4 createTemp ;      /* temporary create only ( name null ) */
   BOOL4 safety ;
   short fileFlush ;       /* AS 07/21/99 - added for win 95/98 to avoid endless laze writes */
   BOOL4 readOnly ;        /* for catalog purposes */
   short foxCreateIndexBlockSize ;   /* 0 if use fox defaults, else set to actual size... */
   short collatingSequence ;
   short collateName ;
   short collateNameUnicode ;
   char indexFileName[even4up( LEN4PATH + 1 )] ;
   /* start of info... */
} CONNECTION4INDEX_CREATE_INFO_IN ;



typedef struct
{
   unsigned short numTags ;
   char indexFileName[even4up( LEN4PATH + 1 )] ;
   /* start of info... */
} CONNECTION4TAG_ADD_INFO_IN ;



typedef struct
{
   char tagName[quad4oddUp( LEN4TAG_ALIAS + 1 )] ;
   /* start of info... */
} CONNECTION4TAG_REMOVE_INFO_IN ;



typedef struct
{
   BOOL4 lockedDatafile ;
} CONNECTION4TAG_ADD_INFO_OUT ;



typedef struct
{
   BOOL4 lockedDatafile ;
} CONNECTION4INDEX_CREATE_INFO_OUT ;



typedef struct
{
   BOOL4 lockedDatafile ;
} CONNECTION4REINDEX_INFO_OUT ;



typedef struct
{
   BOOL4 lockedDatafile ;
} CONNECTION4CHECK_INFO_OUT ;



typedef struct
{
   short lockedDatafile ;
} CONNECTION4PACK_INFO_OUT ;



typedef struct
{
   BOOL4 lockedDatafile ;
} CONNECTION4ZAP_INFO_OUT ;



typedef struct
{
   S4LONG startRecno ;
   BOOL4 usesTag ;
   char tagName[quad4oddUp( LEN4TAG_ALIAS + 1 )] ;
} CONNECTION4DATA_POS_IN ;



typedef struct
{
   double position ;
} CONNECTION4DATA_POS_OUT ;



typedef struct
{
   double position ;
   BOOL4 usesTag ;
   char tagName[quad4oddUp( LEN4TAG_ALIAS + 1 )] ;
} CONNECTION4DATA_POS_SET_IN ;



typedef struct
{
   S4LONG recNo ;
} CONNECTION4DATA_POS_SET_OUT ;



typedef struct
{
   /* structures for d4seek communication information between client/server */
   S4LONG startPos ;  /* for seek next */
   unsigned short keyLen ;
   BOOL4 fromCurrentPos ;
   char tagName[quad4up( LEN4TAG_ALIAS + 1 )] ;
   /* char *key */
} CONNECTION4SEEK_INFO_IN ;



typedef struct
{
   S4LONG recno ;
   char tagName[quad4up( LEN4TAG_ALIAS + 1 )] ;
} CONNECTION4TAG_SYNCH_INFO_IN ;



typedef struct
{
   S4LONG recNo ;
} CONNECTION4TAG_SYNCH_INFO_OUT ;



typedef struct
{
   /* structures for d4skip communication information between client/server */
   S4LONG startPos ;
   S4LONG numSkip ;
   BOOL4 usesTag ;
   char tagName[quad4oddUp( LEN4TAG_ALIAS + 1 )] ;
} CONNECTION4SKIP_INFO_IN ;



#ifndef S4SINGLE
   typedef struct
   {
      short numLocks ;
      /* locks */
   } CONNECTION4LOCK_GROUP_INFO_IN ;
#endif



typedef struct
{
   /* structures for d4go communication information between client/server */
   S4LONG recNo ;
} CONNECTION4GO_INFO_IN ;



typedef struct
{
   S4LONG recNo ;
   short skipRc ;    /* used by d4skip to distinguish return codes */
   BOOL4 recordLocked ;
   /*   char *record ;  */
} CONNECTION4GO_INFO_OUT ;



typedef struct
{
   BOOL4 usesTag ;
   char tagName[even4up( LEN4TAG_ALIAS + 1 )] ;
} CONNECTION4TOP_INFO_IN ;



typedef struct
{
   S4LONG recNo ;
   BOOL4 recordLocked ;
   BOOL4 eofFlag ;
   BOOL4 bofFlag ;
   char blank[2] ;
   /*   char *record ; */
} CONNECTION4TOP_INFO_OUT ;



typedef struct
{
   BOOL4 usesTag ;
   char tagName[even4up( LEN4TAG_ALIAS + 1 )] ;
} CONNECTION4BOTTOM_INFO_IN ;



typedef struct
{
   char calcName[even4up( E4MAX_CALC_NAME ) ] ;
   /*   char *expr */
   /* num aliases */
   /* data aliases */
} CONNECTION4CALC_CREATE_INFO_IN ;



typedef struct
{
   S4LONG recNo ;
   BOOL4 recordLocked ;
   BOOL4 eofFlag ;
   BOOL4 bofFlag ;
   char blank[2] ;
   /*   char *record ; */
} CONNECTION4BOTTOM_INFO_OUT ;



typedef struct
{
   /* structure for d4recCount communication information between client/server */
   S4LONG recCount ;
   BOOL4 appendLocked ;
   char blank[2] ;
} CONNECTION4RECCOUNT_INFO_OUT ;



#ifndef S4SINGLE
   typedef struct
   {
      /* structures for d4lock communication information between client/server */
      short type ;
      short test ;    /* are we testing for a lock only? */
      enum Lock4type lockType ;  /* lock4read/lock4write */
      /* record info if appropriate */
   } CONNECTION4LOCK_INFO_IN ;
#endif



typedef struct
{
   /* structures for d4write communication information between client/server */
   S4LONG recNo ;
   short numMemoFields ;   /* number of memo fields with data included */
   BOOL4 unlock ;   /* should a general unlock call be performed (d4appendStart) */
   /* char record[recWidth] */
   /* CONNECTION4MEMO memo[numMemoFields] */
} CONNECTION4WRITE_INFO_IN ;



typedef struct
{
   BOOL4 recordLocked ;
} CONNECTION4WRITE_INFO_OUT ;



typedef struct
{
   unsigned S4LONG int memoLen ;
   unsigned short fieldNum ;
   char blank[2] ;
   /* char memo[memoLen] */
} CONNECTION4MEMO ;



typedef struct
{
   S4LONG recNo ;
   short fieldNo ;
   char blank[2] ;
} CONNECTION4MEMO_INFO_IN ;



typedef struct
{
  S4LONG int memoLen ;
   /* char memo[memoLen] */
} CONNECTION4MEMO_INFO_OUT ;



typedef struct
{
   short numMemoFields ;   /* number of memo fields with data included */
   /* char record[recWidth] */
   /* CONNECTION4MEMO memo[numMemoFields] */
} CONNECTION4APPEND_INFO_IN ;



typedef struct
{
   S4LONG recNum ;
   double autoIncrementVal ;
   BOOL4 bofFlag ;
   BOOL4 eofFlag ;
   BOOL4 recordChanged ;
   BOOL4 recordLocked ;
   BOOL4 appendLocked ;
   char  blank[2];
} CONNECTION4APPEND_INFO_OUT ;



typedef struct
{
   /* structures for d4zap communication information between client/server */
   S4LONG recStart ;
   S4LONG recStop ;
} CONNECTION4ZAP_INFO_IN ;



typedef struct
{
   short unique ;
   char alias[even4up( LEN4TAG_ALIAS + 1 )] ;
} CONNECTION4UNIQUE_TAG_INFO ;



typedef struct
{
   unsigned short numTags ;
   /* for each tag...
      CONNECTION4UNIQUE_TAG_INFO tags[numTags] ;
   */
} CONNECTION4UNIQUE_INFO_IN ;



typedef struct
{
   char dateFormat[20] ;
} CONNECTION4DATE_FORMAT_SET_INFO_IN ;



typedef struct
{
   S4LONG clientId ;
   CONNECTION4PTR_UNION dataTagName ;
   CONNECTION4PTR_UNION masterExpr ;
   CONNECTION4PTR_UNION dataAccessName ;
   CONNECTION4PTR_UNION dataAliasName ;
   short matchLen ;
   short relationType ;
   /*   short sortType ;*/
   short errorAction ;
   short numSlaves ;
   short dataId ;
   char blank[2] ;
} CONNECTION4RELATE ;



typedef struct
{
   CONNECTION4PTR_UNION exprSource ;
   CONNECTION4PTR_UNION sortSource ;
   BOOL4 skipBackwards ;
   char blank[2] ;
} CONNECTION4RELATION ;



typedef struct
{
   CONNECTION4RELATION relation ;
   unsigned S4LONG relationId ;
   S4LONG masterClientId ;
   unsigned short relateOffset ;
   unsigned short flexOffset ;
   unsigned short bitmapDisable ;
   char blank[2] ;
   /*   CONNECTION4RELATE relates[] ; */
   /*   flex data */
} CONNECTION4RELATE_INIT_INFO_IN ;



typedef struct
{
   unsigned S4LONG relationId ;
   /* list of relateid's for each relate */
} CONNECTION4RELATE_INIT_INFO_OUT ;



typedef struct
{
   /* the output from relate4top, relate4bottom, relate4skip, relate4seek */
   unsigned S4LONG relationId ;
   /* record_num, record_count, record for each record */
} CONNECTION4RELATION_DATA_OUT ;



typedef struct
{
   unsigned S4LONG relationId ;
   short useGeneralTagsInRelate ;
   char blank[2] ;
} CONNECTION4RELATE_TOP_INFO_IN ;



typedef struct
{
   unsigned S4LONG relationId ;
   S4LONG numSkips ;
} CONNECTION4RELATE_SKIP_INFO_IN ;



typedef struct
{
   unsigned S4LONG relationId ;
} CONNECTION4RELATE_BOTTOM_INFO_IN ;



typedef struct
{
   unsigned S4LONG relationId ;
   short useGeneralTagsInRelate ;
   char blank[2] ;
} CONNECTION4RELATE_OPT_INFO_IN ;



typedef struct
{
   unsigned S4LONG relationId ;
   S4LONG masterStartPos ;
   unsigned short relateId ;
   char blank[2] ;
} CONNECTION4RELATE_DO_INFO_IN ;



typedef struct
{
   unsigned S4LONG relationId ;
   S4LONG masterStartPos ;
   unsigned short relateId ;
   char blank[2] ;
} CONNECTION4RELATE_DO_ONE_INFO_IN ;



typedef struct
{
   S4LONG recNo ;
} CONNECTION4RELATE_DO_ONE_INFO_OUT ;



typedef struct
{
   unsigned S4LONG eof ;
} CONNECTION4TRAN_EOF_INFO_OUT ;



typedef struct
{
   unsigned S4LONG eof ;
} CONNECTION4TRAN_EOF_HALT_INFO_OUT ;



typedef struct
{
   unsigned S4LONG relationId ;
} CONNECTION4RELATE_FREE_INFO_IN ;



#ifndef S4SINGLE
   typedef struct
   {
      S4LONG clientId ;
      S4LONG serverId ;
   } CONNECTION4RELATE_LOCK_SUB_DATA ;



   typedef struct
   {
      unsigned S4LONG relationId ;
   } CONNECTION4RELATE_LOCK_INFO_IN ;



   typedef struct
   {
      unsigned short count ;
   } CONNECTION4RELATE_LOCK_INFO_OUT ;



   typedef struct
   {
      unsigned S4LONG relationId ;
   } CONNECTION4RELATE_UNLOCK_INFO_IN ;



   typedef struct
   {
      S4LONG lockPos ;  /* ( record no if > 0, appendBytes if 0, file if -1 ) */
      /*   char fileName[] ;*/
      /*   char userId[] ;*/
      /*   char netId[] ;*/
   } CONNECTION4LOCKED_INFO_OUT ;
#endif



typedef struct
{
   S4LONG elapsedSeconds ;
   S4LONG clientId ;
   unsigned S4LONG numRequests ;
   unsigned S4LONG numTransactions ;
   unsigned S4LONG numCompletedTransactions ;
   unsigned S4LONG numRollbacks ;
   unsigned short numData ;
   unsigned short numRelate ;
   short activeTransaction ;   /* is there a current transaction? */
   char blank[6] ;
} CONNECTION4CLIENT_INFO ;



typedef struct
{
   // AS 12/06/99 -- changed, the clientId may be as large as a LONGLONG.  Just make it
   // 20 characters to ensure works with unix, etc.
   char clientId[20] ;
} CONNECTION4CLIENT_CANCEL_INFO_IN ;



typedef struct CONNECTION4SERVER_INFO_OUTSt
{
   S4LONG elapsedSeconds ;
   S4LONG memMax ;
   S4LONG memAlloc ;
   unsigned S4LONG numRequests ;
   unsigned short numClients ;
   unsigned short nOpenFiles ;
   unsigned short numClientsOdbc ;
   /* char blank[2] ; */
   /* CONNECTION4CLIENT_INFO clients[numClients] */
} CONNECTION4SERVER_INFO_OUT ;



typedef struct CONNECT5FIELD_INFO_INSt
{
   char name[12] ;
   short type ;
   unsigned short len ;
   unsigned short dec ;
   unsigned short nulls ;
} CONNECT5FIELD_INFO_IN ;



#ifdef __cplusplus
   extern "C" {
#endif
void connection4clear( CONNECTION4 * ) ;
int connection4addData( CONNECTION4 *, const void *, long, void ** ) ;

int connection4disconnect( CONNECTION4 * ) ;
int connection4errorDescribeExecute( const CONNECTION4 *, CODE4 *, int, long, const char *, const char *, const char * ) ;
int connection4init( CONNECTION4 *, CONNECT4 * ) ;
int connection4initUndo( CONNECTION4 * ) ;
int connection4insertData( CONNECTION4 *, const void *, const unsigned int, const int, const int ) ;
int connection4setStatus( CONNECTION4 *, const short ) ;

#define connection4send( a, b, c ) ( connect4send( (a)->connect, (b), (c) ) )
#define connection4sendFlush( a ) ( connect4sendFlush( (a)->connect ) )

int S4FUNCTION connection4sendMessageLow( CONNECTION4 *, char ) ;
/* AS 08/17/99 --> should be freeing up message every time by default, else server has
   massive memory leaks...
   #define connection4sendMessage( c ) connection4sendMessageLow( (c), 0 )
*/
#define connection4sendMessage( c ) connection4sendMessageLow( (c), 1 )
int S4FUNCTION connection4receive( CONNECTION4 *connection, char *data, int len ) ;
int connection4receiveMessage( CONNECTION4 * ) ;

#ifdef S4UTILS
   int S4FUNCTION connection4assign( CONNECTION4 *, const short, const long, const long ) ;
#else
   int connection4assign( CONNECTION4 *, const short, const long, const long ) ;
#endif

#define connection4data( c ) ( (c)->buffer )

#ifdef S4CLIENT
   int code4ping(CODE4 *c4, long s, long r ) ;
#endif

#ifdef S4SERVER
   int connection4waitForWork( SERVER4 * ) ;
#else
   short connection4repeat( CONNECTION4 * ) ;
#endif

#ifdef E4PARM_LOW
   int connection4type( const CONNECTION4 * ) ;
   long connection4errCode2( const CONNECTION4 * ) ;
   S4EXPORT short connection4status( const CONNECTION4 * ) ;
   int connection4setLen( CONNECTION4 *, const long int ) ;
   void connection4setRequestLockedInfo( CONNECTION4 *, int ) ;
   long connection4clientId( const CONNECTION4 * ) ;
   int connection4readLock( const CONNECTION4 * ) ;
   int connection4unlockAuto( const CONNECTION4 * ) ;
   long connection4serverId( const CONNECTION4 * ) ;
   long connection4len( const CONNECTION4 * ) ;
   void packet4setType( PACKET4 *, const short ) ;
   void packet4setStatus( PACKET4 *, int ) ;
   int packet4setLen( PACKET4 *, const long int ) ;
   long packet4len( const PACKET4 * ) ;
   short packet4status( const PACKET4 * ) ;
   long packet4errCode2( const PACKET4 * ) ;
   int packet4setErrCode2( PACKET4 *, const long ) ;
   int packet4type( const PACKET4 * ) ;
   long packet4serverId( const PACKET4 * ) ;
   long packet4clientId( const PACKET4 * ) ;
   int packet4readLock( const PACKET4 * ) ;
   int packet4unlockAuto( const PACKET4 * ) ;
   void packet4setClientId( PACKET4 *, const long ) ;
   void packet4setServerId( PACKET4 *, const long ) ;
   void packet4setReadLock( PACKET4 *, const int ) ;
   void packet4setRequestLockedInfo( PACKET4 *, const int ) ;
   void packet4setUnlockAuto( PACKET4 *, const int ) ;
#else
   #define packet4len( p ) ( (long)ntohl5((p)->dataLen) )
   #define packet4setLen( p, l ) ( ( (p)->dataLen = (htonl5(l)) ), 0 )
   #define packet4setStatus( p, s ) ( ( (p)->status = (htons5(s)) ) )
   #define packet4setType( p, t ) ( ( (p)->type = (htons5(t)) ) )
   #define packet4status( p ) ( (short)(ntohs5((p)->status )))
   #define packet4setErrCode2( p, e ) ( ((p)->errCode2 = (htonl5(e))), 0 )
   #define packet4errCode2( p ) ( ntohl5((p)->errCode2 ))
   #define packet4type( p ) ( ntohs5((p)->type) )
   #define packet4serverId( p ) ( ntohl5((p)->serverDataId ))
   #define packet4clientId( p ) ( ntohl5((p)->clientDataId ))
   #define packet4readLock( p ) ( ntohs5((p)->readLock) )
   #define packet4unlockAuto( p ) ( ntohs5((p)->unlockAuto) )
   #define packet4setClientId( p, i ) ( ((p)->clientDataId = (htonl5(i))) )
   #define packet4setServerId( p, i ) ( ((p)->serverDataId = (htonl5(i))) )
   #define packet4setReadLock( p, r ) ( ((p)->readLock = (htons5(r))) )
   #define packet4setUnlockAuto( p, v ) ( ((p)->unlockAuto = (htons5(v))) )
   #define connection4clientId( c ) ( packet4clientId( &((c)->packet) ) )
   #define connection4readLock( c ) ( packet4readLock( &((c)->packet) ) )
   #define connection4unlockAuto( c ) ( packet4unlockAuto( &((c)->packet) ) )
   #define connection4serverId( c ) ( packet4serverId( &((c)->packet) ) )
   #define connection4len( c ) ( packet4len( &((c)->packet) ) )
   #define connection4setLen( c, l ) ( packet4setLen( &((c)->packet), (l) ) )
   #define connection4setRequestLockedInfo( c, v ) ( packet4setRequestLockedInfo( &((c)->packet), (v) ) )
   #define connection4status( c ) ( packet4status( &((c)->packet) ) )
   #define connection4errCode2( c ) ( packet4errCode2( &((c)->packet) ) )
   #define connection4type( c ) ( packet4type( &((c)->packet)) )
#endif

#ifdef E4FILE_LINE
   #define connection4error( a, b, c, d ) ( s4fileName = __FILE__ , s4lineNo = __LINE__ , connection4errorDescribeExecute( a, b, c, d, 0, 0, 0 ) )
   #define connection4errorDescribe( a, b, c, d, e, f, g ) ( s4fileName = __FILE__ , s4lineNo = __LINE__ , connection4errorDescribeExecute( a, b, c, d, e, f, g ) )
#else
   #define connection4error( a, b, c, d ) ( connection4errorDescribeExecute( a, b, c, d, 0, 0, 0 ) )
   #define connection4errorDescribe( a, b, c, d, e, f, g ) ( connection4errorDescribeExecute( a, b, c, d, e, f, g ) )
#endif

#ifdef __cplusplus
   }
#endif

#endif  /* S4OFF_COMMUNICATIONS */
