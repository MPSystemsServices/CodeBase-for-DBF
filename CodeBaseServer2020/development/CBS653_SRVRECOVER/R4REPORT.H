/* r4report.h   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#ifndef __REPORT4HEADER
#define __REPORT4HEADER

#define ABS5(x)                ( (x<0)?-1*(x):x )
#define MIN5(a,b) (((a) < (b)) ? (a) : (b))
#define MAX5(a,b) (((a) > (b)) ? (a) : (b))


/* #include <windows.h> */

#define CREP_MASK 0x15  /* This masks the first byte of a .rep file */

#define START_VERSION_MASK 0x20
#ifdef S4WIN32
   #define VERSION_MASK     0x28
   #define VERSION_MASK_OLD 0x26
#else
   #define VERSION_MASK 0x26
#endif
#define STYLE_MASK 0x16

/* defined types used for specifying displayed units of measurement */
#define UNIT4POINTS    1
#define UNIT4INCHES    2
#define UNIT4CM        3

/* defines to distinguish the type of file being opened */
#define R4REPORT_FILE 1
#define R4RELATE_FILE 2

/* defined total types used for internal flags */
#define total4lowest  11
#define total4highest 12
#define total4count   13
#define total4average 14
#define total4sum     15

/* defined types for report output objects */
#define obj4type_field       400
#define obj4type_expr        401
#define obj4type_total       402
#define obj4type_calc        403
#define obj4type_text        404
#define obj4type_bitmap1     405
#define obj4type_bitmap2     409
#define obj4type_bitmap3     410
#define obj4type_hline       406
#define obj4type_vline       407
#define obj4type_frame       408

/* defines used for specifying text alignment within an output object */
#define justify4left   0
#define justify4right  1
#define justify4center 2

/* defines used for specifying display format for numeric values  */
#define obj4numNumber   0
#define obj4numExponent 1
#define obj4numCurrency 2
#define obj4numPercent  3

/* this define forces the use of the default index extension regardless of
   what extension that was saved in the report file  */
#define S4DEFAULT_INDEX 1

/* defines used for specifying current index format: these are for report
   function use only -- redefined in d4defs.h, previously used in cb51
#ifndef S4CB51
   #define r4mdx 1
   #define r4ndx 2
   #define r4ntx 3
   #define r4cdx 4
#endif
*/

/* these defines are for running the report module under windows  */
#ifdef S4WINDOWS
   /* user defined message sent by the report popup output window
      to the report parent window upon closing the output window */
   #define CRM_REPORTCLOSED (WM_USER+1000)

   #ifdef S4WIN32
      /* These macros are used for parsing commands in wind procs and are used
         for portability between windows and NT */
      #define GET_WM_COMMAND_CMD(wp, lp)      HIWORD(wp)
      #define GET_WM_COMMAND_ID(wp, lp)       LOWORD(wp)
//      #define GET_WM_COMMAND_HWND(wp, lp)     ((HWND)lp)

      /* offsets into the extra bytes of the popup output window */
      #define PWIN_OFF_REPORT 0
      #define PWIN_OFF_BDC    8
      #define PWIN_OFF_HBIT   12
      #define PWIN_OFF_PGWIN  16
      #define PWIN_OFF_OLDOBJ 20

      /* offsets into the extra bytes of the output page window */
      #define PGWIN_OFF_BDC   0
      #define PGWIN_OFF_HBIT  4
      #define PGWIN_OFF_YOFF  8

      /* portability macros used for setting and getting info in the window
         extra bytes */
      #define MGetWinP(a,b)  GetWindowLong( (HWND)a, (int)b )
      #define MSetWinP(a,b,c)  SetWindowLong( (HWND)a, (int)b, (LONG)c )
   #else
      /* These macros are used for parsing commands in wind procs and are used
         for portability between windows and NT */
      #define GET_WM_COMMAND_CMD(wp, lp)      HIWORD(lp)
      #define GET_WM_COMMAND_ID(wp, lp)       (wp)
      #define GET_WM_COMMAND_HWND(wp, lp)     ((HWND)LOWORD(lp))

      /* offsets into the extra bytes of the popup output window */
      #define PWIN_OFF_REPORT 0
      #define PWIN_OFF_BDC    4
      #define PWIN_OFF_HBIT   6
      #define PWIN_OFF_PGWIN  8
      #define PWIN_OFF_OLDOBJ 10

      /* offsets into the extra bytes of the output page window */
      #define PGWIN_OFF_BDC   0
      #define PGWIN_OFF_HBIT  2
      #define PGWIN_OFF_YOFF  4

      /* portability macros used for setting and getting info in the window
         extra bytes */
      #define MGetWinP(a,b) GetWindowWord( (HWND)a, (int)b )
      #define MSetWinP(a,b,c) SetWindowWord( (HWND)a, (int)b, (WORD)c )
      #define MoveToEx(a, b, c, d) MoveTo( a, b, c )
   #endif

   typedef struct
   {
      long dtStyle;
      BYTE dtItemCount;
      int  dtX;
      int  dtY;
      int  dtCX;
      int  dtCY;
   } DlgTemplateHeader;

   typedef struct
   {
      int  dtilX;
      int  dtilY;
      int  dtilCX;
      int  dtilCY;
      int  dtilID;
      long dtilStyle;
   } DlgItemTemplateHeader;

   typedef struct
   {
      HANDLE hBmi;
      HANDLE hDIBits;
   } R4DIBINFO, *PR4DIBINFO;

   #ifdef S4WIN32
      GLOBALHANDLE PrintDlgTemplate( void ) ;
   #else
      GLOBALHANDLE NewDlgTemplate( DWORD, WORD, WORD, WORD, WORD,
                                   LPSTR, LPSTR, LPSTR,
                                   LPSTR, WORD);

      BOOL AddDlgItem( GLOBALHANDLE, DWORD, WORD, WORD, WORD,
                       WORD, LPSTR, LPSTR, BYTE, LPSTR,
                       int );
      int TemplateSize( GLOBALHANDLE );
   #endif

   /*typedef struct tagLOGFONT R4LOGFONT;*/
   /*typedef R4LOGFONT* PR4LOGFONT;*/
   typedef HFONT R4HFONT;
   typedef COLORREF R4COLORREF;
   typedef BYTE R4BYTE;
   typedef WORD R4WORD;
   typedef DWORD R4DWORD;
#endif

#ifndef S4WINDOWS
   #undef TRUE
   #undef FALSE

   #define TRUE 1
   #define FALSE 0

   #define LF_FACESIZE 32

   typedef unsigned char R4BYTE;
   typedef unsigned short R4HFONT;
   typedef unsigned short R4WORD;
   typedef unsigned long  R4DWORD;
   typedef long R4COLORREF;
#endif

/* This structure is the same as the windows LOGFONT structure, I define it here
   so that it can be referred to in both windows and dos code */
   typedef struct r4tagLOGFONT
   {
      #ifdef S4WIN32
         long      lfHeight;
         long      lfWidth;
         long      lfEscapement;
         long      lfOrientation;
         long      lfWeight;
      /* previous longs used to be shorts - changed for 32 bit CodeReporter */
      #else
         short     lfHeight;
         short     lfWidth;
         short     lfEscapement;
         short     lfOrientation;
         short     lfWeight;
      #endif
      R4BYTE    lfItalic;
      R4BYTE    lfUnderline;
      R4BYTE    lfStrikeOut;
      R4BYTE    lfCharSet;
      R4BYTE    lfOutPrecision;
      R4BYTE    lfClipPrecision;
      R4BYTE    lfQuality;
      R4BYTE    lfPitchAndFamily;
      R4BYTE    lfFaceName[32];
   } R4LOGFONT, *PR4LOGFONT ;

/* macros for creating and parsing a colorref value */
#define R4GETBVALUE(rgb) ((R4BYTE)((rgb)>>16))
#define R4GETGVALUE(rgb) ((R4BYTE)(((R4WORD)(rgb)) >> 8))
#define R4GETRVALUE(rgb) ((R4BYTE)(rgb))
#define R4RGB(r,g,b)     ((R4COLORREF)(((R4BYTE)(r)|((R4WORD)(g)<<8))|(((R4DWORD)(R4BYTE)(b))<<16)))


/******************************************************************************/



/* this structure is used for supplying equivalent names to the windows font
   names for use on other platforms */
typedef struct STYLE4EQUIVFONTst
{
   LINK4 link;
   char winfont[33];
   char equivfont[33];
} STYLE4EQUIVFONT, S4PTR *PSTYLE4EQUIVFONT;

/******************************************************************************/
/* This structure is defined in  "e4expr.h" */

typedef struct TOTAL4st S4PTR *PTOTAL4 ;

/******************************************************************************/

/* The STYLE4 structure contains a style definition.  It contains information
   for both windows and non-windows cases*/
typedef struct STYLE4St
{
   LINK4 link;

   char name[20];    /* name of the style ex. 'Bold Blue' */

   /* These four members are used to hold the windows font/color info */
   R4LOGFONT lfont;   /* windows logical font structure used to create fonts */
   R4HFONT   screen_font, printer_font; /* handles for fonts created */
   R4COLORREF color; /* color of the styles text */

   /* members used by the codereporter exe for manipulating the style menu */
   long menu_id;
   short position;

   short point_size;

   /* members used for holding additional (non-windows) style info */
   short codes_before_len, codes_after_len;
   char *codes_before, *codes_after;

   #ifdef S4WINDOWS
   #ifdef S4WIN32
   LOGFONT ntFont;
   #endif
   #endif

} STYLE4, S4PTR *PSTYLE4;



/******************************************************************************/
/* GROUP4 structure: used for specifying the groupings of the records in the
   report
*/
typedef struct GROUP4St
{
   LINK4 link;

   short position;  /* holds the groups position in the groups in the report */

   short is_active; /* used by CR .exe to flag the selected group */

   /* two flags used to indicate whether this group is the page header/footer
      or the title summary group.  These two groups are created automatically
      and cannot be deleted. */
   short header_footer;
   short title_summary;

   /* flag used to indicate that the title/summary has been outputted */
   short tsdone;

   /* expression and values used for determining whether or not a group should
      reset */
   S4CONV( EXPR4 *resetExpression, EXPR4 *reset_expression ) ;
   S4CONV( char  *lastResetValue, char  *last_reset_value ) ;
   char *held_reset_value;

   /* user defined name for the group */
   char group_label[26];

   /* lists containing the display areas for the groups header and footer */
   LIST4 header_areas;
   LIST4 footer_areas;

   /* flags used to specify user defined options on the group */
   short swap_header;
   short swap_footer;
   short repeat_header;
   short reset_page;
   short reset_pagenum;
   short reset_flag;
   short lookahead;

   /* temporary flags */
   short tswap_header;
   short tswap_footer;
   short trepeat_header;
   short treset_page;
   short treset_pagenum;

   /* containing report */
   struct REPORT4St *report;

   /* in the CR executable the group has two info windows corresponding to
      the group header and footer */
   #ifdef S4WINDOWS
      HWND header_info_hWnd;
      HWND footer_info_hWnd;
   #endif
} GROUP4, S4PTR *PGROUP4;


/******************************************************************************/
/* AREA4 structure. Each group contains header and footer areas.  These areas
   have a specific size, and contain the actual report output objects.
*/
typedef struct AREA4St
{
   LINK4 link;

   short position;   /* position in list of areas */
   long height;      /* height of the area in 1000's of an inch */
   int  height_dev;  /* height of the area in device output units */

   short is_header;  /* flag specifying whether area is a header area */
   short is_active;  /* used by CR exe to indicate selected area */

   EXPR4 *suppression_condition;

   GROUP4 *group;    /* containing group */

   short allow_pagebreaks;  /* user set flag specifying whether an area can
                               be broken across page boundaries in output */

   LIST4 objects;    /* list of output objects in the area */

   struct REPORT4St *report;  /* containing report */

   /* in CR each area has a window associated with it */
   #ifdef S4WINDOWS
      HWND hWnd;
   #endif

} AREA4, S4PTR *PAREA4;


/******************************************************************************/
/* For non windows output each page of the report is placed in a buffer file.
   the information is in the format of a series of OBJECT4 structures.
*/
typedef struct OBJECT4St
{
   short objtype;    /* type of object. ie. obj4text, etc. */
   short alignment;  /* alignment of objects text, ie. justify4left */
   long x, y, w, h;  /* objects position in page and size in 1000's of inches */

   long info_len;    /* length of the info block */
   void *info;       /* information for the objects output, usually a text
                        string */

   short style_index; /* index into the reports style list */

} OBJECT4, S4PTR *POBJECT4;


/******************************************************************************/
/* PAGE4 structure is used for buffering the pages of the report in
   non-windows output
*/
typedef struct PAGE4St
{
   FILE4  file_buf;
   FILE4SEQ_READ seq_rd;
   FILE4SEQ_WRITE seq_wr;
   char fmem_buf[8192];
   char rmem_buf[8192];
   char *mem_buf, *mem_buf_pos;

   short first_read;
   long info_buf_len;

   OBJECT4 object;

} PAGE4, S4PTR *PPAGE4;


/******************************************************************************/
/* REPORT4 structure.  This is the main report structure.
*/

typedef struct REPORT4St
{
   S4CONV( CODE4 *codeBase, CODE4 *code_base ) ;  /* applications CODE4 structure */
   RELATE4 *relate;   /* the reports associated relation */

   char *report_name;         /* name of the report */
   char *report_file_name;    /* name of the file holding the report, with path */

   char *printer_name;
   #ifdef S4WINDOWS
      HDC printerDC;
   #endif

   char *stylesheet_name;
   char *report_caption;
   char default_date_format[20];

   GROUP4 *title_summary; /* GROUP4 holding title and summary areas.  This
                             group is also in the groups list */

   GROUP4 *page_header_footer; /* GROUP4 holding page header and footer. This
                                  group is not in the group list */

   LIST4 styles;               /* STYLE4 structs associated with report*/
   LIST4 groups;               /* list of the reports GROUP4 structs */
   LIST4 active_objects;       /* a list of the selected OBJ4 structures. This
                                  list is used by the CR .exe */

   LIST4 obj_type_info;        /* a list of OBJ4TYPE structures */
   LIST4 *obj_type_list;

   STYLE4 *active_style;

   long  sensitivity_x, sensitivity_y;
   int   dsx, dsy;
   short adjust_all;
   short units;
   short hide_info;
   short screen_breaks;
   short pgbrk_title;
   short pathnames;
   short bitmaps;

   int output_handle, use_styles;

   long report_width, report_height;
   long margin_top, margin_bottom, margin_right, margin_left;
   long hard_margin_top, hard_margin_bottom, hard_margin_right, hard_margin_left;
   long page_no, start_page, end_page, page_count;
   short page_nums;

   char decimal_point, thousand_separator, currency_symbol[11];

   /* info used in generating report output */
   short hard_resets_flag;
   short first;
   int output_code, output_flag;
   long dev_page_height, dev_page_width;
   long dev_margin_left, dev_margin_top, dev_margin_right, dev_margin_bottom;
   long dev_hard_margin_top, dev_hard_margin_bottom, dev_hard_margin_right, dev_hard_margin_left;
   long disp_bottom;
   long ypos, break_height;
   short in_header, end_report, broken, tdone;
   GROUP4 *group_on, *group_first;
   AREA4  *area_on;

   /* for use in generating a data file from report output */
   short   for_dbf;
   LIST4   output_objs;
   PGROUP4 output_group;
   FILE4SEQ_WRITE dfile_seq;
   char *dfile_name, *dfile_buf;
   DATA4 *data_file;
   long rcount;

   /* for non-windows report output */
   PAGE4 page_buf;

   #ifdef S4WINDOWS
      HWND hWnd;
      HWND hWnd2;
      HWND pagewin;
   #endif
} REPORT4, S4PTR *PREPORT4;


/******************************************************************************/

/* pre declarations */
struct  _OBJ_TYPE_struct;
struct  _OBJECTStruct;

/* OBJ4TYPE structure is used to hold information for each object type.
   currently this is limited to the object type name, windows class name,
   and the menu and cursor to be used in CR.  In the future this structure
   will be used to allow the import of third party or custom output objects */
typedef struct _OBJ_TYPE_struct
{
   LINK4 link;

   char name[26];
   char wndclass_name[46];
   int obj_type;
   #ifdef S4WINDOWS
   HINSTANCE lib_instance;

   HMENU hMenu;
   HCURSOR hCursor;
   #endif
} OBJ4TYPE;


/******************************************************************************/


typedef struct _OBJECTStruct
{
   LINK4 link;
   LINK4 link2;

   /* position within the containing area, and size, in 1000's of an inch */
   long x, y;
   long w, h;

   int num_chars;

   /* flags used by CR .exe for tagging selected objects */
   short is_active, is_first;

   /* flag and expression used for display once functionality.  this works in
      a similar fashion to the group reset */
   short display_once;
   EXPR4 *display_once_expr;
   char  *last_display_val;

   AREA4 *area; /* containing area */

   /* this is the text displayed in the object in the CR layout screen. In the
      case of a text object it is also the text for output */
   char *wintext;

   char *date_format;
   int date_changed;

   /* text to be output and length of text*/
   char *eval_text;
   int eval_len;

   /* style to use for objects output */
   STYLE4 *style;
   short style_location;

   short lookahead;

   OBJ4TYPE *obj_type;
   struct _OBJECTStruct *container;  /* pointer to containing object */

   LIST4 contained;           /* list of pointers to contained objects */

   short obj_type_num;

   short alignment; /* justify4left, justify4right, justify4center,
                       0=horizontal line, 1 = vertical line
                       0=squared frame, 1 = rounded frame */

   short numeric_type, display_zero, dec, use_brackets, leading_zero;
   short mod_type, background;

   void *data; /* generic pointer used to hold object specific info. eg. for
                  an expression object this will be an expression */

   long dev_x, dev_y, dev_w, dev_h;

   /* these six members are used for sending report output to an data file */
   double dval;
   FIELD4 *field;
   char field_name[11];
   char field_type;
   int  field_len;
   int  field_dec;

   #ifdef S4WINDOWS
      HWND hWnd;
   #endif

} OBJ4, S4PTR *POBJ4;


/******************************************************************************/


typedef struct OUTOBJLIST_st
{
   LINK4 link;
   POBJ4 obj;
} OUT4OBJ, S4PTR *POUT4OBJ;


/******************************************************************************/


typedef struct INDEXLIST_st
{
   LINK4 link;
   char  *index_name;
   int   name_length;
}INAME4;


/******************************************************************************/


typedef struct N4CHANGE_st
{
   LINK4 link;
   char  *old_name;
   char  *new_name;
} N4CHANGE, S4PTR *PN4CHANGE;


/******************************************************************************/

/* structure used by CR .exe for creating an instant report */
#ifdef S4CR2
   typedef struct _inst4report
   {
      REPORT4 *report;
      LIST4   groups;
      LIST4   fields;
      LIST4   totals;
      char    *title, *sort_expr, *query_expr;
      int     hspace, vspace, max_obj_width;
      BOOL    vert_wrap, labels, labels_ingroup;
   } INST4REPORT, S4PTR *PINST4REPORT;


/******************************************************************************/


   typedef struct _inst4group
   {
      LINK4  link;
      char   *group_expr;
   }INST4GROUP, S4PTR *PINST4GROUP;


/******************************************************************************/


   typedef struct _inst4field
   {
      LINK4 link;
      FIELD4 *field;
   }INST4FIELD, S4PTR *PINST4FIELD;


/******************************************************************************/


   typedef struct _inst4total
   {
      LINK4 link;
      char *total_expr;
      PINST4GROUP igroup;
      PINST4FIELD ifield;
   }INST4TOTAL, S4PTR *PINST4TOTAL;

#endif


/******************************************************************************/


#ifdef __cplusplus
   extern "C" {
#endif

S4EXPORT PREPORT4 S4FUNCTION   report4init( RELATE4 S4PTR *);
#ifdef S4WINDOWS
   #ifndef S4CR2
      #define report4free( r4, i1, i2 ) report4freeLow( r4, (i1), (i2), 1 )
   #else
      #define report4free( r4, i1, i2 ) report4freeLow( r4, (i1), (i2), 0 )
   #endif
#else
   #define report4free( r4, i1, i2 ) report4freeLow( r4, (i1), (i2), 0 )
#endif
S4EXPORT void S4FUNCTION       report4freeLow( PREPORT4, short, short, short ) ;
S4EXPORT void S4FUNCTION       report4free_styles( REPORT4 *report );

S4EXPORT PGROUP4 S4FUNCTION    group4create( PREPORT4, char S4PTR *, char S4PTR * );
S4EXPORT void S4FUNCTION       group4free( PGROUP4 );
S4EXPORT void S4FUNCTION       group4positionSet( PGROUP4, int );
S4EXPORT PGROUP4 S4FUNCTION    group4create_title_summary( PREPORT4 );
S4EXPORT PGROUP4 S4FUNCTION    group4create_pgheader_pgfooter( PREPORT4 );
S4EXPORT int      S4FUNCTION   group4numHeaders( PGROUP4 );
S4EXPORT int      S4FUNCTION   group4swapHeader( PGROUP4, int );
S4EXPORT int      S4FUNCTION   group4swapFooter( PGROUP4, int );
S4EXPORT int      S4FUNCTION   group4repeatHeader( PGROUP4, int );
S4EXPORT int      S4FUNCTION   group4resetPage( PGROUP4, int );
S4EXPORT int      S4FUNCTION   group4resetPageNum( PGROUP4, int );


S4EXPORT PAREA4 S4FUNCTION     area4create( PGROUP4, long, short, char S4PTR * );
S4EXPORT void S4FUNCTION       area4free( PAREA4 );
S4EXPORT void S4FUNCTION       area4add_object( PAREA4, POBJ4 );

S4EXPORT POBJ4 S4FUNCTION      obj4create( PAREA4, long, long, long, long );
S4EXPORT void S4FUNCTION       obj4free( POBJ4 );

S4EXPORT POBJ4 S4FUNCTION      obj4fieldCreate( PAREA4, FIELD4 S4PTR *, long, long, long, long );
S4EXPORT void S4FUNCTION       obj4fieldFree( POBJ4 );

S4EXPORT POBJ4 S4FUNCTION      obj4totalCreate( PAREA4, PTOTAL4, long, long, long, long );
S4EXPORT void S4FUNCTION       obj4totalFree( POBJ4 );

S4EXPORT POBJ4 S4FUNCTION      obj4calcCreate( PAREA4, EXPR4CALC S4PTR *, long, long, long, long );
S4EXPORT void S4FUNCTION       obj4calcFree( POBJ4 );

S4EXPORT POBJ4 S4FUNCTION      obj4exprCreate( PAREA4, EXPR4 S4PTR *, long, long, long, long );
S4EXPORT void S4FUNCTION       obj4exprFree( POBJ4 );

S4EXPORT POBJ4 S4FUNCTION      obj4textCreate( PAREA4, char S4PTR *, long, long, long, long );
S4EXPORT void S4FUNCTION       obj4textFree( POBJ4 );

S4EXPORT POBJ4 S4FUNCTION      obj4lineCreate( PAREA4, int, long, long, long );
S4EXPORT void S4FUNCTION       obj4lineFree( POBJ4 );
S4EXPORT int S4FUNCTION obj4lineWidth( POBJ4, long );

S4EXPORT POBJ4 S4FUNCTION      obj4frameCreate( PAREA4, long, long, long, long );
S4EXPORT void S4FUNCTION       obj4frameFree( POBJ4 );

S4EXPORT void S4FUNCTION       obj4delete( POBJ4 );
S4EXPORT void S4FUNCTION       obj4sort_obj_tree( POBJ4 );
void                           obj4evaluate( POBJ4 );
void                           obj4evaluate_lookahead( POBJ4 );
S4EXPORT int S4FUNCTION obj4justify( POBJ4, int );
S4EXPORT int S4FUNCTION obj4dateFormat( POBJ4, char S4PTR * );
S4EXPORT int S4FUNCTION obj4decimals( POBJ4, int );
S4EXPORT int S4FUNCTION obj4displayOnce( POBJ4, char S4PTR * );
S4EXPORT int S4FUNCTION obj4displayZero( POBJ4, int );
S4EXPORT int S4FUNCTION obj4numericType( POBJ4, int );
S4EXPORT int S4FUNCTION obj4leadingZero( POBJ4, int );
S4EXPORT int S4FUNCTION obj4brackets( POBJ4, int );
S4EXPORT int S4FUNCTION obj4style( POBJ4, PSTYLE4 );

S4EXPORT int S4FUNCTION report4dateFormat( PREPORT4, char S4PTR * );
S4EXPORT int S4FUNCTION report4querySet( PREPORT4, char S4PTR * );
S4EXPORT int S4FUNCTION report4sortSet( PREPORT4, char S4PTR * );

int report4conv_double( POBJ4, double, char * );

S4EXPORT PTOTAL4 S4FUNCTION    total4create( PREPORT4, char S4PTR *, char S4PTR *, int, char S4PTR * );
S4EXPORT void S4FUNCTION       total4value_reset( PTOTAL4 );
S4EXPORT void S4FUNCTION       total4free( PTOTAL4 );
S4EXPORT void                  total4value_update( PTOTAL4 );

#ifdef S4WINDOWS
S4EXPORT PSTYLE4 S4FUNCTION     style4create( PREPORT4, PR4LOGFONT, char S4PTR *, R4COLORREF, int );
#else
S4EXPORT PSTYLE4 S4FUNCTION     style4create( PREPORT4, char S4PTR *, int, char S4PTR *, int, char S4PTR * );
#endif
S4EXPORT PSTYLE4 S4FUNCTION     style4lookup( PREPORT4, char S4PTR * );
S4EXPORT BOOL S4FUNCTION        style4delete( PREPORT4, char S4PTR * );
S4EXPORT BOOL S4FUNCTION        style4free( PREPORT4, PSTYLE4 );
S4EXPORT int S4FUNCTION         style4color( PSTYLE4, R4COLORREF );
S4EXPORT int S4FUNCTION         style4default_set( PREPORT4, PSTYLE4 );

S4EXPORT int S4FUNCTION report4do( PREPORT4 );
S4EXPORT short S4FUNCTION report4doCB( REPORT4 * ) ;

#ifdef S4CR2
int S4FUNCTION report4save_object( FILE4SEQ_WRITE S4PTR *, POBJ4 );
int S4FUNCTION report4save_area( FILE4SEQ_WRITE S4PTR *, PAREA4 );
int S4FUNCTION report4save_group( FILE4SEQ_WRITE S4PTR *, PGROUP4 );
int S4FUNCTION report4save_style( FILE4SEQ_WRITE S4PTR *, PSTYLE4 );
int S4FUNCTION report4save_calc( FILE4SEQ_WRITE S4PTR *, EXPR4CALC S4PTR * );
int S4FUNCTION report4save_report( PREPORT4, FILE4SEQ_WRITE S4PTR * );

int S4FUNCTION report4retrieve_object( FILE4SEQ_READ S4PTR *, PAREA4 );
int S4FUNCTION report4retrieve_area( FILE4SEQ_READ S4PTR *, PGROUP4 );
int S4FUNCTION report4retrieve_group( FILE4SEQ_READ S4PTR *, PREPORT4 );
int S4FUNCTION report4retrieve_page_header_footer( FILE4SEQ_READ S4PTR *, PREPORT4 );
PSTYLE4 S4FUNCTION report4retrieve_style( FILE4SEQ_READ S4PTR *, PREPORT4, int );
int S4FUNCTION report4retrieve_calc2( FILE4SEQ_READ S4PTR *, PREPORT4 );
int S4FUNCTION report4retrieve_calc( FILE4SEQ_READ S4PTR *, PREPORT4 );
REPORT4 * S4FUNCTION   report4retrieve_report( FILE4SEQ_READ S4PTR *, RELATE4 S4PTR * );

int S4FUNCTION retrieve4string( FILE4SEQ_READ S4PTR *, char S4PTR *, int );
DATA4 S4PTR *  S4FUNCTION relate4lookup_data( RELATE4 S4PTR *, char S4PTR * );

int S4FUNCTION report4save( PREPORT4, char S4PTR *, int );
int S4FUNCTION relate4save2( RELATE4 *relate, char *file_name, int save_paths, char *buf, char *name_buf );
int S4FUNCTION relate4save( RELATE4 S4PTR *, char S4PTR *, int );

REPORT4 * S4FUNCTION  report4retrieve2(CODE4 *c4, char *file_name, int open_files,
                     char *pathname, char *buf, char *name_buf );
PREPORT4 S4FUNCTION  report4retrieve( CODE4 S4PTR *, char S4PTR *, int, char S4PTR * );

RELATE4 * S4FUNCTION relate4retrieve2( CODE4 *c4, char *file_name,
                      int open_files, char *pathname, char *buf, char *name_buf );
RELATE4 S4PTR * S4FUNCTION  relate4retrieve( CODE4 S4PTR *, char S4PTR *, int, char S4PTR * );

RELATE4 * S4FUNCTION   relate4retrieve_relate( FILE4SEQ_READ *seq, int open_files, char *spath, int file_type );
int S4FUNCTION obj4dataFieldSet( POBJ4 obj, char *fname, char ftype, int flength, int fdec );
int S4FUNCTION report4dataFileSet( PREPORT4 report, char *fname );
int S4FUNCTION report4dataGroup( PREPORT4, PGROUP4 );
#else
S4EXPORT int S4FUNCTION report4save_object( FILE4SEQ_WRITE S4PTR *, POBJ4 );
S4EXPORT int S4FUNCTION report4save_area( FILE4SEQ_WRITE S4PTR *, PAREA4 );
S4EXPORT int S4FUNCTION report4save_group( FILE4SEQ_WRITE S4PTR *, PGROUP4 );
S4EXPORT int S4FUNCTION report4save_style( FILE4SEQ_WRITE S4PTR *, PSTYLE4 );
S4EXPORT int S4FUNCTION report4save_calc( FILE4SEQ_WRITE S4PTR *, EXPR4CALC S4PTR * );
S4EXPORT int S4FUNCTION report4save_report( PREPORT4, FILE4SEQ_WRITE S4PTR * );

S4EXPORT int S4FUNCTION report4retrieve_object( FILE4SEQ_READ S4PTR *, PAREA4 );
S4EXPORT int S4FUNCTION report4retrieve_area( FILE4SEQ_READ S4PTR *, PGROUP4 );
S4EXPORT int S4FUNCTION report4retrieve_group( FILE4SEQ_READ S4PTR *, PREPORT4 );
S4EXPORT int S4FUNCTION report4retrieve_page_header_footer( FILE4SEQ_READ S4PTR *, PREPORT4 );
S4EXPORT PSTYLE4 S4FUNCTION report4retrieve_style( FILE4SEQ_READ S4PTR *, PREPORT4, int );
S4EXPORT int S4FUNCTION report4retrieve_calc2( FILE4SEQ_READ S4PTR *, PREPORT4 );
S4EXPORT int S4FUNCTION report4retrieve_calc( FILE4SEQ_READ S4PTR *, PREPORT4 );
S4EXPORT REPORT4 * S4FUNCTION   report4retrieve_report( FILE4SEQ_READ S4PTR *, RELATE4 S4PTR * );

S4EXPORT int S4FUNCTION retrieve4string( FILE4SEQ_READ S4PTR *, char S4PTR *, int );
S4EXPORT DATA4 S4PTR *  S4FUNCTION relate4lookup_data( RELATE4 S4PTR *, char S4PTR * );

S4EXPORT int S4FUNCTION report4save( PREPORT4, char S4PTR *, int );
S4EXPORT int S4FUNCTION relate4save2( RELATE4 *relate, char *file_name, int save_paths, char *buf, char *name_buf );
S4EXPORT int S4FUNCTION relate4save( RELATE4 S4PTR *, char S4PTR *, int );

S4EXPORT REPORT4 * S4FUNCTION  report4retrieve2(CODE4 *c4, char *file_name, int open_files,
                        char *pathname, char *buf, char *name_buf );
S4EXPORT PREPORT4 S4FUNCTION  report4retrieve( CODE4 S4PTR *, char S4PTR *, int, char S4PTR * );

S4EXPORT RELATE4 * S4FUNCTION relate4retrieve2( CODE4 *c4, char *file_name,
                      int open_files, char *pathname, char *buf, char *name_buf );
S4EXPORT RELATE4 S4PTR * S4FUNCTION  relate4retrieve( CODE4 S4PTR *, char S4PTR *, int, char S4PTR * );

S4EXPORT RELATE4 * S4FUNCTION   relate4retrieve_relate( FILE4SEQ_READ *seq, int open_files, char *spath, int file_type );
S4EXPORT int S4FUNCTION obj4dataFieldSet( POBJ4 obj, char *fname, char ftype, int flength, int fdec );
S4EXPORT int S4FUNCTION report4dataFileSet( PREPORT4 report, char *fname );
S4EXPORT int S4FUNCTION report4dataGroup( PREPORT4, PGROUP4 );
#endif

S4EXPORT RELATE4 S4PTR* S4FUNCTION relate4masterCB( RELATE4 * ) ;
S4EXPORT const char S4PTR* S4FUNCTION relate4masterExprCB( RELATE4 * ) ;
S4EXPORT DATA4 S4PTR* S4FUNCTION relate4dataCB( RELATE4 * ) ;
S4EXPORT TAG4 S4PTR* S4FUNCTION relate4dataTagCB( RELATE4 * ) ;
S4EXPORT RELATE4 S4PTR * S4FUNCTION report4relate( REPORT4 * ) ;

S4EXPORT int S4FUNCTION report4caption( PREPORT4, char S4PTR * );
S4EXPORT int S4FUNCTION obj4frameCorners( POBJ4, int );
S4EXPORT int S4FUNCTION obj4frameFill( POBJ4, int );
S4EXPORT int S4FUNCTION area4pageBreak( PAREA4, int );

S4EXPORT int S4FUNCTION report4hardResets( PREPORT4, int );

int report4alloc_records( PREPORT4 );
int report4swap_old_rec( PREPORT4 );
int report4make_old_rec( PREPORT4 );
int report4output_area_break( PAREA4 );

#ifdef S4WINDOWS
S4EXPORT int S4FUNCTION report4generatePage( PREPORT4, HDC );
int report4output_object( OBJ4 *, HDC );
int report4output_area( AREA4 *, HDC, int );
void report4output_pgheader( REPORT4 *, HDC hDC );
void report4output_pgfooter( REPORT4 *, HDC hDC );
void report4output_repeat_headers( REPORT4 *, HDC, GROUP4 * );
void report4output_swapped_footer( GROUP4 *, HDC );
S4EXPORT HDC S4FUNCTION report4get_printerDC ( void );
void report4output_screen_fonts( REPORT4 * );
int report4register_classes( PREPORT4 );
void report4calc_obj_dev( REPORT4 *, HDC );
S4EXPORT void S4FUNCTION obj4bitmapFileFree( POBJ4 obj );
S4EXPORT POBJ4 S4FUNCTION obj4bitmapFileCreate( PAREA4 area, char *filename, long x, long y, long w, long h );
S4EXPORT void S4FUNCTION obj4bitmapFieldFree( POBJ4 obj );
S4EXPORT POBJ4 S4FUNCTION obj4bitmapFieldCreate( PAREA4 area, FIELD4 *field, long x, long y, long w, long h );
#else
S4EXPORT int S4FUNCTION report4generatePage( PREPORT4 );
int report4output_object( OBJ4 *, PAGE4 *);
int report4output_area( AREA4 *, PAGE4 *, int );
void report4output_pgheader( REPORT4 *, PAGE4 *);
void report4output_pgfooter( REPORT4 *, PAGE4 *);
void report4output_repeat_headers( REPORT4 *, PAGE4 *, GROUP4 * );
void report4output_swapped_footer( GROUP4 *, PAGE4 *);
void report4calc_obj_dev( REPORT4 * );
void style4flagsGet( PSTYLE4 style, int *pt_size, int *bold, int *italic, int *underline, int *strikeout );
int style4equivFontGet( CODE4 *codeBase, PSTYLE4 style, char *buf, int bufsize );
void style4equivFontsRemove( CODE4 *codeBase );
S4EXPORT int S4FUNCTION style4equivFontRegister( CODE4 *codeBase, char *winfont, char *equivfont );
#endif

#ifdef S4DLL
   S4EXPORT HINSTANCE S4FUNCTION c4dllInst(void );
#endif

void report4evaluate_lookahead( GROUP4 * );
void report4check_lookahead( REPORT4 * );
void report4check_display_once( PREPORT4 );

S4EXPORT GROUP4 * S4FUNCTION report4calc_first_change_group( REPORT4 * );

#ifdef S4WINDOWS
HDC S4FUNCTION report4get_printerIC ( PREPORT4 );
#ifndef S4WIN32
long CALLBACK _export PreViewProc( HWND, UINT, WPARAM, LPARAM );
long CALLBACK _export OutputPageProc( HWND, UINT, WPARAM, LPARAM );
long CALLBACK _export MouseEatProc( HWND, UINT, WPARAM, LPARAM );
long CALLBACK _export PageStatProc( HWND, UINT, WPARAM, LPARAM );
#else
S4EXPORT long CALLBACK PreViewProc( HWND, UINT, WPARAM, LPARAM );
S4EXPORT long CALLBACK OutputPageProc( HWND, UINT, WPARAM, LPARAM );
S4EXPORT long CALLBACK MouseEatProc( HWND, UINT, WPARAM, LPARAM );
#endif
S4EXPORT void S4FUNCTION obj4bitmapStaticFree( POBJ4 obj );
S4EXPORT POBJ4 S4FUNCTION obj4bitmapStaticCreate( PAREA4 area, HANDLE hDIB, long x, long y, long w, long h );

S4EXPORT LPSTR S4FUNCTION bmp4FindDIBBits (LPSTR );
S4EXPORT WORD S4FUNCTION bmp4PaletteSize (LPSTR );
S4EXPORT WORD S4FUNCTION bmp4DIBNumColors (LPSTR );

S4EXPORT BOOL S4FUNCTION bmp4WriteDIB ( LPSTR, HANDLE );
S4EXPORT HANDLE S4FUNCTION bmp4ReadDIBFile (int);
S4EXPORT HANDLE S4FUNCTION bmp4GetDIB ( LPSTR, CODE4 S4PTR * );

S4EXPORT short S4FUNCTION report4printerSelect( PREPORT4 report );
S4EXPORT int S4FUNCTION report4parent( PREPORT4, HWND );
S4EXPORT HDC S4FUNCTION report4printerDC( PREPORT4, HDC );
#endif

S4EXPORT int S4FUNCTION expr4calc_remove( EXPR4CALC *calc, int remove_all );

S4EXPORT int S4FUNCTION expr4calcModify( EXPR4CALC *calc, char *expr_source );

S4EXPORT int S4FUNCTION report4output( PREPORT4 report, int output_handle, int use_styles );
S4EXPORT int S4FUNCTION report4margins( PREPORT4 report, long left, long right, long top, long bottom, int unit_type );
S4EXPORT int S4FUNCTION report4pageSize( PREPORT4 report, long pg_height, long pg_width, int unit_type );

S4EXPORT int S4FUNCTION report4deleteCalc( PREPORT4 report, EXPR4CALC S4PTR *del_calc );

S4EXPORT int S4FUNCTION report4styleSheetSave( REPORT4 *report, char *file_name );
S4EXPORT int S4FUNCTION report4styleSheetLoad( REPORT4 *report, char *file_name, int override );
S4EXPORT int S4FUNCTION report4currency( PREPORT4, char * );
S4EXPORT int S4FUNCTION report4decimal( PREPORT4, char );
S4EXPORT int S4FUNCTION report4separator( PREPORT4, char );

S4EXPORT int S4FUNCTION report4pageInit( PREPORT4 report );
S4EXPORT int S4FUNCTION report4pageFree( PREPORT4 report );
S4EXPORT POBJECT4 S4FUNCTION report4pageObjNext( PREPORT4 report );
S4EXPORT POBJECT4 S4FUNCTION report4pageObjFirst( PREPORT4 report );
S4EXPORT int S4FUNCTION report4toScreen( PREPORT4 report, short to_screen );

S4EXPORT PSTYLE4 S4FUNCTION report4styleFirst( PREPORT4 report );
S4EXPORT PSTYLE4 S4FUNCTION report4styleNext( PREPORT4 report, PSTYLE4 style );
S4EXPORT PSTYLE4 S4FUNCTION report4styleLast( PREPORT4 report );
S4EXPORT PSTYLE4 S4FUNCTION report4stylePrev( PREPORT4 report, PSTYLE4 style );
S4EXPORT int S4FUNCTION report4numStyles( PREPORT4 report );

S4EXPORT PGROUP4 S4FUNCTION report4groupFirst( PREPORT4 report );
S4EXPORT PGROUP4 S4FUNCTION report4groupNext( PREPORT4 report, PGROUP4 group );
S4EXPORT PGROUP4 S4FUNCTION report4groupLast( PREPORT4 report );
S4EXPORT PGROUP4 S4FUNCTION report4groupPrev( PREPORT4 report, PGROUP4 group );
S4EXPORT int S4FUNCTION report4numGroups( PREPORT4 report );
S4EXPORT PGROUP4 S4FUNCTION report4titleSummary( PREPORT4 report );
S4EXPORT PGROUP4 S4FUNCTION report4pageHeaderFooter( PREPORT4 report );

S4EXPORT PAREA4 S4FUNCTION group4headerFirst( PGROUP4 group );
S4EXPORT PAREA4 S4FUNCTION group4headerNext( PGROUP4 group, PAREA4 area );
S4EXPORT PAREA4 S4FUNCTION group4headerLast( PGROUP4 group );
S4EXPORT PAREA4 S4FUNCTION group4headerPrev( PGROUP4 group, PAREA4 area );
S4EXPORT int S4FUNCTION area4numHeaders( PGROUP4 group );
S4EXPORT PAREA4 S4FUNCTION group4footerFirst( PGROUP4 group );
S4EXPORT PAREA4 S4FUNCTION group4footerNext( PGROUP4 group, PAREA4 area );
S4EXPORT PAREA4 S4FUNCTION group4footerLast( PGROUP4 group );
S4EXPORT PAREA4 S4FUNCTION group4footerPrev( PGROUP4 group, PAREA4 area );
S4EXPORT int S4FUNCTION group4numFooters( PGROUP4 group );

S4EXPORT int S4FUNCTION report4titlePage( PREPORT4 report, int title_page );

S4EXPORT TOTAL4 * S4FUNCTION total4lookup( REPORT4 *report, char *name );
S4EXPORT int S4FUNCTION report4index_type( void );
S4EXPORT int S4FUNCTION report4off_write( void );

S4EXPORT PGROUP4 S4FUNCTION report4groupLookup( PREPORT4 report, char *group_name );
S4EXPORT int S4FUNCTION group4resetExprSet( PGROUP4 group, char *expr_src );

CR2EXPORT PREPORT4 S4FUNCTION report4retrieve_old(CODE4 *c4,char *file_name,int open_files);

#ifdef S4CR2
   CR2EXPORT int S4FUNCTION AlternateDataFile( char S4PTR *dname_buf, int len );
   CR2EXPORT int S4FUNCTION AlternateIndexFile( char S4PTR *iname_buf, int len );
   CR2EXPORT int S4FUNCTION AlternateTagName( char S4PTR *tname_buf, int len );
   CR2EXPORT int S4FUNCTION AlternateMasterExpression( char S4PTR *expr_buf, RELATE4 S4PTR *relate, int len );
   CR2EXPORT int S4FUNCTION AlternateSortExpression(  char S4PTR *expr_buf, RELATE4 S4PTR *relate, int len );
   CR2EXPORT int S4FUNCTION AlternateQueryExpression( char S4PTR *expr_buf, RELATE4 S4PTR *relate, int len );
#endif

#ifdef S4DLL
   S4EXPORT long S4FUNCTION vbRepWndProc( HWND, UINT, WPARAM, LPARAM ) ;
   S4EXPORT void S4FUNCTION report4registerVBClass( REPORT4 * ) ;
#endif
S4EXPORT int S4FUNCTION report4decimal_v( PREPORT4 report, char *decimal ) ;
S4EXPORT int S4FUNCTION report4separator_v( PREPORT4, char * ) ;

S4EXPORT POBJ4 S4FUNCTION area4objFirst( PAREA4 area );
S4EXPORT POBJ4 S4FUNCTION area4objNext( PAREA4 area, POBJ4 aobj );
S4EXPORT POBJ4 S4FUNCTION area4objPrev( PAREA4 area, POBJ4 aobj );
S4EXPORT POBJ4 S4FUNCTION area4objLast( PAREA4 area );

S4EXPORT void S4FUNCTION obj4remove( POBJ4 obj );
S4EXPORT int S4FUNCTION area4numObjects( PAREA4 area );
S4EXPORT void S4FUNCTION area4sort_obj_tree( PAREA4 area );
S4EXPORT int S4FUNCTION report4pageMarginsGet( PREPORT4, long S4PTR *, long S4PTR*, long S4PTR *, long S4PTR * );
S4EXPORT int S4FUNCTION report4pageSizeGet( PREPORT4, long S4PTR *,long S4PTR * );
void report4nchange( CODE4 *, char **, int, int );
void report4free_name_list( void );
S4EXPORT PSTYLE4 S4FUNCTION report4styleSelected( PREPORT4 );
S4EXPORT int S4FUNCTION report4styleSelect( PREPORT4, PSTYLE4 );
S4EXPORT PSTYLE4 S4FUNCTION style4index( PREPORT4, int );

S4EXPORT int S4FUNCTION total4addCondition( PTOTAL4, char *, int );
S4EXPORT int S4FUNCTION report4dataDo( PREPORT4 report );

S4EXPORT int S4FUNCTION obj4lookAhead( POBJ4, int );


#ifdef __cplusplus
 }
#endif

#define  E4_REP_NOSNAM    "Not a valid name in call to style4create()."
#define  E4_REP_DUPSNAM   "The specified name is already in use by another style."
#define  E4_REP_SSELECT   "NULL style specified in call to report4styleSelect()."
#define  E4_REP_SINDEX    "Invalid index specified in call to style4index()."
#define  E4_REP_AHEIGHT   "Invalid value specified for area height."
#define  E4_REP_AMEM      "Unable to allocate memory for area."
#define  E4_REP_AEXPR     "Invalid suppression expression."
#define  E4_REP_OBJNXT    "Invalid object pointer."
#define  E4_REP_OBJPRV    "Invalid object pointer."
#define  E4_REP_GNAME     "Invalid name specified in call to group4create()."
#define  E4_REP_DUPGNAME  "The specified name is in use by another group."
#define  E4_REP_GEXPR     "Invalid group reset expression."
#define  E4_REP_GPOS      "Invalid group position."
#define  E4_REP_SWPHDR    "Flag must be zero or greater."
#define  E4_REP_SWPFTR    "Flag must be zero or greater."
#define  E4_REP_RPTHDR    "Flag must be zero or greater."
#define  E4_REP_RSTPG     "Flag must be zero or greater."
#define  E4_REP_RSTPGN    "Flag must be zero or greater."
#define  E4_REP_GRPLK     "Invalid name specified in call to report4groupLookup()."
#define  E4_REP_ONE       "A report is already in existence."
#define  E4_REP_REPALLOC  "Unable to allocate memory for report."
#define  E4_REP_NONE      "There is no report currently in existence."
#define  E4_REP_CAPALLOC  "Unable to allocate memory for the report caption."
#define  E4_REP_BADRESET  "Invalid parameter in report4hardResets()."
#define  E4_REP_BADTITLE  "Invalid parameter in report4titlePage()."
#define  E4_REP_BADSCREEN "Invalid parameter in report4toScreen()."
#define  E4_REP_DFILE     "Unable to open report data file."
#define  E4_REP_IFILE     "Unable to open report index file."
#define  E4_REP_NOTAG     "Unable to locate report tag."
#define  E4_REP_NOMEXPR   "Unable to parse relate master expression."
#define  E4_REP_NOSORT    "Unable to parse relation sort expression."
#define  E4_REP_NOQUERY   "Unable to parse relation query expression."
#define  E4_REP_RELERR    "Error retrieving relation from file."
#define  E4_REP_TCREATEVAL "Invalid parameter in call to total4create()."
#define  E4_REP_TEXPR     "Unable to parse total expression."
#define  E4_REP_TMEM      "Unable to allocate memory for total."
#define  E4_REP_TCALC     "Unable to create total calculation."
#define  E4_REP_TREXPR    "Unable to parse total reset expression."
#define  E4_REP_TLKP      "Invalid parameter in call to total4lookup()."
#define  E4_REP_OBJMEM    "Unable to allocate memory for object."
#define  E4_REP_OBJAL     "Invalid parameter in call to obj4justify()."
#define  E4_REP_ZERO      "Invalid parameter in call to obj4displayZero()."
#define  E4_REP_NTYPE     "Invalid parameter in call to obj4numericType()."
#define  E4_REP_LZERO     "Invalid parameter in call to obj4leadingZero()."
#define  E4_REP_BRACK     "Invalid parameter in call to obj4brackets()."
#define  E4_REP_OSTYLE    "Invalid parameter in call to obj4style()."
#define  E4_REP_FCRN      "Invalid parameter in call to obj4frameCorners()."
#define  E4_REP_FFILL     "Invalid parameter in call to obj4frameFill()."
#define  E4_REP_OLOOK     "Invalid parameter in call to obj4lookahead()."
#define  E4_REP_OUTWIN    "Unable to create report output window."
#define  E4_REP_PAGEWIN   "Unable to create report page window."
#define  E4_REP_PFILE     "Unable to create report page file."
#define  E4_REP_DOEXPR    "Unable to parse display once expression."
#define  E4_REP_SOBJ      "Unable to save object."
#define  E4_REP_SAREA     "Unable to save area."
#define  E4_REP_SGROUP    "Unable to save group."
#define  E4_REP_SPHF      "Unable to save page header/footer."
#define  E4_REP_ROBJ      "Unable to retrieve object."
#define  E4_REP_RHAREA    "Unable to retrieve header area."
#define  E4_REP_RFAREA    "Unable to retrieve footer area."
#define  E4_REP_RCALC     "Unable to retrieve calculation/total."
#define  E4_REP_RSTYLE    "Unable to retrieve style."
#define  E4_REP_RPHF      "Unable to retrieve page header/footer."
#define  E4_REP_RGROUP    "Unable to retrieve group."
#define  E4_REP_SFILE     "Unable to create save file."
#define  E4_REP_SCALC     "Unable to save calculation/total."
#define  E4_REP_SSTYLE    "Unable to save style."
#define  E4_REP_SREL      "Unable to save relate."
#define  E4_REP_RNUM      "Unable to retrieve report.\r\nA report already exists."
#define  E4_REP_RFILE     "Unable to retrieve report.\r\nFile is not a CodeReporter file."
#define  E4_REP_RFILE2    "Unable to retrieve report.\r\nBad file version."
#define  E4_REP_PGBRK     "Invalid parameter in call to area4pageBreak()."
#define  E4_REP_NODSETUP  "Error in setup for data file generation."
#define  E4_REP_NODCREATE "Unable to create the specified data file for generation."
#define  E4_REP_NOBUFPAGE "There is no output page currently buffered."
#define  E4_REP_OBJDATAAL "Unable to allocate memory for object data."

#define  E4_RESULT_INT    "Internal buffer too small while retrieving report"
#define  E4_RESULT_LCF    "Could not locate File:"

#endif /* __REPORT4HEADER */

