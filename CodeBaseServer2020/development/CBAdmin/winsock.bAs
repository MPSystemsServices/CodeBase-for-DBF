Attribute VB_Name = "Module1"

'' wsock32 header file for Visual Basic 5.0/32 bit
'' By: Steven E. Jacobs, sjacobs@vnet.net
'' Any errors, comments, additions, etc email them to me PLEASE!!
'' Although I tried to check everything, NOT ALL routine calls to wsock32.dll have been
'' checked! I used the standards in the VB5 Programmers guide, Chapter 26.
'' This file is NOT guaranteed to be correct!
''
Option Explicit
Global sockettype As Integer
Global Const FD_SETSIZE = 64
''
Global Const IOCPARM_MASK = &H7F
Global Const IOC_VOID = &H20000000
Global Const IOC_OUT = &H40000000
Global Const IOC_IN = &H80000000
Global Const IOC_INOUT = IOC_IN Or IOC_OUT
''
Global Const IPPROTO_IP = 0
Global Const IPPROTO_ICMP = 1
Global Const IPPROTO_GGP = 2
Global Const IPPROTO_TCP = 6
Global Const IPPROTO_PUP = 12
Global Const IPPROTO_UDP = 17
Global Const IPPROTO_IDP = 22
Global Const IPPROTO_ND = 77
Global Const IPPROTO_RAW = 255
Global Const IPPROTO_MAX = 256
'' Ports
Global Const IPPORT_ECHO = 7
Global Const IPPORT_DISCARD = 9
Global Const IPPORT_SYSTAT = 11
Global Const IPPORT_DAYTIME = 13
Global Const IPPORT_NETSTAT = 15
Global Const IPPORT_FTP = 21
Global Const IPPORT_TELNET = 23
Global Const IPPORT_SMTP = 25
Global Const IPPORT_TIMESERVER = 37
Global Const IPPORT_NAMESERVER = 42
Global Const IPPORT_WHOIS = 43
Global Const IPPORT_MTP = 57
'' Port/Socket numbers: Host specific functions
Global Const IPPORT_TFTP = 69
Global Const IPPORT_RJE = 77
Global Const IPPORT_FINGER = 79
Global Const IPPORT_HTTP = 80
Global Const IPPORT_TTYLINK = 87
Global Const IPPORT_SUPDUP = 95
'' UNIX TCP sockets
Global Const IPPORT_EXECSERVER = 512
Global Const IPPORT_LOGINSERVER = 513
Global Const IPPORT_CMDSERVER = 514
Global Const IPPORT_EFSSERVER = 520
'' UNIX UDP sockets
Global Const IPPORT_BIFFUDP = 512
Global Const IPPORT_WHOSERVER = 513
Global Const IPPORT_ROUTESERVER = 520
'' Ports < IPPORT_RESERVED are reserved for privileged processes (e.g. root)
Global Const IPPORT_RESERVED = 1024
' Link Numbers
Global Const IMPLINK_IP = 155
Global Const IMPLINK_LOWEXPER = 156
Global Const IMPLINK_HIGHEXPER = 158
'' Definitions of bits in internet address integers.
'' On subnets, the decomposition of addresses to host and net parts
'' is done according to subnet mask, not the masks here.
Global Const IN_CLASSA_NET = &HFF000000   ' 0xff000000
Global Const IN_CLASSA_NSHIFT = 24
Global Const IN_CLASSA_HOST = &HFFFFFF    ' 0x00ffffff
Global Const IN_CLASSA_MAX = 128
Global Const IN_CLASSB_NET = &HFFFF0000   ' 0xffff0000
Global Const IN_CLASSB_NSHIFT = 16
Global Const IN_CLASSB_HOST = &HFFFF      ' 0x0000ffff
Global Const IN_CLASSB_MAX = 65536
Global Const IN_CLASSC_NET = &HFFFFFF00   ' 0xffffff00
Global Const IN_CLASSC_NSHIFT = 8
Global Const IN_CLASSC_HOST = &HFF        ' 0x000000ff
Global Const INADDR_ANY = &H0
Global Const INADDR_LOOPBACK = &H7F000001 ' 0x7f000001
Global Const INADDR_BROADCAST = &HFFFFFFFF
Global Const INADDR_NONE = &HFFFFFFFF
''
Global Const WSADESCRIPTION_LEN = 256
Global Const WSASYS_STATUS_LEN = 128
'' Options for use with [gs]etsockopt at the IP level
Global Const IP_OPTIONS = 1
'' Types
Global Const SOCK_STREAM = 1
Global Const SOCK_DGRAM = 2
Global Const SOCK_RAW = 3
Global Const SOCK_RDM = 4
Global Const SOCK_SEQPACKET = 5
'' Option flags per-socket
Global Const SO_DEBUG = &H1         ' 0x0001 turn on debugging info recording
Global Const SO_ACCEPTCONN = &H2    ' 0x0002 socket has had listen()
Global Const SO_REUSEADDR = &H4     ' 0x0004 allow local address reuse
Global Const SO_KEEPALIVE = &H8     ' 0x0008 keep connections alive
Global Const SO_DONTROUTE = &H10    ' 0x0010 just use interface addresses
Global Const SO_BROADCAST = &H20    ' 0x0020 permit sending of broadcast messages
Global Const SO_USELOOPBACK = &H40  ' 0x0040 bypass hardware when possible
Global Const SO_LINGER = &H80       ' 0x0080 linger on close if data present
Global Const SO_OOBINLINE = &H100   ' 0x0100 leave received OOB data in line
Global Const SO_DONTLINGER = Not SO_LINGER
'' Additional options
Global Const SO_SNDBUF = &H1001   ' 0x1001 send buffer size
Global Const SO_RCVBUF = &H1002   ' 0z1002 receive buffer size
Global Const SO_SNDLOWAT = &H1003 ' 0x1003 send low-water mark
Global Const SO_RCVLOWAT = &H1004 ' 0x1004 receive low-water mark
Global Const SO_SNDTIMEO = &H1005 ' 0x1005 send timeout
Global Const SO_RCVTIMEO = &H1006 ' 0x1006 receive timeout
Global Const SO_ERROR = &H1007    ' 0x1007 get error status and clear
Global Const SO_TYPE = &H1008     ' 0x1008 get socket type
'' TCP options
Global Const TCP_NODELAY = &H1 ' 0x0001
'' Address families
Global Const AF_UNSPEC = 0     ' unspecified
Global Const AF_UNIX = 1       ' local to host (pipes, portals)
Global Const AF_INET = 2       ' internetworks: UDP, TCP, etc.
Global Const AF_IMPLINK = 3    ' arpanet imp addresses
Global Const AF_PUP = 4        ' pup protocols: e.g. BSP
Global Const AF_CHAOS = 5      ' mit CHAOS protocols
Global Const AF_NS = 6         ' XEROX NS protocols
Global Const AF_ISO = 7        ' ISO protocols
Global Const AF_OSI = 7        ' OSI is ISO
Global Const AF_ECMA = 8       ' european computer manufacturers
Global Const AF_DATAKIT = 9    ' datakit protocols
Global Const AF_CCITT = 10     ' CCITT protocols, X.25 cte
Global Const AF_SNA = 11       ' IBM SNA
Global Const AF_DECNET = 12    ' DECnet
Global Const AF_DLI = 13       ' Direct data link interface
Global Const AF_LAT = 14       ' LAT
Global Const AF_HYLINK = 15    ' NSC Hyperchannel
Global Const AF_APPLETALK = 16 ' AppleTalk
Global Const AF_NETBIOS = 17   ' NetBios style addresses
Global Const AF_MAX = 18
'' Protocol families, same as address families for now
Global Const PF_UNSPEC = 0     ' unspecified
Global Const PF_UNIX = 1       ' local to host (pipes, portals)
Global Const PF_INET = 2       ' internetworks: UDP, TCP, etc.
Global Const PF_IMPLINK = 3    ' arpanet imp addresses
Global Const PF_PUP = 4        ' pup protocols: e.g. BSP
Global Const PF_CHAOS = 5      ' mit CHAOS protocols
Global Const PF_NS = 6         ' XEROX NS protocols
Global Const PF_ISO = 7        ' ISO protocols
Global Const PF_OSI = 7        ' OSI is ISO
Global Const PF_ECMA = 8       ' european computer manufacturers
Global Const PF_DATAKIT = 9    ' datakit protocols
Global Const PF_CCITT = 10     ' CCITT protocols, X.25 cte
Global Const PF_SNA = 11       ' IBM SNA
Global Const PF_DECNET = 12    ' DECnet
Global Const PF_DLI = 13       ' Direct data link interface
Global Const PF_LAT = 14       ' LAT
Global Const PF_HYLINK = 15    ' NSC Hyperchannel
Global Const PF_APPLETALK = 16 ' AppleTalk
Global Const PF_NETBIOS = 17   ' NetBios style addresses
Global Const PF_MAX = 18
'' Level number for [get/set]sockopt() to apply to socket itself
Global Const SOL_SOCKET = &HFFFF ' 0xffff options for socket level
'' Maximum que length specifiable by listen()
Global Const SOMAXCONN = 5

Global Const MSG_OOB = 1       ' 0x1 process out of band data
Global Const MSG_PEEK = 2      ' 0x2 peek at incoming message
Global Const MSG_DONTROUTE = 4 ' 0x4 send without using routing tables
Global Const MSG_MAXIOVLEN = 16
'' define constant based on RFC883, used by gethostbyxxxx()
Global Const FD_READ = 1       ' 0x01
Global Const FD_WRITE = 2      ' 0x02
Global Const FD_OOB = &H4      ' 0x04
Global Const FD_ACCEPT = &H8   ' 0x08
Global Const FD_CONNECT = &H10 ' 0x10
Global Const FD_CLOSE = &H20   ' 0x20
'' Windows Sockets definitions of regular Microsoft C error constants
Global Const WSABASEERR = 10000
Global Const WSAEINTR = 10004
Global Const WSAEBADF = 10009
Global Const WSAEACCES = 10013
Global Const WSAEFAULT = 10014
Global Const WSAEINVAL = 10022
Global Const WSAEMFILE = 10024
'' Windows Sockets definitions of regular Berkeley error constants
Global Const WSAEWOULDBLOCK = 10035
Global Const WSAEINPROGRESS = 10036
Global Const WSAEALREADY = 10037
Global Const WSAENOTSOCK = 10038
Global Const WSAEDESTADDRREQ = 10039
Global Const WSAEMSGSIZE = 10040
Global Const WSAEPROTOTYPE = 10041
Global Const WSAEPROTOOPT = 10042
Global Const WSAEPROTONOSUPPORT = 10043
Global Const WSAESOCKTNOSUPPORT = 10044
Global Const WSAEOPNOTSUPP = 10045
Global Const WSAEPFNOSUPPORT = 10046
Global Const WSAEAFNOSUPPORT = 10047
Global Const WSAEADDRINUSE = 10048
Global Const WSAEADDRNOTAVAIL = 10049
Global Const WSAENETDOWN = 10050
Global Const WSAENETUNREACH = 10051
Global Const WSAENETRESET = 10052
Global Const WSAECONNABORTED = 10053
Global Const WSAECONNRESET = 10054
Global Const WSAENOBUFS = 10055
Global Const WSAEISCONN = 10056
Global Const WSAENOTCONN = 10057
Global Const WSAESHUTDOWN = 10058
Global Const WSAETOOMANYREFS = 10059
Global Const WSAETIMEDOUT = 10060
Global Const WSAECONNREFUSED = 10061
Global Const WSAELOOP = 10062
Global Const WSAENAMETOOLONG = 10063
Global Const WSAEHOSTDOWN = 10064
Global Const WSAEHOSTUNREACH = 10065
Global Const WSAENOTEMPTY = 10066
Global Const WSAEPROCLIM = 10067
Global Const WSAEUSERS = 10068
Global Const WSAEDQUOT = 10069
Global Const WSAESTALE = 10070
Global Const WSAEREMOTE = 10071
'' Extended Windows sockets error constant definitions
Global Const WSASYSNOTREADY = 10091
Global Const WSAVERNOTSUPPORTED = 10092
Global Const WSANOTINITIALISED = 10093
'' Authoritative Answer: Host Not Found
Global Const WSAHOST_NOT_FOUND = WSABASEERR + 1001
Global Const HOST_NOT_FOUND = WSAHOST_NOT_FOUND
'' Non-Authoritative: Host Not Found or SERVERFAIL
Global Const WSATRY_AGAIN = WSABASEERR + 1002
Global Const TRY_AGAIN = WSATRY_AGAIN
'' Non recoverable errors, FORMERR, REFUSED, NOTIMP
Global Const WSANO_RECOVERY = WSABASEERR + 1003
Global Const NO_RECOVERY = WSANO_RECOVERY
'' Valid name, no data record of requested type
Global Const WSANO_DATA = WSABASEERR + 1004
Global Const NO_DATA = WSANO_DATA
'' No address, look for MX record
Global Const WSANO_ADDRESS = WSANO_DATA
Global Const NO_ADDRESS = WSANO_ADDRESS
'' generic socket errors
#If Win32 Then
    Global Const INVALID_SOCKET = Not 0 '' 32 bit unsigned even though VB is signed
    Global Const SOCK_ERROR = -1
    Global Const SOCKET_ERROR = -1
#Else
    Global Const INVALID_SOCKET = -1
    Global Const SOCK_ERROR = -1
    Global Const SOCKET_ERROR = -1
#End If
'' Structures
Type fd_setT
  fd_count As Integer          '' how many are in the set
  fd_array(FD_SETSIZE) As Long '' array of SOCKET handles (64)
End Type
Global FD__SET As fd_setT
Rem
Rem   Structure used in select() call, taken from the BSD file sys/time.h.
Rem
Type timeval
  tv_sec As Long
  tv_usec As Long
End Type

Type sockaddr_inT
  sin_family As Integer
  sin_port As Integer
  sin_addr As Long
  sin_zero As String * 8
End Type
Global sockaddr_in As sockaddr_inT

Type in_addr
 temp As Long
End Type

Type sockaddrT
  sa_family As Integer
  sa_data(14) As Byte
End Type
Global sockaddr As sockaddrT

Type WSAdata_type
   wVersion As Integer
   wHighVersion As Integer
   szDescription As String * 257
   szSystemStatus As String * 129
   iMaxSockets As Integer
   iMaxUdpDg As Long
   lpVendorInfo As String * 200
End Type

Global WSAdata As WSAdata_type
Global MySocket As Integer
Global WWWServer As sockaddr_inT


Type sockprotoT
  sp_family As Integer
  sp_protocol As Integer
End Type
Global sockproto As sockprotoT

Type lingerT
  l_onoff As Integer
  l_linger As Integer
End Type
Global Linger As lingerT

'' Windows API declares
#If Win32 Then
    Declare Function PostMessage Lib "user32" Alias "PostMessageA" (ByVal hwnd As Long, ByVal wMsg As Long, ByVal wParam As Long, ByVal lParam As Long) As Long
    ''Declare Sub MemCopy Lib "kernel32" Alias "RtlMoveMemory" (dest As Any, src As Any, ByVal Bytes As Long)
    Declare Function lstrlen Lib "kernel32" Alias "lstrlenA" (ByVal lpString As String) As Long
    Declare Function lstrcpyn Lib "kernel32" Alias "lstrcpynA" (ByVal lpString1 As String, ByVal lpString2 As String, ByVal iMaxLength As Long) As Long
#Else
    Declare Function PostMessage Lib "User" (ByVal hwnd As Integer, ByVal wMsg As Integer, ByVal wParam As Integer, lParam As Any) As Integer
    Declare Sub MemCopy Lib "Kernel" Alias "hmemcpy" (dest As Any, src As Any, ByVal Bytes As Long)
    Declare Function lstrlen Lib "Kernel" (ByVal lpString As Any) As Integer
    Declare Function lstrcpy Lib "Kernel" (ByVal lpString1 As Any, ByVal lpString2 As Any) As Long
#End If

#If Win32 Then
    Declare Function bind Lib "wsock32.dll" (ByVal s As Long, addr As sockaddr_inT, ByVal namelen As Long) As Integer
    Declare Function htonl Lib "wsock32.dll" (ByVal a As Long) As Integer
    Declare Function inet_addr Lib "wsock32.dll" (ByVal s As String) As Integer
    Declare Function inet_ntoa Lib "wsock32.dll" (ByVal in_address As Long) As Integer
    Declare Function ntohl Lib "wsock32.dll" (ByVal a As Long) As Integer
    Declare Function Socket Lib "wsock32.dll" Alias "socket" (ByVal af As Long, ByVal typesock As Long, ByVal protocol As Long) As Integer
    Declare Function htons Lib "wsock32.dll" (ByVal a As Long) As Integer
    Declare Function ntohs Lib "wsock32.dll" (ByVal a As Long) As Integer
    Declare Function connect Lib "wsock32.dll" (ByVal sock As Long, sockstruct As sockaddr_inT, ByVal structlen As Long) As Integer
    Declare Function Send Lib "wsock32.dll" Alias "send" (ByVal sock As Long, ByVal msg As String, ByVal msglen As Long, ByVal flag As Long) As Integer
    Declare Function recv Lib "wsock32.dll" (ByVal sock As Long, ByVal msg As String, ByVal msglen As Long, ByVal flag As Long) As Integer
    Declare Function listen Lib "wsock32.dll" (ByVal sock As Long, ByVal backlog As Long) As Integer
    Declare Function closesocket Lib "wsock32.dll" (ByVal sock As Long) As Integer
    Declare Function accept Lib "wsock32.dll" (ByVal sock As Long, sad As sockaddrT, sadlen As Long) As Integer
    Declare Function gethostname Lib "wsock32.dll" (ByVal name As String, ByVal namelen As Long) As Integer
    '' I'm NOT sure about these yet!
    ''Declare Function gethostbyname Lib "wsock32.dll" (ByVal name As String) As hostentT
    ''Declare Function gethostbyaddr Lib "wsock32.dll" (ByVal addr As String, ByVal length As Long, ByVal thetype As Long) As hostentT
    Declare Function getpeername Lib "wsock32.dll" (ByVal sock As Long, sockaddr As sockaddrT, sockaddrlen As Long) As Integer
    Declare Function getsockname Lib "wsock32.dll" (ByVal sock As Long, sockaddr As sockaddrT, sockaddrlen As Long) As Integer
    Declare Function shutdown Lib "wsock32.dll" (ByVal sock As Long, ByVal how As Long) As Integer
    Declare Function getsockopt Lib "wsock32.dll" (ByVal sock As Long, ByVal level As Long, ByVal optname As Long, ByVal optval As String, optlen As Long) As Integer
    Declare Function setsockopt Lib "wsock32.dll" (ByVal sock As Long, ByVal level As Integer, ByVal optname As Integer, optval As Any, ByVal optlen As Long) As Integer
    Declare Function ioctlsocket Lib "wsock32.dll" (ByVal sock As Long, ByVal cmd As Long, ByRef argp) As Integer
    Declare Function recvfrom Lib "wsock32.dll" (ByVal sock As Long, ByVal buf As String, ByVal length As Long, ByVal flags As Long, ByRef sad As sockaddrT, ByRef fromlen As Long) As Integer
    Declare Function sendto Lib "wsock32.dll" (ByVal sock As Long, ByVal buf As String, ByVal length As Long, ByVal flags As Long, ByRef sad As sockaddrT, ByRef tolen As Long) As Integer
    '' The next one will probably not work correctly! BE WARNED!!
    Declare Function sockselect Lib "wsock32.dll" Alias "_select" (ByVal nfds As Long, ByRef readfds As Any, ByRef writefds As Any, ByRef exceptfds As Any, ByRef timeout As timeval) As Integer
    ''
    Declare Function WSAStartup Lib "wsock32.dll" (ByVal a As Long, b As WSAdata_type) As Integer
    Declare Function WSACleanup Lib "wsock32.dll" () As Integer
    Declare Function WSAGetLastError Lib "wsock32.dll" () As Integer
    Declare Sub WSASetLastError Lib "Winsock.dll" (ByVal iError As Long)
    Declare Function WSAAsyncSelect Lib "wsock32.dll" (ByVal sock As Long, ByVal hwnd As Long, ByVal Message As Long, ByVal wEvent As Long) As Integer
    Declare Function WSAFDIsSet Lib "wsock32.dll" Alias "__WSAFDIsSet" (ByVal s As Long, passed_set As fd_setT) As Integer
    Declare Function WSAAsyncGetHostByAddr Lib "wsock32.dll" (ByVal hwnd As Long, ByVal wMsg As Long, ByVal addr As String, ByVal length As Long, ByVal uType As Long, ByVal buff As String, ByVal buflen As Long) As Integer
    Declare Function WSAAsyncGetHostByName Lib "wsock32.dll" (ByVal hwnd As Long, ByVal wMsg As Long, ByVal name As String, ByVal buf As String, ByVal buflen As Long) As Integer
    Declare Function WSAAsyncGetProtoByName Lib "wsock32.dll" (ByVal hwnd As Long, ByVal wMsg As Long, ByVal name As String, ByVal buf As String, ByVal buflen As Long) As Integer
    Declare Function WSAAsyncGetProtoByNumber Lib "wsock32.dll" (ByVal hwnd As Long, ByVal wMsg As Long, ByVal number As Long, ByVal buf As String, ByVal buflen As Long) As Integer
    Declare Function WSAAsyncGetServByName Lib "wsock32.dll" (ByVal hwnd As Long, ByVal wMsg As Long, ByVal name As String, ByVal proto As String, ByVal buf As String, ByVal buflen As Long) As Integer
    Declare Function WSAAsyncGetServByPort Lib "wsock32.dll" (ByVal hwnd As Long, ByVal wMsg As Long, ByVal Port As Integer, ByVal proto As String, ByVal buf As String, ByVal buflen As Long) As Integer
    Declare Function WSACancelAsyncRequest Lib "wsock32.dll" (ByVal taskhandle As Long) As Integer
    Declare Function WSACancelBlockingCall Lib "wsock32.dll" () As Integer
    Declare Function WSAIsBlocking Lib "wsock32.dll" () As Integer
    '' Routines in the control
    Declare Function SetSockLinger Lib "usermsg.ocx" (ByVal Socket As Long, ByVal l_onoff As Integer, ByVal l_linger As Integer) As Integer
    Declare Function GetSockLinger Lib "usermsg.ocx" (ByVal Socket As Long, ByVal l_onoff As Integer, ByVal l_linger As Integer) As Integer

#Else
    Declare Function accept Lib "Winsock.dll" (ByVal s As Integer, addr As sockaddr, addrlen As Integer) As Integer
    Declare Function bind Lib "Winsock.dll" (ByVal s As Integer, addr As sockaddr, ByVal namelen As Integer) As Integer
    Declare Function closesocket Lib "Winsock.dll" (ByVal s As Integer) As Integer
    Declare Function connect Lib "Winsock.dll" (ByVal s As Integer, addr As sockaddr, ByVal namelen As Integer) As Integer
    Declare Function ioctlsocket Lib "Winsock.dll" (ByVal s As Integer, ByVal cmd As Long, argp As Long) As Integer
    Declare Function getpeername Lib "Winsock.dll" (ByVal s As Integer, sname As sockaddr, namelen As Integer) As Integer
    Declare Function getsockname Lib "Winsock.dll" (ByVal s As Integer, sname As sockaddr, namelen As Integer) As Integer
    Declare Function getsockopt Lib "Winsock.dll" (ByVal s As Integer, ByVal level As Integer, ByVal optname As Integer, ByVal optval As String, optlen As Integer) As Integer
    Declare Function htonl Lib "Winsock.dll" (ByVal hostlong As Long) As Long
    Declare Function htons Lib "Winsock.dll" (ByVal hostshort As Integer) As Integer
    Declare Function inet_addr Lib "Winsock.dll" (ByVal cp As String) As Long
    Declare Function inet_ntoa Lib "Winsock.dll" (ByVal inn As Long) As Long
    Declare Function listen Lib "Winsock.dll" (ByVal s As Integer, ByVal backlog As Integer) As Integer
    Declare Function ntohl Lib "Winsock.dll" (ByVal netlong As Long) As Long
    Declare Function ntohs Lib "Winsock.dll" (ByVal netshort As Integer) As Integer
    Declare Function recv Lib "Winsock.dll" (ByVal s As Integer, buf As Any, ByVal buflen As Integer, ByVal flags As Integer) As Integer
    Declare Function recvfrom Lib "Winsock.dll" (ByVal s As Integer, ByVal buf As String, ByVal buflen As Integer, ByVal flags As Integer, from As sockaddr, fromlen As Integer) As Integer
    Declare Function ws_select Lib "Winsock.dll" Alias "select" (ByVal nfds As Integer, readfds As FD_SET, writefds As FD_SET, exceptfds As FD_SET, timeout As timeval) As Integer
    Declare Function Send Lib "Winsock.dll" Alias "send" (ByVal s As Integer, buf As Any, ByVal buflen As Integer, ByVal flags As Integer) As Integer
    Declare Function sendto Lib "Winsock.dll" (ByVal s As Integer, buf As Any, ByVal buflen As Integer, ByVal flags As Integer, to_addr As sockaddr, ByVal tolen As Integer) As Integer
    Declare Function setsockopt Lib "Winsock.dll" (ByVal s As Integer, ByVal level As Integer, ByVal optname As Integer, optval As Any, ByVal optlen As Integer) As Integer
    Declare Function shutdown Lib "Winsock.dll" (ByVal s As Integer, ByVal how As Integer) As Integer
    Declare Function Socket Lib "Winsock.dll" Alias "socket" (ByVal af As Integer, ByVal s_type As Integer, ByVal protocol As Integer) As Integer
    'DATABASE FUNCTIONS
    Declare Function gethostbyaddr Lib "Winsock.dll" (addr As Long, ByVal addr_len As Integer, ByVal addr_type As Integer) As Long
    Declare Function gethostbyname Lib "Winsock.dll" (ByVal host_name As String) As Long
    Declare Function gethostname Lib "Winsock.dll" (ByVal host_name As String, ByVal namelen As Integer) As Integer
    Declare Function getservbyport Lib "Winsock.dll" (ByVal Port As Integer, ByVal proto As String) As Long
    Declare Function getservbyname Lib "Winsock.dll" (ByVal serv_name As String, ByVal proto As String) As Long
    Declare Function getprotobynumber Lib "Winsock.dll" (ByVal proto As Integer) As Long
    Declare Function getprotobyname Lib "Winsock.dll" (ByVal proto_name As String) As Long
    'WINDOWS EXTENSIONS
    Declare Function WSAStartup Lib "Winsock.dll" (ByVal wVR As Integer, lpWSAD As WSADataType) As Integer
    Declare Function WSACleanup Lib "Winsock.dll" () As Integer
    Declare Sub WSASetLastError Lib "Winsock.dll" (ByVal iError As Integer)
    Declare Function WSAGetLastError Lib "Winsock.dll" () As Integer
    Declare Function WSAIsBlocking Lib "Winsock.dll" () As Integer
    Declare Function WSAUnhookBlockingHook Lib "Winsock.dll" () As Integer
    Declare Function WSASetBlockingHook Lib "Winsock.dll" (ByVal lpBlockFunc As Long) As Long
    Declare Function WSACancelBlockingCall Lib "Winsock.dll" () As Integer
    Declare Function WSAAsyncGetServByName Lib "Winsock.dll" (ByVal hwnd As Integer, ByVal wMsg As Integer, ByVal serv_name As String, ByVal proto As String, ByVal buf As String, ByVal buflen As Integer) As Integer
    Declare Function WSAAsyncGetServByPort Lib "Winsock.dll" (ByVal hwnd As Integer, ByVal wMsg As Integer, ByVal Port As Integer, ByVal proto As String, ByVal buf As String, ByVal buflen As Integer) As Integer
    Declare Function WSAAsyncGetProtoByName Lib "Winsock.dll" (ByVal hwnd As Integer, ByVal wMsg As Integer, ByVal proto_name As String, ByVal buf As String, ByVal buflen As Integer) As Integer
    Declare Function WSAAsyncGetProtoByNumber Lib "Winsock.dll" (ByVal hwnd As Integer, ByVal wMsg As Integer, ByVal number As Integer, ByVal buf As String, ByVal buflen As Integer) As Integer
    Declare Function WSAAsyncGetHostByName Lib "Winsock.dll" (ByVal hwnd As Integer, ByVal wMsg As Integer, ByVal host_name As String, ByVal buf As String, ByVal buflen As Integer) As Integer
    Declare Function WSAAsyncGetHostByAddr Lib "Winsock.dll" (ByVal hwnd As Integer, ByVal wMsg As Integer, ByVal addr As String, ByVal addr_len As Integer, ByVal addr_type As Integer, ByVal buf As String, ByVal buflen As Integer) As Integer
    Declare Function WSACancelAsyncRequest Lib "Winsock.dll" (ByVal hAsyncTaskHandle As Integer) As Integer
    Declare Function WSAAsyncSelect Lib "Winsock.dll" (ByVal s As Integer, ByVal hwnd As Integer, ByVal wMsg As Integer, ByVal lEvent As Long) As Integer
    Declare Function WSARecvEx Lib "Winsock.dll" (ByVal s As Integer, buf As Any, ByVal buflen As Integer, ByVal flags As Integer) As Integer
#End If
'' Custom Constants
Global Const WM_USER = &H400
'' The constants below can be anything that you want!
Global Const CLIENT_MSG = WM_USER + 1
'' This constant is used in VBInitListenSocket
Global Const INCOMING_MSG = WM_USER + 2
Global Const VER_REQUIRED = &H101
Global Const BUFFER_SIZE = 1024

'This routine sends debug output to 'debug.log' in the apps
'current directory
Public Sub DebugPrintF(oStr$)
    Dim oFileHandle As Integer
    oFileHandle = FreeFile
    Open App.path & "\debug.log" For Append As #oFileHandle
    Print #oFileHandle, oStr$
    Close #oFileHandle
End Sub

'This routine sends oStr$ to the printer (LPT1:)
Public Sub DebugPrintP(oStr$)
    Dim oFileHandle As Integer
    oFileHandle = FreeFile
    Open "LPT1:" For Output As #oFileHandle
    Print #oFileHandle, oStr$
    Close #oFileHandle
End Sub



'added: november 17, 1995
'last revised: november 17, 1995
Function WSAGetAsyncBufLen(ByVal lParam As Long) As Long
'used only by the async lookups and wparam will
'be the task handle for these events
'    On Error Resume Next
'    '#define WSAGETASYNCBUFLEN(lParam)           LOWORD(lParam)
'    WSAGetAsyncBufLen = lParam And &HFFFF&
'    If Err Then
'        WSAGetAsyncBufLen = 0
'    End If
End Function

'added: november 17, 1995
'last revised: november 17, 1995
Function WSAGetSelectEvent(ByVal lParam As Long) As Long
'    On Error Resume Next
'    '#define WSAGETSELECTEVENT(lParam)            LOWORD(lParam)
'    WSAGetSelectEvent = (lParam And &HFFFF&)
'    If Err Then
'        WSAGetSelectEvent = 0
'    End If
End Function



'added: november 17, 1995
'last revised: november 17, 1995
Function WSAGetAsyncError(ByVal lParam As Long) As Long
'    On Error Resume Next
'    'WSAGETASYNCERROR(lParam) HIWORD(lParam)
'    WSAGetAsyncError = (lParam \ &H10000 And &HFFFF&)
'    If Err Then
'        WSAGetAsyncError = 0
'    End If
End Function



Function AddrToIP(ByVal AddrOrIP$) As String
'    On Error Resume Next
'    AddrToIP$ = getascip(GetHostByNameAlias(AddrOrIP$))
'    If Err Then AddrToIP$ = "255.255.255.255"
End Function

Function ConnectSock(ByVal host$, ByVal Port%, retIpPort$, HWndToMsg%, Async%) As Integer
'    Dim sockin As sockaddrT
'    Dim s%, SelectOps%, dummy%
'    SockReadBuffer$ = ""
'    sockin = saZero
'    sockin.sin_family = AF_INET
'
'    sockin.sin_port = htons(Port%)
'    If sockin.sin_port = INVALID_PORT Then
'        ConnectSock% = INVALID_SOCKET
'        Exit Function
'    End If
'
'    sockin.sin_addr = GetHostByNameAlias(host$)
'    If sockin.sin_addr = INADDR_NONE Then
'        ConnectSock% = INVALID_SOCKET
'        Exit Function
'    End If
'    retIpPort$ = getascip$(sockin.sin_addr) & ":" & ntohs%(sockin.sin_port)
'
'    s% = Socket%(PF_INET, SOCK_STREAM, IPPROTO_TCP)
'    If s% < 0 Then
'        ConnectSock% = INVALID_SOCKET
'        Exit Function
'    End If
'    If SetSockLinger(s%, 1, 0) = SOCKET_ERROR Then
'        If s% > 0 Then
'            dummy% = closesocket(s%)
'        End If
'        ConnectSock = INVALID_SOCKET
'        Exit Function
'    End If
'    If Not Async% Then
'        If connect%(s%, sockin, sockaddr_size) <> 0 Then
'            If s% > 0 Then
'                dummy = closesocket(s%)
'            End If
'            ConnectSock% = INVALID_SOCKET
'            Exit Function
'        End If
'        SelectOps = FD_READ Or FD_WRITE Or FD_CONNECT Or FD_CLOSE
'        If WSAAsyncSelect(s%, HWndToMsg%, ByVal 1025, ByVal SelectOps) Then
'            If s% > 0 Then
'                dummy% = closesocket(s%)
'            End If
'            ConnectSock = INVALID_SOCKET
'            Exit Function
'        End If
'    Else
'        SelectOps = FD_READ Or FD_WRITE Or FD_CONNECT Or FD_CLOSE
'        If WSAAsyncSelect(s%, HWndToMsg%, ByVal 1025, ByVal SelectOps) Then
'            If s% > 0 Then
'                dummy% = closesocket(s%)
'            End If
'            ConnectSock = INVALID_SOCKET
'            Exit Function
'        End If
'        If connect%(s%, sockin, sockaddr_size) <> -1 Then
'            If s% > 0 Then
'                dummy = closesocket(s%)
'            End If
'            ConnectSock% = INVALID_SOCKET
'            Exit Function
'        End If
'    End If
'    ConnectSock% = s%
End Function

''Function SetSockLinger(ByVal SockNum As Integer, ByVal OnOff As Integer, ByVal LingerTime As Integer) As Integer
''    Dim ret%, ret1%, LingBuf$, Linger As lingerT
''
''    LingBuf$ = Space$(4)
''    Linger.l_onoff = OnOff
''    Linger.l_linger = LingerTime
''
''    MemCopy ByVal LingBuf$, Linger, 4
''
''    ret% = setsockopt(SockNum%, SOL_SOCKET, SO_LINGER, ByVal LingBuf$, 4)
''    Debug.Print "Error Setting Linger info: "; ret% '
''    LingBuf$ = Space$(4)
''    Linger.l_onoff = 0   'reset this so we can get an honest look
''    Linger.l_linger = 0  'reset this so we can get an honest look
''    ret% = getsockopt(SockNum%, SOL_SOCKET, SO_LINGER, LingBuf$, 4)
''
''    MemCopy Linger, ByVal LingBuf$, 4
''
''    Debug.Print "Error Getting Linger info: "; ret%
''    Debug.Print "Linger is on if nonzero: "; Linger.l_onoff
''    Debug.Print "Linger time if linger is on: "; Linger.l_linger
''    SetSockLinger = ret%
''End Function

Sub EndWinsock()
'    Dim ret%
'    If WSAIsBlocking%() Then
'        ret% = WSACancelBlockingCall%()
'    End If
'    ret% = WSACleanup%()
'    WSAStartedUp% = False
End Sub

Function getascip(ByVal inn As Long) As String
'    On Error Resume Next
'    Dim lpStr&
'    Dim nStr%
'    Dim retString$
'    retString$ = String$(32, 0)
'    lpStr& = inet_ntoa&(inn)
'    If lpStr& = 0 Then
'        getascip = "255.255.255.255"
'        Exit Function
'    End If
'    nStr% = lstrlen(lpStr&)
'    If nStr% > 32 Then nStr% = 32
'    MemCopy ByVal retString$, ByVal lpStr&, nStr%
'    retString$ = Left$(retString$, nStr%)
'    getascip = retString$
'    If Err Then getascip = "255.255.255.255"
End Function

Function GetHostByAddress(ByVal addr As Long) As String
'    On Error Resume Next
'    Dim phe&, ret&
'    Dim heDestHost As HostEnt
'    Dim hostname$
'    phe& = gethostbyaddr&(addr, 4, PF_INET)
'    If phe& <> 0 Then
'        MemCopy heDestHost, ByVal phe&, hostent_size
'        hostname$ = String$(256, 0)
'        ret& = lstrcpy(hostname$, heDestHost.h_name)
'        GetHostByAddress = Left$(hostname$, InStr(hostname$, Chr$(0)) - 1)
'    Else
'        GetHostByAddress = WSA_NoName
'    End If
'    If Err Then GetHostByAddress = WSA_NoName
End Function

Function GetHostByNameAlias(hostname$) As Long
'    On Error Resume Next
'    'Return IP address as a long, in network byte order
'
'    Dim phe&    ' pointer to host information entry
'    Dim heDestHost As HostEnt 'hostent structure
'    Dim addrList&
'    Dim retIP&
'    'first check to see if what we have been passed is a valid IP
'    retIP& = inet_addr&(hostname$)
'    If retIP& = INADDR_NONE Then
'        'it wasn't an IP, so do a DNS lookup
'        phe& = gethostbyname&(hostname$)
'        If phe& <> 0 Then
'            'Pointer is non-null, so copy in hostent structure
'            MemCopy heDestHost, ByVal phe&, hostent_size
'            'Now get first pointer in address list
'            MemCopy addrList&, ByVal heDestHost.h_addr_list, 4
'            MemCopy retIP&, ByVal addrList&, heDestHost.h_length
'        Else
            'its not a valid address
'            retIP& = INADDR_NONE
'        End If
'    End If
'    GetHostByNameAlias = retIP&
'    If Err Then GetHostByNameAlias = INADDR_NONE
End Function

Function GetLocalHostName() As String
'    Dim dummy As Integer
'    Dim LocalName As String
'    Dim s As String
'    On Error Resume Next
'    LocalName = String$(256, 0)
'    LocalName = WSA_NoName
'    dummy = 1
'    s = String$(256, 0)
'    dummy = gethostname(s, 256)
'    If dummy = 0 Then
'        s = Left$(s, InStr(s, Chr$(0)) - 1)
'        If Len(s) > 0 Then
'            LocalName = s
'        End If
'    End If
'    GetLocalHostName = LocalName
'    If Err Then GetLocalHostName = WSA_NoName
End Function

Function GetPeerAddress(ByVal s As Integer) As String
'    On Error Resume Next
'    Dim sa As sockaddr
'    Dim addrlen%
'    Dim ret%
'    addrlen% = sockaddr_size
'    ret% = getpeername%(s, sa, addrlen%)
'    If ret% = 0 Then
'        GetPeerAddress = SockaddressToString$(sa)
'    Else
'        GetPeerAddress = ""
'    End If
'    If Err Then GetPeerAddress = ""
End Function

Function GetPortFromString(ByVal PortStr$) As Integer
    'sometimes users provide ports outside the range of a VB
    'integer, so this function returns an integer for a string
    'just to keep an error from happening, it converts the
    'number to a negative if needed
'    On Error Resume Next
'    If Val(PortStr$) > 32767 Then
'        GetPortFromString = CInt(Val(PortStr$) - &H10000)
'    Else
'        GetPortFromString = Val(PortStr$)
'    End If
'    If Err Then GetPortFromString = 0
End Function

'I can not vouche for this function, I have never used it
'and never plan to, just left it here for compatibility
Function GetProtocolByName(protocol$) As Integer
'    On Error Resume Next
'    Dim ppe&
'    Dim peDestProt As protoent
'    Dim tmpShort%
'    ppe& = getprotobyname&(protocol$)
'    If ppe& = 0 Then
'        tmpShort% = Val(protocol$)
'        If tmpShort% <> 0 Or protocol$ = "0" Or protocol = "" Then
'            GetProtocolByName = htons%(tmpShort%)
'        Else
'            GetProtocolByName = INVALID_PROTO
'        End If
'    Else
'        MemCopy peDestProt, ByVal ppe&, protoent_size
'        GetProtocolByName = peDestProt.p_proto
'    End If
'    If Err Then GetProtocolByName = INVALID_PROTO
End Function

'I can not vouche for this function, I have never used it
'and never plan to
Function GetServiceByName(service$, protocol$) As Integer
'    On Error Resume Next
'    Dim pse&
'    Dim seDestServ As servent
'    Dim serv%
'    pse& = getservbyname&(service$, protocol$)
'    If pse& <> 0 Then
'        MemCopy seDestServ, ByVal pse&, servent_size
'        GetServiceByName = seDestServ.s_port
'    Else
'        serv% = Val(service$)
'        If serv% <> 0 Then
'            GetServiceByName = htons%(serv%)
'        Else
'            GetServiceByName = INVALID_PORT
'        End If
'    End If
'    If Err Then GetServiceByName = INVALID_PORT
End Function

Function GetSockAddress(ByVal s As Integer) As String
'    On Error Resume Next
'    Dim sa As sockaddr
'    Dim szRet$
'    Dim addrlen%
'    Dim ret%
'    szRet$ = String$(32, 0)
'    addrlen% = sockaddr_size
'    ret% = getsockname%(s, sa, addrlen%)
'    If ret% = 0 Then
'        GetSockAddress = SockaddressToString$(sa)
'    Else
'        GetSockAddress = ""
'    End If
'    If Err Then GetSockAddress = ""
End Function

Function GetWSAErrorString(errnum As Integer) As String
    On Error Resume Next
    Select Case errnum
        Case 10004: GetWSAErrorString = "Interrupted system call."
        Case 10009: GetWSAErrorString = "Bad file number."
        Case 10013: GetWSAErrorString = "Permission Denied."
        Case 10014: GetWSAErrorString = "Bad Address."
        Case 10022: GetWSAErrorString = "Invalid Argument."
        Case 10024: GetWSAErrorString = "Too many open files."
        Case 10035: GetWSAErrorString = "Operation would block."
        Case 10036: GetWSAErrorString = "Operation now in progress."
        Case 10037: GetWSAErrorString = "Operation already in progress."
        Case 10038: GetWSAErrorString = "Socket operation on nonsocket."
        Case 10039: GetWSAErrorString = "Destination address required."
        Case 10040: GetWSAErrorString = "Message too long."
        Case 10041: GetWSAErrorString = "Protocol wrong type for socket."
        Case 10042: GetWSAErrorString = "Protocol not available."
        Case 10043: GetWSAErrorString = "Protocol not supported."
        Case 10044: GetWSAErrorString = "Socket type not supported."
        Case 10045: GetWSAErrorString = "Operation not supported on socket."
        Case 10046: GetWSAErrorString = "Protocol family not supported."
        Case 10047: GetWSAErrorString = "Address family not supported by protocol family."
        Case 10048: GetWSAErrorString = "Address already in use."
        Case 10049: GetWSAErrorString = "Can't assign requested address."
        Case 10050: GetWSAErrorString = "Network is down."
        Case 10051: GetWSAErrorString = "Network is unreachable."
        Case 10052: GetWSAErrorString = "Network dropped connection."
        Case 10053: GetWSAErrorString = "Software caused connection abort."
        Case 10054: GetWSAErrorString = "Connection reset by peer."
        Case 10055: GetWSAErrorString = "No buffer space available."
        Case 10056: GetWSAErrorString = "Socket is already connected."
        Case 10057: GetWSAErrorString = "Socket is not connected."
        Case 10058: GetWSAErrorString = "Can't send after socket shutdown."
        Case 10059: GetWSAErrorString = "Too many references: can't splice."
        Case 10060: GetWSAErrorString = "Connection timed out."
        Case 10061: GetWSAErrorString = "Connection refused."
        Case 10062: GetWSAErrorString = "Too many levels of symbolic links."
        Case 10063: GetWSAErrorString = "File name too long."
        Case 10064: GetWSAErrorString = "Host is down."
        Case 10065: GetWSAErrorString = "No route to host."
        Case 10066: GetWSAErrorString = "Directory not empty."
        Case 10067: GetWSAErrorString = "Too many processes."
        Case 10068: GetWSAErrorString = "Too many users."
        Case 10069: GetWSAErrorString = "Disk quota exceeded."
        Case 10070: GetWSAErrorString = "Stale NFS file handle."
        Case 10071: GetWSAErrorString = "Too many levels of remote in path."
        Case 10091: GetWSAErrorString = "Network subsystem is unusable."
        Case 10092: GetWSAErrorString = "Winsock DLL cannot support this application."
        Case 10093: GetWSAErrorString = "Winsock not initialized."
        Case 10101: GetWSAErrorString = "Disconnect."
        Case 11001: GetWSAErrorString = "Host not found."
        Case 11002: GetWSAErrorString = "Nonauthoritative host not found."
        Case 11003: GetWSAErrorString = "Nonrecoverable error."
        Case 11004: GetWSAErrorString = "Valid name, no data record of requested type."
        Case Else:
    End Select
End Function

Function IpToAddr(ByVal AddrOrIP$) As String
'    On Error Resume Next
'    IpToAddr = GetHostByAddress(GetHostByNameAlias(AddrOrIP$))
'    If Err Then IpToAddr = WSA_NoName
End Function

Function IrcGetAscIp(ByVal IPL$) As String
'    'this function is IRC specific, it expects a long ip stored in Network byte order, in a string
'    'the kind that would be parsed out of a DCC command string
'    On Error GoTo IrcGetAscIPError:
'    Dim lpStr&
'    Dim nStr%
'    Dim retString$
'    Dim inn&
'    If Val(IPL$) > 2147483647 Then
'        inn& = Val(IPL$) - 4294967296#
'    Else
'        inn& = Val(IPL$)'
'    End If
'    inn& = ntohl(inn&)
'    retString$ = String$(32, 0)
'    lpStr& = inet_ntoa&(inn)
'    If lpStr& = 0 Then
'        IrcGetAscIp = "0.0.0.0"
'        Exit Function
'    End If
'    nStr% = lstrlen(lpStr&)
'    If nStr% > 32 Then nStr% = 32
'    MemCopy ByVal retString$, ByVal lpStr&, nStr%
'    retString$ = Left$(retString$, nStr%)
'    IrcGetAscIp = retString$
'    Exit Function
'IrcGetAscIPError:
'    IrcGetAscIp = "0.0.0.0"
'    Exit Function
'    Resume
End Function

Function IrcGetLongIp(ByVal AscIp$) As String
'    'this function converts an ascii ip string into a long ip in network byte order
'    'and stick it in a string suitable for use in a DCC command.
'    On Error GoTo IrcGetLongIpError:
'    Dim inn&
'    inn& = inet_addr&(AscIp$)
'    inn& = htonl(inn&)
'    If inn& < 0 Then
'        IrcGetLongIp = CVar(inn& + 4294967296#)
'        Exit Function
'    Else
'        IrcGetLongIp = CVar(inn&)
'        Exit Function
'    End If
'    Exit Function
'IrcGetLongIpError:
'    IrcGetLongIp = "0"
'    Exit Function
'    Resume
End Function

Function ListenForConnect(Port%, HWndToMsg%) As Integer
'    Dim sockin As sockaddr
'    Dim s%, dummy%
'    Dim SelectOps As Integer
'
'    sockin = saZero     'zero out the structure
'    sockin.sin_family = AF_INET
'    sockin.sin_port = htons(Port%)
'    If sockin.sin_port = INVALID_PORT Then
'        ListenForConnect = INVALID_SOCKET
'        Exit Function
'    End If
'    sockin.sin_addr = htonl(INADDR_ANY)
'    If sockin.sin_addr = INADDR_NONE Then
'        ListenForConnect = INVALID_SOCKET
'        Exit Function
'    End If
'    s% = Socket%(PF_INET, SOCK_STREAM, 0)
'    If s% < 0 Then
'        ListenForConnect = INVALID_SOCKET
'        Exit Function
'    End If
''    If SetSockLinger(s%, 1, 0) = SOCKET_ERROR Then
''        If s% > 0 Then
''            dummy% = closesocket(s%)
''        End If
''        ListenForConnect = INVALID_SOCKET
''        Exit Function
''    End If
'
'    If bind(s%, sockin, sockaddr_size) Then
'        If s% > 0 Then
'            dummy = closesocket(s%)
'        End If
'        ListenForConnect = INVALID_SOCKET
'        Exit Function
'    End If
'    SelectOps = FD_READ Or FD_WRITE Or FD_CLOSE Or FD_ACCEPT
'    If WSAAsyncSelect(s%, HWndToMsg%, ByVal 1025, ByVal SelectOps) Then
'        If s% > 0 Then
'            dummy = closesocket(s%)
'        End If
'        ListenForConnect = SOCKET_ERROR
'        Exit Function
'    End If
'
'    If listen(s%, 1) Then
'        If s% > 0 Then
'            dummy = closesocket(s%)
'        End If
'        ListenForConnect = INVALID_SOCKET
'        Exit Function
'    End If
'    ListenForConnect = s%
End Function

Function SendData(ByVal s As Integer, ByVal Message As String)
    SendData = Send(s, ByVal Message, Len(Message), 0)
End Function

Function SockaddressToString(sa As sockaddrT) As String
'    On Error Resume Next
'    SockaddressToString = getascip(sa.sin_addr) & ":" & ntohs%(sa.sin_port)
'    If Err Then SockaddressToString = ""
End Function

Function StartWinsock%(desc$)
'    Dim ret%
'    Dim WinsockVers%
'    Dim wsadStartupData As WSADataType
'    WinsockVers = &H101   'Vers 1.1

'    If WSAStartedUp% = False Then
'        ret% = 1
'        ret% = WSAStartup%(WinsockVers, wsadStartupData)
'        If ret% = 0 Then
'            WSAStartedUp% = True
'            Debug.Print "wVersion="; VBntoaVers$(wsadStartupData.wVersion), "wHighVersion="; VBntoaVers$(wsadStartupData.wHighVersion)
'            Debug.Print "szDescription="; wsadStartupData.szDescription
'            Debug.Print "szSystemStatus="; wsadStartupData.szSystemStatus
'            Debug.Print "iMaxSockets="; wsadStartupData.iMaxSockets, "iMaxUdpDg="; wsadStartupData.iMaxUdpDg
'            desc$ = wsadStartupData.szDescription
'        Else
'            WSAStartedUp = False
'        End If
'    End If
'    StartWinsock% = WSAStartedUp%
End Function

Function VBntoaVers$(ByVal vers As Integer)
'    On Error Resume Next
'    Dim szVers$
'    szVers$ = String$(5, 0)
'    szVers$ = (vers And &HFF) & "." & ((vers And &HFF00) / 256)
'    VBntoaVers$ = szVers$
End Function

'this function uses MemCopy to transfer data from
'2 integers into 2 strings, then combines the strings
'and copys that data into a long, ineffect MAKELONG()
Function WSAMakeSelectReply(ByVal TheEvent%, ByVal TheError%) As Long
'    Dim EventStr$, ErrorStr$, BothStr$, TheLong&
'    EventStr$ = Space$(2)
'    ErrorStr$ = Space$(2)
'    BothStr$ = Space$(4)
'    MemCopy ByVal EventStr$, TheEvent%, 2
'    MemCopy ByVal ErrorStr$, TheError%, 2
'    BothStr$ = EventStr$ & ErrorStr$
'    If Len(BothStr$) = 4 Then
'        MemCopy TheLong&, ByVal BothStr$, 4
'    End If
'    WSAMakeSelectReply = TheLong&
End Function

Public Sub FD_CLR(fd As Long, fdset As fd_setT)
    Dim i As Long
    For i = 0 To fdset.fd_count
        If fdset.fd_array(i) = fd Then
            While (i < (fdset.fd_count - 1))
                fdset.fd_array(i) = fdset.fd_array(i + 1)
            Wend
            fdset.fd_count = fdset.fd_count - 1
            Exit For
        End If
    Next i
End Sub

Public Sub FD_SET(fd As Long, fdset As fd_setT)
    If fdset.fd_count < FD_SETSIZE Then
        fdset.fd_array(fdset.fd_count + 1) = fd
        fdset.fd_count = fdset.fd_count + 1
    End If
End Sub


Public Sub FD_ZERO(fdset As fd_setT)
    fdset.fd_count = 0
End Sub

Public Function VBgethostname() As String
Dim tempstr As String
Dim strlen As Integer, retval As Integer
    strlen = 128
    tempstr = Space$(strlen)
    retval = gethostname(tempstr, strlen)
    If retval = 0 Then
        VBgethostname = Trim$(tempstr)
    Else
        VBgethostname = "get_hostname: ERROR"
    End If
    tempstr = ""
End Function


Public Function VBInitializeWinsock(ShowMsgOnFail As Boolean) As Boolean
    Dim iErr%
    iErr% = WSAStartup(VER_REQUIRED, WSAdata)
    If iErr% <> 0 Then
        If ShowMsgOnFail = True Then MsgBox ("WSAStartup: Could not initialize Windows Sockets")
        WSACleanup
        VBInitializeWinsock = False
        Exit Function
    End If
    VBInitializeWinsock = True
End Function



Public Function VBInitListenSocket(myWnd As Integer, wsPort As Integer) As Integer
'' This function attempts to initialize a listening socket
'' returning the socket handle if successful
'' or INVALID_SOCKET on an error
'' The caller must supply a handle to a window for messaging
'' purposes. THIS SHOULD BE THE HANDLE TO THE WMUSER.OCX WINDOW OTHERWISE YOU WILL
'' NEVER GET THE MESSAGE FROM THE WINDOWS MESSAGE QUE!!
''
''Const INVALID_SOCKET = -1
''Const SOCK_ERROR = -1
Dim hListenSocket As Long '' The listening socket
Dim iError As Integer        '' Used for function return values
'' Lets get a socket and check for an error
hListenSocket = Socket(AF_INET, SOCK_STREAM, 0)
If hListenSocket = INVALID_SOCKET Then
    '' Error, no sense going any further
    VBInitListenSocket = INVALID_SOCKET
    Exit Function
Else
    '' Success, lets continue on
    iError = WSAAsyncSelect(hListenSocket, myWnd, INCOMING_MSG, FD_ACCEPT)
    If iError = SOCK_ERROR Then
        '' Error, no sense going any farther, lets close the socket and exit
        iError = closesocket(hListenSocket)
        VBInitListenSocket = SOCK_ERROR
        Exit Function
    Else
        '' Lets set up the socket params and bind it
        sockaddr_in.sin_family = AF_INET
        sockaddr_in.sin_port = htons(wsPort)
        sockaddr_in.sin_addr = 0&
        sockaddr_in.sin_zero = " "
        iError = bind(hListenSocket, sockaddr_in, Len(sockaddr_in))
        If iError = SOCK_ERROR Then
            '' Error, close socket and leave
            iError = closesocket(hListenSocket)
            VBInitListenSocket = SOCKET_ERROR
            Exit Function
        Else
            '' Set up socket to listen
            iError = listen(hListenSocket, 5)
            If iError = SOCK_ERROR Then
                '' Error, close socket and leave
                iError = closesocket(hListenSocket)
                VBInitListenSocket = SOCK_ERROR
                Exit Function
              End If
        End If
    End If
    VBInitListenSocket = hListenSocket
End If
End Function


Public Function FD_ISSET(fd As Long, fdset As fd_setT) As Long
    FD_ISSET = WSAFDIsSet(fd, fdset)
End Function



