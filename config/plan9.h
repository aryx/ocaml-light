//!! If you modify this file, copy it to s.h before recompiling!!

// this new type required to modify stdlib/filename.ml and stdlib/lexing.ml
// to pattern match with | "Unix" | "Plan9" -> ...
#define OCAML_OS_TYPE "Plan9"

// undo what is in m.h
#undef ARCH_SIXTYFOUR
// disable gcc extensions
#undef __GNUC__

// if you don't want to use APE, uncomment the following:
//#define OS_PLAN9
// if you want to use APE, uncomment the following instead:
//#define OS_PLAN9_APE

//*************************************************************************
// Regular (no APE) config
//*************************************************************************
#ifdef OS_PLAN9

#define NULL 0

// we rely on plan9 libc, not unix libc
#include <u.h>
#include <libc.h>

// Adapt Unix libc functions to Plan9 libc, the easy one via macros
// For the more complex one like open() and stat(), see sys.c
// unix_open and unix_stat in OS_PLAN9 ifdef
#define bcopy(src,dst,len) memmove((dst), (src), (len))
#define sprintf sprint
#define fprintf fprint

#define stderr STDERR

#define fflush(x) USED(x)
#define exit(x) _exits(0)

#define lseek(a,b,c) seek(a,b,c)
#define SEEK_END SEEK__END
#define SEEK_SET SEEK__START

#define INT_MAX		0x7fffffff
#define LONG_MAX	0x7fffffffL

#define EINTR -1
#define EAGAIN -1

#define O_RDONLY OREAD
#define O_WRONLY OWRITE
#define O_TRUNC OTRUNC
#define O_EXCL OEXCL
//TODO
#define O_CREAT -1
#define O_APPEND -1
#define O_NONBLOCK -1

typedef unsigned long size_t;

//origin: chatGPT "I am in plan9 code and want to emulate the unix stat function; 
// write unix_stat() that internally calls plan9 dirstat"
struct unix_stat {
    ulong  st_mode;
    uvlong st_size;
    ulong  st_atime;
    ulong  st_mtime;
    ulong  st_ctime;
    uvlong st_ino;
    ulong  st_nlink;
    ulong  st_uid;
    ulong  st_gid;
};
/* Unix mode bits */
// those seems to be the only one needed in sys.c
#define S_IFMT   0170000
#define S_IFDIR  0040000
#define S_IFREG  0100000
//#define S_IFCHR  0020000
//#define S_IRUSR  00400
//#define S_IWUSR  00200
//#define S_IXUSR  00100
//#define S_IRGRP  00040
//#define S_IWGRP  00020
//#define S_IXGRP  00010
//#define S_IROTH  00004
//#define S_IWOTH  00002
//#define S_IXOTH  00001

//TODO:
#undef POSIX_SIGNALS
enum {
    SIGINT  = 2,
    SIGQUIT = 3,
    SIGKILL = 9,
    SIGALRM = 14,
    SIGTERM = 15,
};
//TODO, again see chatGPT to get first draft for those constants
// and functions
//void signal(int, void (*)());
#define SIG_DFL ((void (*)())0)
#define SIG_ERR ((void (*)())-1)
#define SIG_IGN ((void (*)())1)

// for io.c, and defined in sys.c OS_PLAN9 ifdef
extern int errno;

#undef HAS_TERMCAP
#undef HAS_SOCKETS
#undef HAS_UNISTD

#undef HAS_DIRENT
#undef HAS_REWINDDIR
#undef HAS_LOCKF
#undef HAS_MKFIFO
#undef HAS_GETCWD
#undef HAS_GETWD
#undef HAS_GETPRIORITY
#undef HAS_UTIME
#undef HAS_UTIMES
#undef HAS_DUP2
#undef HAS_FCHMOD
#undef HAS_TRUNCATE
#undef HAS_SELECT
#undef HAS_SYS_SELECT_H
#undef HAS_SYMLINK
#undef HAS_WAITPID
#undef HAS_WAIT4
#undef HAS_GETGROUPS
#undef HAS_TERMIOS
#undef HAS_ASYNC_IO
#undef HAS_SETITIMER
#undef HAS_GETHOSTNAME
#undef HAS_UNAME
#undef HAS_GETTIMEOFDAY
#undef HAS_MKTIME
#undef HAS_SETSID

#endif // OS_PLAN9


//*************************************************************************
// APE config
//*************************************************************************
#ifdef OS_PLAN9_APE

#undef HAS_TERMCAP
//todo: replace by something plan9 offer? in APE? alarm?
#undef HAS_SETITIMER

#define HAS_UNISTD
#define HAS_SOCKETS
#define HAS_DIRENT
#define HAS_TRUNCATE
#define HAS_GETCWD
#define HAS_GETTIMEOFDAY
#define HAS_MKTIME
// actually not really but ape libbsd.a provide one returning an error
#define HAS_SYMLINK
#define HAS_REWINDDIR
#define HAS_SELECT
#define HAS_UTIMES
#define HAS_WAITPID

#endif // OS_PLAN9_APE
