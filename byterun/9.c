#include "config.h"

#ifdef OS_PLAN9

//TODO: emulate via errstr?
int errno = -1;
char * strerror(int n)      { return "strerror: 9 ERROR"; }

int unix_open(char* path, int _flags, int perm) {
    // flags
    return open (path, perm);
}

// thx chatGPT
static ulong
plan9mode_to_unixmode(Dir *d)
{
    ulong m = 0;

    /* file type */
    if(d->mode & DMDIR)
        m |= S_IFDIR;
    else
        m |= S_IFREG;
//    if(d->mode & DMDEVICE)
//        m = (m & ~S_IFMT) | S_IFCHR;
    /* permissions */
//    if(d->mode & DMREAD){
//        m |= S_IRUSR | S_IRGRP | S_IROTH;
//    }
//    if(d->mode & DMWRITE){
//        m |= S_IWUSR | S_IWGRP | S_IWOTH;
//    }
//    if(d->mode & DMEXEC){
//        m |= S_IXUSR | S_IXGRP | S_IXOTH;
//    }
    return m;
}
int unix_stat(char* path, struct unix_stat* st) {
    Dir *d;
    d = dirstat(path);
    if (d == nil) 
        return -1;
    // else
    memset(st, 0, sizeof(*st));
    st->st_mode  = plan9mode_to_unixmode(d);
    st->st_size  = d->length;
    st->st_atime = d->atime;
    st->st_mtime = d->mtime;
    st->st_ctime = d->mtime;     /* Plan 9 has no ctime */
    st->st_ino   = d->qid.path;  /* closest thing to inode */
    st->st_nlink = 1;            /* Plan 9 has no hard links */
    st->st_uid   = 0;            /* no numeric UID */
    st->st_gid   = 0;            /* no numeric GID */
    free(d);
    return 0;
}

void posix_signal(int x, void (*)()) {
    print("TODO: posix_signal(%d)", x);
    //TODO:
}


int unlink(char*)           { return -1;}
int rename(char*, char*)    { return -1; }
int getcwd(char*, int)      { return 0; }
int system(char*)           { return -1; }
int sscanf(const char *, const char *, ...) { return -1; }

#endif
