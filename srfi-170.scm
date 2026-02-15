;;
;; Implementation of the SRFI-170 spec
;; Some of these functions exist in one form or another in other eggs.
;; In these cases it's hard to decide whether to reimplement from scratch
;; or to piggy back off them. In most (but not all) cases I've take tne
;; approach to reimplement from scratch, partly on the assumption that those
;; other eggs might want to reliquish resonsibility for them for the standard,
;; partly because the spec has specific error reporting, which they may
;; not implement.
;; It's an open question whether this spec has a better interface than
;; some of those other specs. I think at least some of them are better here.
;; For example, the option to use open-directory to control the open/close
;; lifecycle, and file-info which has named getters.
;;
(module srfi-170 (
				  posix-error?
				  posix-error-name
				  posix-error-number
				  posix-error-message
				  posix-error-procedure
				  posix-error-arguments
										;				  make-time
										;binary-input
										; textual-input
				  ;binary-output
										;textual-output
										;binary-input/output
										;buffer-none
										;buffer-block
										; buffer-line
										;				  open-file
										; fd->port
				  create-directory
				  create-fifo
				  create-hard-link
				  create-symlink
				  read-symlink
				  rename-file
				  delete-directory
				  set-file-owner
				  set-file-times
				  truncate-file
				  file-info
				  file-info?
				  file-info:device
				  file-info:inode
				  file-info:mode
				  file-info:nlinks
				  file-info:uid
				  file-info:gid
				  file-info:rdev
				  file-info:size
				  file-info:blksize
				  file-info:blocks
				  file-info:atime
				  file-info:mtime
				  file-info:ctime
				  file-info-directory?
				  file-info-fifo?
				  file-info-symlink?
				  file-info-regular?
				  file-info-socket?
				  file-info-device?
				  set-file-mode
				  directory-files
				  make-directory-files-generator
				  open-directory
				  read-directory-entry
				  dirent-name
				  dirent-ino
				  read-directory
				  close-directory
				  real-path
				  file-system-info
				  file-system-info?
				  file-system-info:block-size
				  file-system-info:fragment-size
				  file-system-info:number-of-fragments
				  file-system-info:blocks-free
				  file-system-info:blocks-available
				  file-system-info:inode-number
				  file-system-info:inodes-free
				  file-system-info:inodes-available
				  file-system-info:id
				  file-system-info:name-max
				  file-space
										;temp-file-prefix
										;				  create-temp-file
										;				  call-with-temporary-filename
				  umask
				  set-umask!
				  current-directory
				  set-current-directory!
				  pid
				  nice
				  user-uid
				  user-gid
				  user-effective-uid
				  user-effective-gid
				  user-supplementary-gids
				  user-info
				  user-info?
				  user-info:name
				  user-info:password
				  user-info:uid
				  user-info:gid
				  user-info:home-dir
				  user-info:shell
				  user-info:full-name
				  user-info:parsed-full-name
				  group-info
				  group-info?
				  group-info:name
				  group-info:password
				  group-info:gid
				  group-info:members
				  posix-time
				  monotonic-time
				  delete-environment-variable!
				  terminal?)
  (import scheme)
  (import srfi-1)
  (import srfi-9)
  (import srfi-13)
  (import (only srfi-19 make-time time-utc time-second time-nanosecond))
  (import srfi-121)
  (import (only (chicken file posix) port->fileno))
;  (import (only (chicken file) user-information))
  (import (chicken foreign))
  (import (chicken gc))
  (import (chicken pathname))
  (import (chicken process-context))
  (import (chicken process-context posix))
  (import posix-groups)
  (import (chicken memory))
  (import (chicken port))
  (import (chicken base))
  (import (chicken condition))
  (import (chicken time posix))
  (import (chicken blob))
  (import (chicken bitwise))
  (import (chicken string))    ; for string-split
  (import (chicken irregex))   ; for string-substitute

  (define c-error-name
	(foreign-lambda* c-string ((int err))
      "switch(err) {
      case EACCES:       C_return(\"EACCES\");
      case EIO:          C_return(\"EIO\");
      case ELOOP:        C_return(\"ELOOP\");
      case ENAMETOOLONG: C_return(\"ENAMETOOLONG\");
      case ENOENT:       C_return(\"ENOENT\");
      case ENOTDIR:      C_return(\"ENOTDIR\");
      case ENOMEM:       C_return(\"ENOMEM\");
      default:           C_return(\"UNKNOWN\");
    }"))

  (define errno 
	(foreign-lambda* int () "C_return(errno);"))

  (define set-errno!
	(foreign-lambda* void ((int v))
     "
      errno = v;
     "))

  
  (define c-strerror (foreign-lambda c-string "strerror" int))

  (define (raise-posix-error loc . args)
	(let* ((err-num (errno))
           (err-name (c-error-name err-num))
           (err-msg  (c-strerror err-num)))
      (abort
       (make-composite-condition
		;; We pass 'args' directly to the 'arguments slot as it is already a list
		(make-property-condition 'exn 
								 'message err-msg 
								 'location loc 
								 'arguments args)
		(make-property-condition 'i/o-error)
		(make-property-condition 'file-error)
		(make-property-condition 'posix-error 
								 'errno err-num 
								 'name err-name)))))
  
  (define (posix-error? c)
	((condition-predicate 'posix-error) c))

  (define (posix-error-name err)
	(if (and (condition? err) 
			 ((condition-predicate 'posix-error) err))
		(get-condition-property err 'posix-error 'name)
		(error "Object is not a posix-error condition" err)))
  
  (define (posix-error-number err)
	(if (and (condition? err) 
			 ((condition-predicate 'posix-error) err))
		(get-condition-property err 'posix-error 'errno)
		(error "Object is not a posix-error condition" err)))

  (define (posix-error-message err)
	(if (and (condition? err) 
			 ((condition-predicate 'posix-error) err))
		(get-condition-property err 'posix-error 'message)
		(error "Object is not a posix-error condition" err)))

  (define (posix-error-procedure err)
	(if (and (condition? err) 
			 ((condition-predicate 'posix-error) err))
		(get-condition-property err 'posix-error 'location)
		(error "Object is not a posix-error condition" err)))

  (define (posix-error-arguments err)
	(if (and (condition? err) 
			 ((condition-predicate 'posix-error) err))
		(get-condition-property err 'posix-error 'arguments)
		(error "Object is not a posix-error condition" err)))


  (foreign-declare "#include <dirent.h>")
  (foreign-declare "#include <sys/types.h>")
  (foreign-declare "#include <sys/stat.h>")
  (foreign-declare "#include <sys/fcntl.h>")
  
  ;; Define DIR* type for readability
  (define-foreign-type DIR* (c-pointer "DIR"))
  (define-foreign-type dirent* (c-pointer "struct dirent"))
  (define-foreign-type mode-t unsigned-int)
  (define-foreign-type uid-t unsigned-int)
  (define-foreign-type gid-t unsigned-int)
  (define-foreign-type off-t int)

  (define (create-directory fname #!optional (permission-bits #o775))
	(let ((res ((foreign-lambda int "mkdir" c-string mode-t) fname permission-bits)))
	  (if (< res 0)
		  (raise-posix-error 'create-directory fname))))
	  
  (define (create-fifo fname #!optional (permission-bits #o664))
	(let ((res ((foreign-lambda int "mkfifo" c-string mode-t) fname permission-bits)))
	  (if (< res 0)
		  (raise-posix-error 'create-fifo fname))))

  (define (create-hard-link old-fname new-fname)
	(let ((res ((foreign-lambda int "link" c-string c-string) old-fname new-fname)))
	  (if (< res 0)
		  (raise-posix-error 'create-hard-link old-fname new-fname))))

  (define (create-symlink old-fname new-fname)
	(let ((res ((foreign-lambda int "symlink" c-string c-string) old-fname new-fname)))
	  (if (< res 0)
		  (raise-posix-error 'create-symlink old-fname new-fname))))

  (define (read-symlink path)
	(let* ((c-readlink
			(foreign-lambda* c-string* ((c-string path))
			  "
    size_t sz = 1024;
    char * buf = malloc(sz+1);
    if (!buf) C_return(NULL);
    ssize_t len = readlink(path, buf, sz+1);
    while (len > (ssize_t)sz) {
       sz *= 2;
       char * tmp = realloc(buf, sz+1);
       if (!tmp) {
           free(buf);
           C_return(NULL);
       }
       buf = tmp;
       len = readlink(path, buf, sz+1);
    }
    if (0 <= len) {
       buf[len] = '\\0'; // readlink does not append a null byte
       C_return(buf);
    }
    free(buf);
    C_return(NULL);
       "))
		   (ptr (c-readlink path)))
	  (if ptr
		  ptr
		  (raise-posix-error 'read-symlink path))))

  (define (rename-file old-fname new-fname)
	(let ((res ((foreign-lambda int "rename" c-string c-string) old-fname new-fname)))
	  (if (< res 0)
		  (raise-posix-error 'rename-file old-fname new-fname))))

  (define (delete-directory fname)
	(let ((res ((foreign-lambda int "rmdir" c-string) fname)))
	  (if (< res 0)
		  (raise-posix-error 'delete-directory fname))))

  (define (set-file-owner fname uid gid)
	(let ((res ((foreign-lambda int "chown" c-string uid-t gid-t) fname uid gid)))
	  (if (< res 0)
		  (raise-posix-error 'set-file-owner fname))))

  
  (foreign-declare "#include <sys/stat.h>")
  (foreign-declare "#include <fcntl.h>")
  (foreign-declare "#include <sys/time.h>")
  
  ;; int futimens(int fd, const struct timespec times[2]);
  (define %futimens
	(foreign-lambda int "futimens" int c-pointer))
  
  ;; Keep %utimensat for path-based calls
  (define %utimensat
	(foreign-lambda int "utimensat" int c-string c-pointer int))

  (define AT_FDCWD            (foreign-value "AT_FDCWD" int))
  (define AT_SYMLINK_NOFOLLOW (foreign-value "AT_SYMLINK_NOFOLLOW" int))

  (define (set-file-times fname/port atime mtime #!optional (follow? #t))
  (let* ((ts-size (foreign-value "sizeof(struct timespec)" int))
         (buf     (make-blob (* 2 ts-size)))
         (flags   (if follow? 0 AT_SYMLINK_NOFOLLOW)))

    (define (pack-srfi19-time! t offset)
      (let ((sec  (time-second t))
            (nsec (time-nanosecond t)))
        ((foreign-lambda* void ((scheme-object b) (int off) (long s) (long ns))
           "struct timespec *ts = (struct timespec *)(C_data_pointer(b) + off);
            ts->tv_sec = (time_t)s;
            ts->tv_nsec = (long)ns;") 
         buf offset sec nsec)))

    (pack-srfi19-time! atime 0)
    (pack-srfi19-time! mtime ts-size)

    (let ((status (if (port? fname/port)
                      (%futimens (port->fileno fname/port) (location buf))
                      (%utimensat AT_FDCWD fname/port (location buf) flags))))
      (if (not (zero? status))
		  (raise-posix-error 'set-file-times fname/port)))))
  

  (foreign-declare "#include <sys/stat.h>")

  (foreign-declare "
#include <sys/stat.h>
#include <unistd.h>

/* Use token pasting (##) to handle the 'a', 'm', and 'c' prefixes 
   for both BSD/Apple and Linux/POSIX naming conventions. */
#if defined(__APPLE__) || defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)
  #define ST_NSEC(s, prefix) ((s)->st_ ## prefix ## timespec.tv_nsec)
#else
  #define ST_NSEC(s, prefix) ((s)->st_ ## prefix ## tim.tv_nsec)
#endif
")
  
  (define-record-type <file-info>
	(make-file-info
	 device inode mode nlinks uid gid rdev
	 size atime mtime ctime blksize blocks)
	file-info?
	(device  file-info:device)
	(inode   file-info:inode)
	(mode    file-info:mode)
	(nlinks  file-info:nlinks)
	(uid     file-info:uid)
	(gid     file-info:gid)
	(rdev    file-info:rdev)
	(size    file-info:size)
	(atime   file-info:atime)
	(mtime   file-info:mtime)
	(ctime   file-info:ctime)
	(blksize file-info:blksize)
	(blocks  file-info:blocks))
  
  (define %stat  (foreign-lambda int "stat" c-string c-pointer))
  (define %lstat (foreign-lambda int "lstat" c-string c-pointer))
  (define %fstat (foreign-lambda int "fstat" int c-pointer))
  
  (define get-dev     (foreign-lambda* unsigned-integer64 ((scheme-object p)) "C_return(((struct stat *)C_data_pointer(p))->st_dev);"))
  (define get-ino     (foreign-lambda* unsigned-integer64 ((scheme-object p)) "C_return(((struct stat *)C_data_pointer(p))->st_ino);"))
  (define get-mode    (foreign-lambda* unsigned-int       ((scheme-object p)) "C_return(((struct stat *)C_data_pointer(p))->st_mode);"))
  (define get-nlink   (foreign-lambda* unsigned-integer64 ((scheme-object p)) "C_return(((struct stat *)C_data_pointer(p))->st_nlink);"))
  (define get-uid     (foreign-lambda* unsigned-int       ((scheme-object p)) "C_return(((struct stat *)C_data_pointer(p))->st_uid);"))
  (define get-gid     (foreign-lambda* unsigned-int       ((scheme-object p)) "C_return(((struct stat *)C_data_pointer(p))->st_gid);"))
  (define get-rdev    (foreign-lambda* unsigned-integer64 ((scheme-object p)) "C_return(((struct stat *)C_data_pointer(p))->st_rdev);"))
  (define get-size    (foreign-lambda* integer64          ((scheme-object p)) "C_return(((struct stat *)C_data_pointer(p))->st_size);"))
  (define get-blksize (foreign-lambda* unsigned-integer64 ((scheme-object p)) "C_return(((struct stat *)C_data_pointer(p))->st_blksize);"))
  (define get-blocks  (foreign-lambda* unsigned-integer64 ((scheme-object p)) "C_return(((struct stat *)C_data_pointer(p))->st_blocks);"))
  
  ;; Basic seconds getters
  (define get-atime-sec (foreign-lambda* integer64 ((scheme-object p)) "C_return(((struct stat *)C_data_pointer(p))->st_atime);"))
  (define get-mtime-sec (foreign-lambda* integer64 ((scheme-object p)) "C_return(((struct stat *)C_data_pointer(p))->st_mtime);"))
  (define get-ctime-sec (foreign-lambda* integer64 ((scheme-object p)) "C_return(((struct stat *)C_data_pointer(p))->st_ctime);"))
  
  ;; Nanoseconds getters using the ST_NSEC macro
  (define get-atime-nsec (foreign-lambda* integer64 ((scheme-object p)) "C_return(ST_NSEC((struct stat *)C_data_pointer(p), a));"))
  (define get-mtime-nsec (foreign-lambda* integer64 ((scheme-object p)) "C_return(ST_NSEC((struct stat *)C_data_pointer(p), m));"))
  (define get-ctime-nsec (foreign-lambda* integer64 ((scheme-object p)) "C_return(ST_NSEC((struct stat *)C_data_pointer(p), c));"))
  
  (define S_IFMT   (foreign-value "S_IFMT"   unsigned-int))
  (define S_IFDIR  (foreign-value "S_IFDIR"  unsigned-int))
  (define S_IFCHR  (foreign-value "S_IFCHR"  unsigned-int))
  (define S_IFBLK  (foreign-value "S_IFBLK"  unsigned-int))
  (define S_IFREG  (foreign-value "S_IFREG"  unsigned-int))
  (define S_IFLNK  (foreign-value "S_IFLNK"  unsigned-int))
  (define S_IFIFO  (foreign-value "S_IFIFO"  unsigned-int))
  (define S_IFSOCK (foreign-value "S_IFSOCK" unsigned-int))

  (define (file-info-type-is? info mask)
	(= (bitwise-and (file-info:mode info) S_IFMT) mask))
  
  (define (file-info-directory? info)         (file-info-type-is? info S_IFDIR))
  (define (file-info-character-special? info) (file-info-type-is? info S_IFCHR))
  (define (file-info-block-special? info)     (file-info-type-is? info S_IFBLK))
  (define (file-info-regular? info)           (file-info-type-is? info S_IFREG))
  (define (file-info-symlink? info)           (file-info-type-is? info S_IFLNK))
  (define (file-info-fifo? info)              (file-info-type-is? info S_IFIFO))
  (define (file-info-socket? info)            (file-info-type-is? info S_IFSOCK))
  (define (file-info-device? info)
	(or (file-info-type-is? info S_IFCHR)
		(file-info-type-is? info S_IFBLK)))

  (foreign-declare "
#include <sys/stat.h>
#include <unistd.h>

/* Use token pasting (##) to handle 'a', 'm', and 'c' prefixes. */
#if defined(__APPLE__) || defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)
  /* BSD/Apple Style: st_atimespec.tv_nsec */
  #define ST_NSEC(s, prefix) ((s)->st_ ## prefix ## timespec.tv_nsec)
#elif defined(__linux__) || defined(__CYGWIN__) || defined(__MSYS__) || defined(_POSIX_C_SOURCE)
  /* Linux/Cygwin/MSYS Style: st_atim.tv_nsec */
  #define ST_NSEC(s, prefix) ((s)->st_ ## prefix ## tim.tv_nsec)
#else
  /* Fallback for older or non-standard POSIX environments */
  #define ST_NSEC(s, prefix) (0)
#endif
")

(define-record-type <file-info>
  (make-file-info
   device inode mode nlinks uid gid rdev
   size atime mtime ctime blksize blocks)
  file-info?
  (device  file-info:device)
  (inode   file-info:inode)
  (mode    file-info:mode)
  (nlinks  file-info:nlinks)
  (uid     file-info:uid)
  (gid     file-info:gid)
  (rdev    file-info:rdev)
  (size    file-info:size)
  (atime   file-info:atime)
  (mtime   file-info:mtime)
  (ctime   file-info:ctime)
  (blksize file-info:blksize)
  (blocks  file-info:blocks))

;; --- FFI Bindings ---

(define %stat  (foreign-lambda int "stat"  c-string c-pointer))
(define %lstat (foreign-lambda int "lstat" c-string c-pointer))
(define %fstat (foreign-lambda int "fstat" int      c-pointer))

(define get-dev     (foreign-lambda* unsigned-integer64 ((scheme-object p)) "C_return(((struct stat *)C_data_pointer(p))->st_dev);"))
(define get-ino     (foreign-lambda* unsigned-integer64 ((scheme-object p)) "C_return(((struct stat *)C_data_pointer(p))->st_ino);"))
(define get-mode    (foreign-lambda* unsigned-int       ((scheme-object p)) "C_return(((struct stat *)C_data_pointer(p))->st_mode);"))
(define get-nlink   (foreign-lambda* unsigned-integer64 ((scheme-object p)) "C_return(((struct stat *)C_data_pointer(p))->st_nlink);"))
(define get-uid     (foreign-lambda* unsigned-int       ((scheme-object p)) "C_return(((struct stat *)C_data_pointer(p))->st_uid);"))
(define get-gid     (foreign-lambda* unsigned-int       ((scheme-object p)) "C_return(((struct stat *)C_data_pointer(p))->st_gid);"))
(define get-rdev    (foreign-lambda* unsigned-integer64 ((scheme-object p)) "C_return(((struct stat *)C_data_pointer(p))->st_rdev);"))
(define get-size    (foreign-lambda* integer64          ((scheme-object p)) "C_return(((struct stat *)C_data_pointer(p))->st_size);"))
(define get-blksize (foreign-lambda* unsigned-integer64 ((scheme-object p)) "C_return(((struct stat *)C_data_pointer(p))->st_blksize);"))
(define get-blocks  (foreign-lambda* unsigned-integer64 ((scheme-object p)) "C_return(((struct stat *)C_data_pointer(p))->st_blocks);"))

;; Basic seconds getters
(define get-atime-sec (foreign-lambda* integer64 ((scheme-object p)) "C_return(((struct stat *)C_data_pointer(p))->st_atime);"))
(define get-mtime-sec (foreign-lambda* integer64 ((scheme-object p)) "C_return(((struct stat *)C_data_pointer(p))->st_mtime);"))
(define get-ctime-sec (foreign-lambda* integer64 ((scheme-object p)) "C_return(((struct stat *)C_data_pointer(p))->st_ctime);"))

;; Nanoseconds getters using the ST_NSEC macro
(define get-atime-nsec (foreign-lambda* integer64 ((scheme-object p)) "C_return(ST_NSEC((struct stat *)C_data_pointer(p), a));"))
(define get-mtime-nsec (foreign-lambda* integer64 ((scheme-object p)) "C_return(ST_NSEC((struct stat *)C_data_pointer(p), m));"))
(define get-ctime-nsec (foreign-lambda* integer64 ((scheme-object p)) "C_return(ST_NSEC((struct stat *)C_data_pointer(p), c));"))

;; --- Type Predicates ---

(define S_IFMT   (foreign-value "S_IFMT"   unsigned-int))
(define S_IFDIR  (foreign-value "S_IFDIR"  unsigned-int))
(define S_IFCHR  (foreign-value "S_IFCHR"  unsigned-int))
(define S_IFBLK  (foreign-value "S_IFBLK"  unsigned-int))
(define S_IFREG  (foreign-value "S_IFREG"  unsigned-int))
(define S_IFLNK  (foreign-value "S_IFLNK"  unsigned-int))
(define S_IFIFO  (foreign-value "S_IFIFO"  unsigned-int))
(define S_IFSOCK (foreign-value "S_IFSOCK" unsigned-int))

(define (file-info-type-is? info mask)
  (= (bitwise-and (file-info:mode info) S_IFMT) mask))

(define (file-info-directory? info)         (file-info-type-is? info S_IFDIR))
(define (file-info-character-special? info) (file-info-type-is? info S_IFCHR))
(define (file-info-block-special? info)     (file-info-type-is? info S_IFBLK))
(define (file-info-regular? info)           (file-info-type-is? info S_IFREG))
(define (file-info-symlink? info)           (file-info-type-is? info S_IFLNK))
(define (file-info-fifo? info)              (file-info-type-is? info S_IFIFO))
(define (file-info-socket? info)            (file-info-type-is? info S_IFSOCK))

;; --- Main Procedure ---

(define (file-info path-or-port #!optional (follow? #t))
  (let ((buf (make-blob (foreign-value "sizeof(struct stat)" int))))
    (if (zero?
         (cond ((port? path-or-port)
                (%fstat (port->fileno path-or-port) (location buf)))
               (follow?
                (%stat path-or-port (location buf)))
               (else
                (%lstat path-or-port (location buf)))))
          (make-file-info
           (get-dev buf)
           (get-ino buf)
           (get-mode buf)
           (get-nlink buf)
           (get-uid buf)
           (get-gid buf)
           (get-rdev buf)
           (get-size buf)
           ;; Seconds + Nanoseconds -> SRFI-19 <time>
           (make-time time-utc (get-atime-nsec buf) (get-atime-sec buf))
           (make-time time-utc (get-mtime-nsec buf) (get-mtime-sec buf))
           (make-time time-utc (get-ctime-nsec buf) (get-ctime-sec buf))
           (get-blksize buf)
           (get-blocks buf))
        (raise-posix-error 'file-info path-or-port))))

  (define (truncate-file fname/port len)
	(let ((res 
		   (cond ((string? fname/port) ((foreign-lambda int "truncate" c-string off-t) fname/port len))
				 ((port? fname/port) ((foreign-lambda int "ftruncate" int off-t) (port->fileno fname/port) len)))))
	  (if (< res 0)
		  (raise-posix-error 'truncate-file fname/port))))
  
  (define (set-file-mode fname mode-bits)
	(let ((res ((foreign-lambda int "chmod" c-string mode-t) fname mode-bits)))
	  (if (< res 0)
		  (raise-posix-error 'set-file-mode fname))))

  (define (umask)
	(let ((res ((foreign-lambda mode-t "umask" mode-t) 0)))
	  ((foreign-lambda mode-t "umask" mode-t) res)
	  res))

  (define (set-umask! umask)
	((foreign-lambda mode-t "umask" mode-t) umask))

;  (define (current-directory)
;	(or ((foreign-lambda* c-string* ()
;			  "
;    size_t sz = 1024;
;    char * buf = malloc(sz);
;    if (!buf) {
;       C_return(NULL);
;    }
;    char *rtn = getcwd(buf, sz);
;    while (rtn == NULL && errno == ERANGE) {
;       sz *= 2;
;       char * tmp = realloc(buf, sz);
;       if (!tmp) {
;           free(buf);
;           C_return(NULL);
;       }
;       buf = tmp;
;       rtn = getcwd(buf, sz);
;    }
;    if (rtn == NULL) {
;       free(buf);
;    }
;    C_return(rtn);
;       "))
;		(raise-posix-error 'current-directory)))


  (define (set-current-directory! new-directory)
	(let ((res ((foreign-lambda int "chdir" c-string ) new-directory)))
	  (if (< res 0)
		  (raise-posix-error 'set-current-directory new-directory))))

  (define (pid)
	((foreign-lambda int "getpid")))

  (define (nice #!optional (delta 1))
	(set-errno! 0)
	(let ((res ((foreign-lambda int "nice" int) delta)))
	  (if (and (= res -1) (not (= (errno) 0)))
		  (raise-posix-error 'nice delta))
	  res))

  (define (user-uid)
	(let ((res ((foreign-lambda uid-t "getuid"))))
	  (if (< res 0)
		  (raise-posix-error 'user-uid))
	  res))

  (define (user-gid)
	(let ((res ((foreign-lambda gid-t "getgid"))))
	  (if (< res 0)
		  (raise-posix-error 'user-gid))
	  res))

  (define (user-effective-uid)
	(let ((res ((foreign-lambda uid-t "geteuid"))))
	  (if (< res 0)
		  (raise-posix-error 'user-uid))
	  res))

  (define (user-effective-gid)
	(let ((res ((foreign-lambda gid-t "getegid"))))
	  (if (< res 0)
		  (raise-posix-error 'user-gid))
	  res))

  (foreign-declare "#include <unistd.h>")
  (foreign-declare "#include <grp.h>")
  
  ;; Returns the number of groups or -1 on error
  (define %getgroups
	(foreign-lambda int "getgroups" int c-pointer))
  
  ;; Extractor to get a specific GID out of the array at index i
  (define get-gid-at-index
	(foreign-lambda* unsigned-int ((scheme-object p) (int i))
      "C_return(((gid_t *)C_data_pointer(p))[i]);"))

  (define (user-supplementary-gids)
  ;; Pass 1: Call with 0 to get the count of supplementary groups
	(let ((count (%getgroups 0 #f)))
      (if (= count -1)
		  (raise-posix-error 'user-supplementary-gids)
          (let ((buf (make-blob (* count (foreign-value "sizeof(gid_t)" int)))))
			;; Pass 2: Provide the blob (via location) to be filled
			(let ((actual-count (%getgroups count (location buf))))
              (if (= actual-count -1)
				  (raise-posix-error 'user-supplementary-gids)
				  (list-tabulate actual-count 
								 (lambda (n) (get-gid-at-index buf n)))))))))
    
  (define (open-directory dir)
	(let ((directory-object ((foreign-lambda DIR* "opendir" c-string) dir)))
	  (if directory-object
		  (set-finalizer! directory-object close-directory)
		  (raise-posix-error 'open-directory dir))
	  directory-object))
  
  (define (read-directory-entry directory-object)
	(let ((dirent ((foreign-lambda dirent* "readdir" DIR*) directory-object)))
	  (if (not dirent)
		  (begin
			(close-directory directory-object)
			#!eof)
		  dirent)))

  (define (read-directory directory-object #!optional (dot-files? #f))
	(let ((dirent (read-directory-entry directory-object)))
	  (if (eof-object? dirent)
		  #!eof
		  (let ((name (dirent-name dirent)))
			(if (not (or (string=? name ".")
						 (string=? name "..")
						 (and (not dot-files?)
							   (string-prefix? "." name))))
				name
				(read-directory directory-object))))))
  
  (define (close-directory directory-object)
	(if directory-object
		((foreign-lambda int "closedir" DIR*) directory-object)))
  
  (define (dirent-name dirent)
	(if (eof-object? dirent)
		dirent
		((foreign-lambda* c-string ((dirent* dirent))
		  "C_return(dirent->d_name);") dirent)))
    
  (define dirent-ino
	(foreign-lambda* unsigned-long ((dirent* dirent))
      "C_return(dirent->d_ino);"))

  (define (make-directory-files-generator dir #!optional (dot-files? #f))
	(let ((directory-object (open-directory dir)))
	  (lambda ()
		(read-directory directory-object dot-files?))))

  (define (directory-files dir #!optional (dot-files? #f))
	(generator->list (make-directory-files-generator dir dot-files?)))


  (foreign-declare "#include <sys/statvfs.h>")
  
  ;; Define the record type to hold the results
  (define-record-type <file-system-info>
	(make-file-system-info
	 block-size
	 fragment-size
	 fragments-number
	 blocks-free
	 blocks-available
	 inode-number
	 inodes-free
	 inodes-available
	 id
	 flags
	 name-max)
	file-system-info?
	(block-size file-system-info:block-size)
	(fragment-size file-system-info:fragment-size)
	(fragments-number file-system-info:number-of-fragments)
	(blocks-free file-system-info:blocks-free)
	(blocks-available file-system-info:blocks-available)
	(inode-number file-system-info:inode-number)
	(inodes-free file-system-info:inodes-free)
	(inodes-available file-system-info:inodes-available)
	(id file-system-info:id)
	(flags file-system-info:flags)
	(name-max file-system-info:name-max))

  
  (define %statvfs
	(foreign-lambda int "statvfs" c-string c-pointer))

  (define %fstatvfs
	(foreign-lambda int "fstatvfs" int c-pointer))

  (define get-bsize
	(foreign-lambda* unsigned-integer64 ((scheme-object p))
      "C_return(((struct statvfs *)p)->f_bsize);"))
  
  (define get-frsize
	(foreign-lambda* unsigned-integer64 ((scheme-object p))
      "C_return(((struct statvfs *)p)->f_frsize);"))

  (define get-blocks
	(foreign-lambda* unsigned-integer64 ((scheme-object p))
      "C_return(((struct statvfs *)p)->f_frsize);"))

  (define get-bfree
	(foreign-lambda* unsigned-integer64 ((scheme-object p))
      "C_return(((struct statvfs *)p)->f_bfree);"))
  
  (define get-bavail
	(foreign-lambda* unsigned-integer64 ((scheme-object p))
      "C_return(((struct statvfs *)p)->f_bavail);"))

  (define get-files
	(foreign-lambda* unsigned-integer64 ((scheme-object p))
      "C_return(((struct statvfs *)p)->f_files);"))

  (define get-ffree
	(foreign-lambda* unsigned-integer64 ((scheme-object p))
      "C_return(((struct statvfs *)p)->f_ffree);"))

  (define get-favail
	(foreign-lambda* unsigned-integer64 ((scheme-object p))
      "C_return(((struct statvfs *)p)->f_favail);"))

  (define get-fsid
	(foreign-lambda* unsigned-integer64 ((scheme-object p))
      "C_return(((struct statvfs *)p)->f_fsid);"))

  (define get-flag
	(foreign-lambda* unsigned-integer64 ((scheme-object p))
      "C_return(((struct statvfs *)p)->f_flag);"))

  (define get-namemax
	(foreign-lambda* unsigned-integer64 ((scheme-object p))
      "C_return(((struct statvfs *)p)->f_namemax);"))

  (define (file-space path-or-port)
	(file-system-info:blocks-available (file-system-info path-or-port)))

  (define (file-system-info path-or-port)
  ;; statvfs struct size varies, so we ask C for the size
	(let ((buf (make-blob (foreign-value "sizeof(struct timespec)" int))))
       (if (zero?
			(if (port? path-or-port)
				(%fstatvfs (port->fileno path-or-port) (location buf))
				(%statvfs path-or-port (location buf))))
			(make-file-system-info
			 (get-bsize buf)
			 (get-frsize buf)
			 (get-blocks buf)
			 (get-bfree buf)
			 (get-bavail buf)
			 (get-files buf)
			 (get-ffree buf)
			 (get-favail buf)
			 (get-fsid buf)
			 (get-flag buf)
			 (get-namemax buf))
			(raise-posix-error 'file-system-info path-or-port))))
  
  (define (real-path path)
	(let* ((c-realpath (foreign-lambda c-string* "realpath" c-string c-pointer))
		   (ptr (c-realpath path #f)))
      (if ptr
		  ptr
          (raise-posix-error 'real-path path))))

  (define-record-type <user-info>
	(make-user-info name password uid gid home-dir shell full-name parsed-full-name)
	user-info?
	(name             user-info:name)
	(password         user-info:password)
	(uid              user-info:uid)
	(gid              user-info:gid)
	(home-dir         user-info:home-dir)
	(shell            user-info:shell)
	(full-name        user-info:full-name)
	(parsed-full-name user-info:parsed-full-name))


  
  (foreign-declare "#include <pwd.h>")
  (foreign-declare "#include <errno.h>")
  (foreign-declare "#include <unistd.h>")
  
  ;; int getpwuid_r(uid_t uid, struct passwd *pwd, char *buf, size_t buflen, struct passwd **result);
  (define %getpwuid_r
	(foreign-lambda int "getpwuid_r" unsigned-int c-pointer c-pointer unsigned-long c-pointer))
  
  ;; int getpwnam_r(const char *name, struct passwd *pwd, char *buf, size_t buflen, struct passwd **result);
  (define %getpwnam_r
	(foreign-lambda int "getpwnam_r" c-string c-pointer c-pointer unsigned-long c-pointer))
  
  (define ERANGE (foreign-value "ERANGE" int))

  (define get-pw-name
	(foreign-lambda* c-string ((scheme-object p))
      "C_return(((struct passwd *)C_data_pointer(p))->pw_name);"))
  
  (define get-pw-passwd
	(foreign-lambda* c-string ((scheme-object p))
      "C_return(((struct passwd *)C_data_pointer(p))->pw_passwd);"))
  
  (define get-pw-uid
	(foreign-lambda* unsigned-int ((scheme-object p))
      "C_return(((struct passwd *)C_data_pointer(p))->pw_uid);"))
  
  (define get-pw-gid
	(foreign-lambda* unsigned-int ((scheme-object p))
      "C_return(((struct passwd *)C_data_pointer(p))->pw_gid);"))
  
  (define get-pw-gecos
	(foreign-lambda* c-string ((scheme-object p))
      "C_return(((struct passwd *)C_data_pointer(p))->pw_gecos);"))
  
  (define get-pw-dir
	(foreign-lambda* c-string ((scheme-object p))
      "C_return(((struct passwd *)C_data_pointer(p))->pw_dir);"))
  
  (define get-pw-shell
	(foreign-lambda* c-string ((scheme-object p))
      "C_return(((struct passwd *)C_data_pointer(p))->pw_shell);"))
  
  (define get-pw-user-found?
	(foreign-lambda* bool ((scheme-object p))
      "C_return(*(struct passwd **)C_data_pointer(p) != NULL);"))

  (define (expand-username-for-gecos uname)
	(if (and (not (string-null? uname))
			 (char-lower-case? (string-ref uname 0)))
		(string-titlecase uname) ;; Beautify for human display
		uname))                  ;; Keep as-is if it's already "special"
  
  (define (parse-full-name gecos username)
	(if (and (string? gecos) (not (string-null? gecos)))
		(let ((components (string-split gecos "," #t)))
          (if (null? components)
              '()
              (let* ((raw-name (car components))
					 ;; Correct SRFI-170 expansion: only capitalize if initial is lowercase
					 (replacement (expand-username-for-gecos username))
					 (expanded-name (irregex-replace "&" raw-name replacement)))
				(cons expanded-name (cdr components)))))
		'()))
  
  (define (user-info user-or-uid)
	(let* ((pwd-size   (foreign-value "sizeof(struct passwd)" int))
           (pwd-buf    (make-blob pwd-size))
           (result-ptr (make-blob (foreign-value "sizeof(struct passwd *)" int)))
           ;; Initial buffer size from sysconf or a sane default
           (init-size  (let ((s (foreign-value "sysconf(_SC_GETPW_R_SIZE_MAX)" long)))
						 (if (< s 0) 1024 s))))
      (let loop ((buf-size init-size))
		(let* ((str-buf (make-blob buf-size))
               (status  (if (integer? user-or-uid)
							(%getpwuid_r user-or-uid (location pwd-buf) (location str-buf) buf-size (location result-ptr))
							(%getpwnam_r user-or-uid (location pwd-buf) (location str-buf) buf-size (location result-ptr)))))
          
          (cond
           ;; 1. Success
           ((zero? status)
              (if (get-pw-user-found? result-ptr)
                  (make-user-info
                   (get-pw-name pwd-buf)
                   (get-pw-passwd pwd-buf)
                   (get-pw-uid pwd-buf)
                   (get-pw-gid pwd-buf)
                   (get-pw-dir pwd-buf)
                   (get-pw-shell pwd-buf)
                   (get-pw-gecos pwd-buf)
                   (parse-full-name (get-pw-gecos pwd-buf) (get-pw-name pwd-buf)))
                  #f)) ;; SRFI-170: return #f if no such user
           
           ;; 2. Buffer too small - double and retry
           ((= status ERANGE)
			(loop (* buf-size 2)))
           
           ;; 3. Actual error
           (else (raise-posix-error 'user-info user-or-uid)))))))

  (define-record-type <group-info>
	(make-group-info name password gid members)
	group-info?
	(name     group-info:name)
	(password group-info:password)
	(gid      group-info:gid)
	(members  group-info:members))
  
  (foreign-declare "#include <grp.h>")

  ;; int getgrgid_r(gid_t gid, struct group *grp, char *buf, size_t buflen, struct group **result);
  (define %getgrgid_r
	(foreign-lambda int "getgrgid_r" unsigned-int c-pointer c-pointer unsigned-long c-pointer))

  ;; int getgrnam_r(const char *name, struct group *grp, char *buf, size_t buflen, struct group **result);
  (define %getgrnam_r
	(foreign-lambda int "getgrnam_r" c-string c-pointer c-pointer unsigned-long c-pointer))

  ;; --- Getters ---

  (define get-gr-name
	(foreign-lambda* c-string ((scheme-object p))
      "C_return(((struct group *)C_data_pointer(p))->gr_name);"))
  
  (define get-gr-passwd
	(foreign-lambda* c-string ((scheme-object p))
      "C_return(((struct group *)C_data_pointer(p))->gr_passwd);"))
  
  (define get-gr-gid
	(foreign-lambda* unsigned-int ((scheme-object p))
      "C_return(((struct group *)C_data_pointer(p))->gr_gid);"))
  
  (define get-gr-group-found?
	(foreign-lambda* bool ((scheme-object p))
      "C_return(*(struct group **)C_data_pointer(p) != NULL);"))

  ;; Get the number of members in the gr_mem array
  (define get-gr-mem-count
	(foreign-lambda* int ((scheme-object p))
      "char **mem = ((struct group *)C_data_pointer(p))->gr_mem;
     int i = 0;
     if (mem) { while(mem[i] != NULL) { i++; } }
     C_return(i);"))
  
  ;; Get the string at index 'i' from the gr_mem array
  (define get-gr-mem-at
	(foreign-lambda* c-string ((scheme-object p) (int i))
      "C_return(((struct group *)C_data_pointer(p))->gr_mem[i]);"))
  
  (define (get-gr-members gr-buf)
	(list-tabulate (get-gr-mem-count gr-buf)
                   (lambda (i) (get-gr-mem-at gr-buf i))))

  (define-record-type <group-info>
	(make-group-info name password gid members)
	group-info?
	(name     group-info:name)
	(password group-info:password)
	(gid      group-info:gid)
	(members  group-info:members))
  
(foreign-declare "#include <grp.h>")

(define %getgrgid_r (foreign-lambda int "getgrgid_r" unsigned-int c-pointer c-pointer unsigned-long c-pointer))
(define %getgrnam_r (foreign-lambda int "getgrnam_r" c-string c-pointer c-pointer unsigned-long c-pointer))

(define get-gr-name   (foreign-lambda* c-string ((scheme-object p)) "C_return(((struct group *)C_data_pointer(p))->gr_name);"))
(define get-gr-passwd (foreign-lambda* c-string ((scheme-object p)) "C_return(((struct group *)C_data_pointer(p))->gr_passwd);"))
(define get-gr-gid    (foreign-lambda* unsigned-int ((scheme-object p)) "C_return(((struct group *)C_data_pointer(p))->gr_gid);"))
(define get-gr-found? (foreign-lambda* bool ((scheme-object p)) "C_return(*(struct group **)C_data_pointer(p) != NULL);"))

(define (group-info group-or-gid)
  (let* ((gr-size    (foreign-value "sizeof(struct group)" int))
         (gr-buf     (make-blob gr-size))
         (result-ptr (make-blob (foreign-value "sizeof(struct group *)" int)))
         (init-size  (let ((s (foreign-value "sysconf(_SC_GETGR_R_SIZE_MAX)" long)))
                       (if (< s 0) 1024 s))))
    (let loop ((buf-size init-size))
      (let* ((str-buf (make-blob buf-size))
             (status  (if (integer? group-or-gid)
                          (%getgrgid_r group-or-gid (location gr-buf) (location str-buf) buf-size (location result-ptr))
                          (%getgrnam_r group-or-gid (location gr-buf) (location str-buf) buf-size (location result-ptr)))))
        (cond
         ((zero? status)
          (if (get-gr-found? result-ptr)
              (make-group-info (get-gr-name gr-buf)
                               (get-gr-passwd gr-buf)
                               (get-gr-gid gr-buf)
                               (get-gr-members gr-buf))
              #f))
         ((= status ERANGE) (loop (* buf-size 2)))
         (else (raise-posix-error 'group-info group-or-gid)))))))
  
  ;; (define (user-info user-or-uid)
  ;; 	(let ((ptr (if (integer? user-or-uid)
  ;;                  (%getpwuid user-or-uid)
  ;;                  (%getpwnam user-or-uid))))
  ;;     (if ptr
  ;;         (make-user-info
  ;;          (get-pw-name ptr)
  ;;          (get-pw-uid ptr)
  ;;          (get-pw-gid ptr)
  ;;          (get-pw-dir ptr)
  ;;          (get-pw-shell ptr))
  ;; 			(raise-posix-error 'user-info user-or-uid))))
  
  ;; (define (user-info user-id)
  ;; 	(let ((raw (user-information user-id #t)))
  ;;     (if (not raw)
  ;;         (raise-posix-error 'user-info user-id)
  ;; 		  (make-user-info
  ;; 		   (vector-ref raw 0)
  ;; 		   (vector-ref raw 2)
  ;; 		   (vector-ref raw 3)
  ;; 		   (vector-ref raw 5)
  ;; 		   (vector-ref raw 6)
  ;; 		   (vector-ref raw 4)
  ;; 		   (vector-ref raw 4)))))

  ;; (define-record-type <group-info>
  ;; 	(make-group-info name gid members)
  ;; 	group-info?
  ;; 	(name    group-info:name)
  ;; 	(gid     group-info:gid)
  ;; 	(members group-info:members))

  ;; (define (group-info group-id)
  ;; 	(let ((raw (group-information group-id #t)))
  ;;     (if (not raw)
  ;;         (raise-posix-error 'group-info group-id)
  ;;         (make-group-info
  ;; 		   (vector-ref raw 0)
  ;; 		   (vector-ref raw 2)
  ;; 		   (vector-ref raw 3)))))


  (foreign-declare "#include <time.h>")

  (define %clock_gettime 
	(foreign-lambda int "clock_gettime" int c-pointer))

  (define get-nsec
	(foreign-lambda* long ((scheme-object ptr))
	  "C_return(((struct timespec *)C_data_pointer(ptr))->tv_nsec);"))
  
  (define get-sec
	(foreign-lambda* integer64 ((scheme-object ptr))
	  "C_return(((struct timespec *)C_data_pointer(ptr))->tv_sec);"))
    
  (define (get-time clock-id)
	;; Create a stack-allocated location for the timespec struct
	(let ((ts (make-blob (foreign-value "sizeof(struct timespec)" int))))
      (if (zero? (%clock_gettime clock-id (location ts)))
          (cons (get-nsec ts) (get-sec ts))
		  #f)))
  
  (define (posix-time)
	(let ((data (get-time (foreign-value "CLOCK_REALTIME" int))))
      (if data
          (make-time time-utc (car data) (cdr data))
		  (raise-posix-error 'posix-time))))
  
  ;; This also seems to be defined in srfi-19, which is presumably wrong.
  (define (monotonic-time)
	(let ((data (get-time (foreign-value "CLOCK_MONOTONIC" int))))
      (if data
          (make-time time-utc (car data) (cdr data))
		  (raise-posix-error 'posix-time))))
  
  ;; chicken scheme already has set-environment-variable!
  
  ;; chicken scheme has an unset-environment-varaible! function, so we
  ;; just need to rename it
  (define delete-environment-variable! unset-environment-variable!)
  
  ;; chicken.port egg already defines it as called terminal-port?
  ;; Since it's inappropriate to rewrite the whole port system, we just rename
  (define terminal? terminal-port?)
  
  )

