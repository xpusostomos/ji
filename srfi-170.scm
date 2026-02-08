(module srfi-170 (
				  create-directory
				  create-fifo
				  create-hard-link
				  create-symlink
				  read-symlink
				  open-directory
				  read-directory-entry
				  read-directory
				  close-directory
				  dirent-name
				  dirent-ino
				  make-directory-files-generator
				  directory-files
				  real-path
				  posix-error?
				  posix-error-name
				  posix-error-number)
  (import scheme)
  (import srfi-13)
  (import srfi-121)
;  (import (chicken file posix))
  (import (chicken foreign))
  (import (chicken gc))
  (import (chicken pathname))
  (import (chicken process-context))
  (import (chicken memory))
  (import (chicken base))
  (import (chicken condition))
  
  (foreign-declare "#include <dirent.h>")
  (foreign-declare "#include <sys/types.h>")
  (foreign-declare "#include <sys/stat.h>")
  (foreign-declare "#include <sys/fcntl.h>")
  
  ;; Define DIR* type for readability
  (define-foreign-type DIR* (c-pointer "DIR"))
  (define-foreign-type dirent* (c-pointer "struct dirent"))
  (define-foreign-type mode-t unsigned-int)

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

  (define get-errno 
	(foreign-lambda* int () "C_return(errno);"))
  
  (define c-strerror (foreign-lambda c-string "strerror" int))

  (define (raise-posix-error loc path)
	(let* ((err-num (get-errno))
           (err-name (c-error-name err-num))
           (err-msg  (c-strerror err-num)))
      (abort
       (make-composite-condition
		(make-property-condition 'exn 'message err-msg 'location loc 'arguments (list path))
		(make-property-condition 'i/o-error)
		(make-property-condition 'file-error)
		(make-property-condition 'posix-error 'errno err-num 'name err-name)))))
  
  (define (posix-error? c)
	((condition-predicate 'posix-error) c))

  (define (posix-error-name err)
  (if (and (condition? err) 
           ((condition-predicate 'posix-error) err))
      (get-condition-property err 'posix-error 'name)
      (error "Object is not a posix-error condition" err)))
  
  (define (posix-error-number c)
	(get-condition-property c 'posix-error 'errno))
  
;; Define the function that calls C and frees the memory
(define (real-path path)
  (let* ((c-realpath (foreign-lambda c-string* "realpath" c-string c-pointer))
		 (ptr (c-realpath path #f)))
    (if ptr
		ptr
        (raise-posix-error 'real-path path))))
	  
  )

