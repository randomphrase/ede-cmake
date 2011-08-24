;; Basically the C/C++ specific bits extracted from ede-cpp-root
;;
;; All Copyright (C) 2007, 2008, 2009, 2010, 2011 Eric M. Ludlam


(require 'ede)

(defclass ede-cpp-target ()
  ()
)

(defclass ede-cpp-project ()
  (
   (include-path :initarg :include-path
		 :initform '( "/include" "../include/" )
		 :type list
		 :documentation
		 "The default locate function expands filenames within a project.
If a header file (.h, .hh, etc) name is expanded, and
the :locate-fcn slot is nil, then the include path is checked
first, and other directories are ignored.  For very large
projects, this optimization can save a lot of time.

Directory names in the path can be relative to the current
buffer's `default-directory' (not starting with a /).  Directories
that are relative to the project's root should start with a /, such
as  \"/include\", meaning the directory `include' off the project root
directory.")
   (system-include-path :initarg :system-include-path
			:initform nil
			:type list
			:documentation
			"The system include path for files in this project.
C files initialized in an ede-cpp-root-project have their semantic
system include path set to this value.  If this is nil, then the
semantic path is not modified.")
   (spp-table :initarg :spp-table
	      :initform nil
	      :type list
	      :documentation
	      "C Preprocessor macros for your files.
Preprocessor symbols will be used while parsing your files.
These macros might be passed in through the command line compiler, or
are critical symbols derived from header files.  Providing header files
macro values through this slot improves accuracy and performance.
Use `:spp-files' to use these files directly.")
   (spp-files :initarg :spp-files
	      :initform nil
	      :type list
	      :documentation
	      "C header file with Preprocessor macros for your files.
The PreProcessor symbols appearing in these files will be used while
parsing files in this project.
See `semantic-lex-c-preprocessor-symbol-map' for more on how this works.")
   (header-match-regexp :initarg :header-match-regexp
			:initform
			"\\.\\(h\\(h\\|xx\\|pp\\|\\+\\+\\)?\\|H\\)$\\|\\<\\w+$"
			:type string
			:documentation
			"Regexp used to identify C/C++ header files.")
   (locate-fcn :initarg :locate-fcn
	       :initform nil
	       :type (or null function)
	       :documentation
	       "The locate function can be used in place of
`ede-expand-filename' so you can quickly customize your custom target
to use specialized local routines instead of the EDE routines.
The function symbol must take two arguments:
  NAME - The name of the file to find.
  DIR - The directory root for this cpp-root project.

It should return the fully qualified file name passed in from NAME.  If that file does not
exist, it should return nil."
	       )
   )
  "Helper class for EDE projects which use C++"
)

(defmethod ede-cpp-header-file-p ((proj ede-cpp-project) name)
  "Non nil if in PROJ the filename NAME is a header."
  (save-match-data
    (string-match (oref proj header-match-regexp) name)))

(defmethod ede-cpp-translate-file ((proj ede-cpp-project) filename)
  "For PROJ, translate a user specified FILENAME.
This is for project include paths and spp source files."
  ;; Step one: Root of this project.
  (let ((dir (file-name-directory (oref proj file))))

    ;; Step two: Analyze first char, and rehost
    (if (and (not (string= filename "")) (= (aref filename 0) ?/))
	;; Check relative to root of project
	(setq filename (expand-file-name (substring filename 1)
					 dir))
      ;; Relative to current directory.
      (setq filename (expand-file-name filename)))

    filename))

(defmethod ede-set-project-variables ((project ede-cpp-project) &optional buffer)
  "Set variables local to PROJECT in BUFFER.
Also set up the lexical preprocessor map."
  (call-next-method)
  (when (and (featurep 'semantic-c) (featurep 'semantic-lex-spp))
    (setq semantic-lex-spp-project-macro-symbol-obarray
	  (semantic-lex-make-spp-table (oref project spp-table)))
    ))

(defmethod ede-system-include-path ((this ede-cpp-project))
  "Get the system include path used by project THIS."
  (oref this system-include-path))
  
(defmethod ede-preprocessor-map ((this ede-cpp-project))
  "Get the pre-processor map for project THIS."
  (let ((spp (oref this spp-table))
	(root (ede-project-root this))
	)
    (mapc
     (lambda (F)
       (let* ((expfile (ede-expand-filename root F))
	      (table (when expfile
		       (semanticdb-file-table-object expfile)))
	      )
	 (when (not table)
	   (message "Cannot find file %s in project." F))
	 (when (and table (semanticdb-needs-refresh-p table))
	   (semanticdb-refresh-table table)
	   (setq spp (append spp (oref table lexical-table))))))
     (oref this spp-files))
    spp))

(defmethod ede-system-include-path ((this ede-cpp-target))
  "Get the system include path used by target THIS."
  (ede-system-include-path (ede-target-parent this)))
  
(defmethod ede-preprocessor-map ((this ede-cpp-target))
  "Get the pre-processor map for project THIS."
  (ede-preprocessor-map  (ede-target-parent this)))

(provide 'ede-cpp-project)
