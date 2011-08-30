;; ede-cmake.el --- EDE Project class for CMake-based C/C++ projects

;; Author: Alastair Rankine <alastair@girtby.net>

(require 'ede-cpp-project)
;;(require 'cmake-lists-parser)
(require 'ede-cmake-build-tool)
(require 'ede-base)

(defclass ede-cmake-cpp-target (ede-cpp-target ede-target)
  (
   (parent :initarg :parent)
   (keybindings :allocation :class
                :initform (("F" . cmake-project-compile-buffer-file)
                           (("D" . ede-debug-target))))
   )
)

(defclass ede-cmake-cpp-project (ede-cpp-project ede-project)
  ((file
    :type string
    :initarg :file
    :initform "CMakeLists.txt"
    :documentation "File name where this project is stored.")
   (locate-build-directory
    :initarg :locate-build-directory
    :type function
    :documentation "Function to call to find the build directory
for a given configuration. Takes two arguments, the config and
the project root directory")
   (build-directories
    :initarg :build-directories
    :type list
    :documentation "Per-configuration build directory")
   (configurations
    :initarg :configurations
    :initform ("None" "Debug" "Release" "RelWithDebInfo" "MinSizeRel")
    :type list
    :custom (repeat string)
    :label "Configuration Options"
    :group (settings)
    :documentation "List of available configuration types.
Individual target/project types can form associations between a
configuration, and target specific elements such as build
variables.")
   (configuration-default
    :initarg :configuration-default
    ; no default, will be selected from first valid build directory
    :initform "None"
    :custom string
    :label "Current Configuration"
    :group (settings)
    :documentation "The default configuration.")
   (build-tool
    :initarg :build-tool
    :type ede-cmake-build-tool)
   (build-tool-additional-parameters
    :initarg :build-tool-additional-parameters
    :type string
    :documentation "Additional parameters to build tool")

   ;; Stolen from ede-cpp-root
   (locate-fcn
    :initarg :locate-fcn
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
exist, it should return nil.")

   (menu :allocation :class
	 :initform
	 (
	  [ "Configure CMake Build Directory" cmake-configure-current-build-directory ]
          )
         :documentation "Menu items for this project")
   )
  "EDE CMake C/C++ project.")

(defun cmake-build-directory-valid-p (dir)
  "Returns DIR if a valid build directory, nil otherwise. Also resolves symlinks."
    (if (and dir (file-exists-p dir) (file-directory-p dir))
        (if (file-symlink-p dir)
            (file-truename dir)
          t)
      nil))

(defmethod initialize-instance ((this ede-cmake-cpp-project)
				&rest fields)
  (call-next-method)

  (let ((f (expand-file-name (oref this :file))))
    (oset this file f)
    (oset this directory (file-name-directory f))
    (unless (slot-boundp this 'name)
      (oset this name (directory-file-name f)))
    )
  (unless (slot-boundp this 'targets)
    (oset this :targets nil))

  ;; TODO is this needed?
  ;; (ede-project-directory-remove-hash (oref this directory))
  ;; (ede-add-project-to-global-list this)

  ;; Call the locate build directory function to populate the build-directories slot.
  (when (and (not (slot-boundp this 'build-directories))
             (slot-boundp this 'locate-build-directory))
    (let ((locatefn (oref this locate-build-directory))
          (dir-root (oref this directory)))
      (oset this build-directories
            (mapcar (lambda (c) (cons c (let ((d (funcall locatefn c dir-root)))
                                          (when d
                                            (if (file-name-absolute-p d) d
                                              (concat (file-name-as-directory dir-root) d))))))
                    (oref this configurations)))
      ))

  ;; Does the configuration-default have a valid build directory?
  (unless (cmake-build-directory-valid-p (cdr (assoc (oref this configuration-default)
                                                     (oref this build-directories))))
    ;; No, set the first configuration that has a build directory
    (unless (oset this configuration-default
                  (or (car (delq nil (mapcar (lambda (c) (if (cmake-build-directory-valid-p (cdr c)) (car c) nil))
                                     (oref this build-directories))))
                      ))

      ;; Fallback of last resort - use the project root, but only if it has been used as a
      ;; build directory before (ie it has a "CMakeFiles" directory)
      (let ((cmakefiles (concat (file-name-as-directory (oref this directory)) "CMakeFiles")))
        (when (and (file-exists-p cmakefiles) (file-directory-p cmakefiles))
          (oset this build-directories (list (cons "None" (oref this directory))))
          (oset this configuration-default "None")
          ))
      ))
    
  ;; Set up the build tool
  (unless (slot-boundp this 'build-tool)
    ;; Take a guess as to what the build tool will be based on the system type. Need a better way to
    ;; do this, but is there a way to get the information out of CMake?
    (oset this build-tool (if (eq system-type 'windows-nt)
                              (cmake-visual-studio-build-tool "Visual Studio")
                            (cmake-make-build-tool "GNU Make Tool")))
  )
)

  
(defmethod ede-menu-items-build ((this ede-cmake-cpp-project) &optional current)
  "Override to add a custom target menu item"
  (append (call-next-method)
          (list
           [ "Build Custom Target..." cmake-project-build-custom-target ])))

(defmethod ede-cmake-create-targets-for-directory ((proj ede-cmake-cpp-project) path)
  "Parse the CMakeLists.txt file and extract the targets."
  ;; TODO: Cache the timestamp of the project file and rescan iff necessary
  )

(defmethod ede-find-target ((proj ede-cmake-cpp-project) buffer)
  "Find an EDE target in PROJ for BUFFER.
If one doesn't exist, create a new one for this directory."
  (let* ((targets (oref proj targets))
	 (dirname (directory-file-name (file-name-directory (buffer-file-name buffer))))
         (path (file-relative-name dirname (oref proj directory)))
	 (ans (object-assoc path :path targets))
	 )
    (when (not ans)
      (setq ans (ede-cmake-cpp-target
                 path
                 :name (file-name-nondirectory dirname)
                 :path path
                 :source nil
                 :parent proj))
      (object-add-to-list proj :targets ans)
      ;; TODO: move the above in here:
      ;;(ede-cmake-create-targets-for-directory this path)
      )
    ans))

(defmethod cmake-build-directory ((this ede-cmake-cpp-project) &optional config)
  "Returns the current build directory. Raises an error if the build directory is not valid"
  (let* ((config (or config (oref this configuration-default)))
         (build-dir (cdr (assoc config (oref this build-directories)))))
    (unless build-dir
      (error "Build directory not set"))
    (unless (and (file-exists-p build-dir) (file-directory-p build-dir))
      (error "Build directory doesn't exist (or is not a directory): %S" build-dir))
    build-dir
    ))

(defmethod cmake-configure-build-directory ((this ede-cmake-cpp-project) &optional config)
  "Set up build directory for configuration type CONFIG, or configuration-default if not set"
    (let* ((config (or config (oref this configuration-default)))
           (default-directory (file-name-as-directory (cmake-build-directory this config)))
           (generator (oref (oref this build-tool) generator-string))
           (define-build-type (if (string= config "None") ""
                                (concat "-DCMAKE_BUILD_TYPE=" config)))
           (cmake-command (format "cmake -G \"%s\" %s %s " generator define-build-type (oref this directory) )))
      (compile cmake-command)
    ))

(defun cmake-configure-current-build-directory ()
  "Configure the build directory for the current build type"
  (interactive)
  (cmake-configure-build-directory (ede-current-project)))

(defmethod cmake-build ((this ede-cmake-cpp-project) &optional target-name)
  (let* ((build-dir (cmake-build-directory this))
         (target-arg (if target-name (concat " --target " target-name) ""))
         (additional-args (if (slot-boundp (oref this build-tool) 'additional-parameters)
                             (concat " -- " (oref (oref this build-tool) additional-parameters) "")))
         (cmake-command (concat "cmake --build " build-dir target-arg
                               " --config " (oref this configuration-default)
                               additional-args)))
    (compile cmake-command)
    ))

(defmethod project-compile-project ((this ede-cmake-cpp-project))
  "Compile the project with CMake"
  (cmake-build this))

(defmethod project-compile-target ((this ede-cmake-cpp-target))
  "Compile the target with CMake"
  (cmake-build (ede-project-root (ede-target-parent this)) (ede-target-name this)))

(defmethod project-compile-file ((this ede-cmake-cpp-target) &optional file)
  "Compile FILE, or the current buffer file if not specified"
  (let ((file (directory-file-name (or file (buffer-file-name (current-buffer))))))
    (compile-target-file (oref (ede-target-parent this) build-tool) this file)))

(defun cmake-project-compile-buffer-file ()
  "Compile current buffer file"
  (interactive)
  (project-compile-file (ede-buffer-object)))

(defmethod project-run-target ((this ede-cmake-cpp-target) &optional args)
  "Run the target"
  (require 'ede-shell)
  (let* ((exe (target-binary (oref (ede-target-parent this) build-tool) this))
	 (cmd (read-from-minibuffer "Run (like this): " exe)))
    (ede-shell-run-something this cmd)))

(defmethod project-debug-target ((this ede-cmake-cpp-project) target)
  "Debug the target"
  (debug-target (oref this build-tool) target))

(defmethod project-debug-target ((this ede-cmake-cpp-target))
  "Debug the target"
  (project-debug-target (ede-project-root (ede-target-parent this)) this))

(defun cmake-project-build-custom-target (target)
  "Prompt for a custom target and build it in the current project"
  (interactive "MTarget: ")
  (cmake-build (ede-current-project) target))

(defmethod cmake-build-tool-run-target ((tool ede-cmake-build-tool) target)
  "Run the specified target"
  (error "cmake-build-tool-run-target not supported by %s" (object-name ot)))

(defmethod ede-system-include-path ((this ede-cmake-cpp-target))
  "Get the system include path used by target THIS."
  (ede-system-include-path (oref this parent)))
  
(defmethod ede-preprocessor-map ((this ede-cmake-cpp-target))
  "Get the pre-processor map for project THIS."
  (ede-preprocessor-map  (oref this parent)))

(defun ede-cmake-cpp-project-root (&optional dir)
  "Gets the root directory for DIR."
  ;; Naive algorithm which just looks up in the directory heirarchy for until it doesn't find a
  ;; CMakeLists.txt file, and the last one is assumed to be the project root.
  ;; TODO: parse the CMakeLists.txt files and looking for PROJECT(..) commands.
  (if (file-exists-p (concat (file-name-as-directory dir) "CMakeLists.txt"))
      (let ((updir (ede-up-directory dir)))
        (or (and updir (ede-cmake-cpp-project-root updir))
            dir))
    nil))

(defmethod ede-project-root ((this ede-cmake-cpp-project))
  this)

(defmethod ede-project-root-directory ((this ede-cmake-cpp-project))
  (file-name-directory (oref this file)))

(defmethod ede-find-subproject-for-directory ((proj ede-cmake-cpp-project) dir)
  ;; TODO: read the CMakeLists.txt file and parse the targets
  this)

(defmethod ede-expand-filename-impl ((proj ede-cmake-cpp-project) name)
  "Within this project PROJ, find the file NAME.
This knows details about or source tree."
  ;; The slow part of the original is looping over subprojects.
  ;; This version has no subprojects, so this will handle some
  ;; basic cases.
  (let ((ans (call-next-method)))
    (unless ans
      (let* ((lf (oref proj locate-fcn))
	     (dir (file-name-directory (oref proj file)))
             (lfans (funcall lf name dir)))
	(if lfans
	    (setq ans lfans)
	  (if (ede-cpp-header-file-p proj name)
	      ;; Else, use our little hack.
	      (let ((ip (oref proj include-path))
		    (tmp nil))
		(while ip
		  ;; Translate
		  (setq tmp (ede-cpp-translate-file proj (car ip)))
		  ;; Test this name.
		  (setq tmp (expand-file-name name tmp))
		  (if (file-exists-p tmp)
		      (setq ans tmp))
		  (setq ip (cdr ip)) ))
	    ;; Else, do the usual.
	    (setq ans (call-next-method)))
	  )))
    ;; TODO - does this call-next-method happen twice.  Is that bad??  Why is it here?
    (or ans (call-next-method))))


;; ;; Example only
;; (defvar my-project-root-build-directories
;;   '(("None" . "build")
;;     ("Debug" . "build.dbg")
;;     ("Release" . "build.rel"))
;;   "Alist of build directories in the project root"
;;  )

;; (defun my-project-root-build-locator (config root-dir)
;;   "Locates a build directory in the project root, uses
;; project-root-build-directories to look up the name."
;;   (cdr (assoc config my-project-root-build-directories)))

;; (defun my-load-project (dir)
;;   "Load a project of type `ede-cmake-cpp-project' for the directory DIR.
;;      Return nil if there isn't one."
;;   (ede-cmake-cpp-project 
;;    (file-name-nondirectory (directory-file-name dir))
;;    :file (expand-file-name "CMakeLists.txt" dir)
;;    :locate-build-directory 'my-project-root-build-locator
;;    :build-tool (cmake-make-build-tool "Make" :additional-parameters "-j4 -kr")
;;    :include-path '( "/" )
;;    :system-include-path (list (expand-file-name "external" dir) )
;;    ))

;; (add-to-list 'ede-project-class-files
;;      	     (ede-project-autoload "CMake"
;;                                    :name "CMake"
;;                                    :file 'ede-cmake
;;                                    :proj-file "CMakeLists.txt"
;;                                    :proj-root 'ede-cmake-cpp-project-root
;;                                    :load-type 'my-load-project
;;                                    :class-sym 'ede-cmake-cpp-project)
;;      	     t)


(provide 'ede-cmake)
