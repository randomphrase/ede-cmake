;; ede-cmake.el --- EDE Project class for CMake-based C/C++ projects

;; Author: Alastair Rankine <alastair@girtby.net>

(require 'ede-cpp-project)
(require 'ede-base)

(defclass ede-cmake-cpp-target (ede-target ede-cpp-target)
  ()
)

(defclass ede-cmake-cpp-project (ede-project ede-cpp-project)
  ((file :type string
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
   (cmake-build-tool :initarg :cmake-build-tool
                     :type cmake-build-tool)
   (build-tool-additional-parameters
    :initarg :build-tool-additional-parameters
    :type string
    :documentation "Additional parameters to build tool")
   (menu :allocation :class
	 :initform
	 (
	  [ "Configure CMake Build Directory" cmake-configure-current-build-directory ]
          )
         :documentation "Menu items for this project")
   )
  "EDE CMake C/C++ project.")

(defclass cmake-build-tool ()
  ((name
    :type string
    :initarg :name
    :documentation "Name of build tool")
   (file-generator
    :type string
    :initarg :file-generator
    :documentation "Which CMake generator to use for build files"))
   (additional-parameters
    :initarg :additional-parameters
    :type string
    :documentation "Additional parameters to build tool")
)

(defclass cmake-make-build-tool (cmake-build-tool)
  ()
  )

(defclass cmake-visual-studio-build-tool (cmake-build-tool)
  ()
  )

(defmethod cmake-build-tool-get-target-directory ((this cmake-make-build-tool) project target)
  (let ((builddir (cmake-build-directory project))
        (path (oref target path)))
    (concat (file-name-as-directory builddir) (file-name-as-directory path))))

(defun cmake-build-directory-valid-p (dir)
  "Returns DIR if a valid build directory, nil otherwise. Also resolves symlinks."
    (if (and dir (file-exists-p dir) (file-directory-p dir))
        (if (file-symlink-p dir)
            (file-truename dir)
          t)
      nil))

(defmethod initialize-instance ((this ede-cmake-cpp-project)
				&rest fields)
  ;; Add ourselves to the master list
  (call-next-method)

  (let ((f (expand-file-name (oref this :file))))
    (oset this file f)
    (oset this directory (file-name-directory f))
    (unless (slot-boundp this 'name)
      (oset this name (directory-file-name f)))
    )
  (unless (slot-boundp this 'targets)
    (oset this :targets nil))

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
    (oset this configuration-default
          ;; TODO: what if none found?
          (car (delq nil (mapcar (lambda (c) (if (cmake-build-directory-valid-p (cdr c)) (car c) nil))
                                 (oref this build-directories))))
          ))

  ;; Set up the build tool
  (unless (slot-boundp this 'cmake-build-tool)
    (oset this cmake-build-tool
          (if (eq system-type 'windows-nt)
              (cmake-visual-studio-build-tool)
            (cmake-make-build-tool
             (when (slot-boundp this 'build-tool-additional-parameters)
               :additional-parameters (oref this build-tool-additional-parameters)))
            )))
  )

  
(defmethod ede-menu-items-build ((this ede-cmake-cpp-project) &optional current)
  "Override to add a custom target menu item"
  (append (call-next-method)
          (list
           [ "Build Custom Target..." cmake-project-build-custom-target ])))

(defmethod ede-find-subproject-for-directory ((proj ede-cmake-cpp-project)
					      dir)
  "Return PROJ, for handling all subdirs below DIR."
  proj)

(defmethod ede-expand-filename-impl ((proj ede-cmake-cpp-project) name)
  "Within this project PROJ, find the file NAME.
This knows details about or source tree."
  ;; The slow part of the original is looping over subprojects.
  ;; This version has no subprojects, so this will handle some
  ;; basic cases.
  (let ((ans (call-next-method)))
    (unless ans
      (let* ((lf (oref proj locate-fcn))
	     (dir (file-name-directory (oref proj file))))
	(if lf
	    (setq ans (funcall lf name dir))
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


(defmethod ede-find-target ((this ede-cmake-cpp-project) buffer)
  "Find an EDE target in PROJ for BUFFER.
If one doesn't exist, create a new one for this directory."
  (let* ((targets (oref proj targets))
	 (dirname (directory-file-name (file-name-directory (buffer-file-name buffer))))
         (path (file-relative-name dirname (oref this directory)))
	 (ans (object-assoc path :path targets))
	 )
    (when (not ans)
      ;; TODO: factor out to separate function, allow multiple targets to be created in single dir
      (setq ans (ede-cmake-cpp-target path
                 :name (file-name-nondirectory dirname)
		 :path path
		 :source nil))
      (object-add-to-list proj :targets ans)
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
           (cmake-define-build-type (if (string= config "None") ""
                                      (concat "-DCMAKE_BUILD_TYPE=" config)))
           (cmake-command (concat "cmake " cmake-define-build-type " "
                                  (oref this directory) )))
      (compile cmake-command)
    ))

(defun cmake-configure-current-build-directory ()
  "Configure the build directory for the current build type"
  (interactive)
  (cmake-configure-build-directory (ede-current-project)))

(defmethod cmake-build ((this ede-cmake-cpp-project) &optional target-name)
  (let* ((build-dir (cmake-build-directory this))
         (target-arg (if target-name (concat " --target " target-name) ""))
         (additional-args (if (slot-boundp this 'build-tool-additional-parameters)
                             (concat " -- " (oref this build-tool-additional-parameters) "")))
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
  (cmake-build ede-object-project (ede-target-name this)))

(defmethod project-run-target ((this ede-cmake-cpp-target) &optional args)
  "Run the target"
  (require 'ede-shell)
  (let* ((proj (ede-current-project))
         (bindir (cmake-build-tool-get-target-directory (oref proj cmake-build-tool) proj this))
         (exe (concat bindir (oref this name)))
         (cmd (if args (concat exe " " args) exe)))
    (ede-shell-run-something this cmd)))

(defun cmake-project-build-custom-target (target)
  "Prompt for a custom target and build it in the current project"
  (interactive "MTarget: ")
  (cmake-build (ede-current-project) target))

(defmethod tool-run-target ((tool cmake-build-tool) target)
  "Run the specified target"
  (error "tool-run-target not supported by %s" (object-name ot)))

;; ;; Example only
;; (add-to-list 'ede-project-class-files
;;              (ede-project-autoload "cmake" :name "CMAKE ROOT" 
;;                                    :file 'ede-cmake
;;                                    :proj-file 'ede-cmake-cpp-project-file-for-dir
;;                                    :proj-root 'ede-cmake-cpp-project-root
;;                                    :class-sym 'ede-cmake-cpp-project))

;; ;; Example only
;; (defvar project-root-build-directories
;;   '(("None" . "build")
;;     ("Debug" . "build.dbg")
;;     ("Release" . "build.rel"))
;;   "Alist of build directories in the project root"
;;  )

;; ;; Example only
;; (defun project-root-build-locator (config root-dir)
;;   "Locates a build directory in the project root, uses
;; project-root-build-directories to look up the name."
;;   (cdr (assoc config project-root-build-directories)))

;; ;; Example only
;; (defun ede-cmake-load (dir)
;;   "Load a cmake-cpp-project from DIR."
;;   (ede-cmake-cpp-project
;;    "NAME"
;;    :file (expand-file-name "CMakeLists.txt" dir)
;;    :locate-build-directory 'project-root-build-locator
;;    :cmake-build-arguments "-j4"
;;    ))

(provide 'ede-cmake)
