;; ede-cmake.el --- EDE Project class for CMake-based C/C++ projects

;; Author: Alastair Rankine <alastair@girtby.net>

(require 'ede-cpp-root)

(defclass ede-cmake-cpp-project (ede-cpp-root-project)
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
   (cmake-build-arguments
    :initarg :cmake-build-arguments
    :type string
    :documentation "Additional arguments to build tool")
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
  ;; Add ourselves to the master list
  (call-next-method)

  ;; Call the locate build directory function to populate the build-directories slot.
  (when (and (not (slot-boundp this 'build-directories))
             (slot-boundp this 'locate-build-directory))
    (let ((locatefn (oref this locate-build-directory))
          (dir-root (oref this directory)))
      (oset this build-directories
            (mapcar (lambda (c) (cons c (funcall locatefn c dir-root)))
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
  )
  

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
         (additional-args (if (slot-boundp this 'cmake-build-arguments)
                             (concat " -- " (oref this cmake-build-arguments) "")))
         (cmake-command (concat "cmake --build " build-dir target-arg
                               " --config " (oref this configuration-default)
                               additional-args)))
    (compile cmake-command)
    ))

(defmethod project-compile-project ((this ede-cmake-cpp-project))
  "Compile the project with cmake"
  (cmake-build this))

(defmethod project-compile-target ((this ede-cpp-root-target))
  "Compile the project with cmake"
  (cmake-build ede-object-project (oref this name)))

(defun cmake-project-build-custom-target (target)
  "Prompt for a custom target and build it in the current project"
  (interactive "MTarget: ")
  (cmake-build (ede-current-project) target))

;; ;; Example only
;; (add-to-list 'ede-project-class-files
;;              (ede-project-autoload "cmake" :name "CMAKE ROOT" 
;;                                    :file 'ede-cmake
;;                                    :proj-file 'ede-cpp-root-project-file-for-dir
;;                                    :proj-root 'ede-cpp-root-project-root
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
