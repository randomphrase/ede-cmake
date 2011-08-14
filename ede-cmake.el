(require 'ede-cpp-root)

(defvar project-root-build-directories
  '(("None" . "build")
    ("Debug" . "build.dbg")
    ("Release" . "build.rel"))
  "Alist of build directories in the project root"
 )

(defun project-root-build-locator (config root-dir)
  "Locates a build directory in the project root, uses
project-root-build-directories to look up the name. It may be a
symlink elsewhere, in whch case we resolve the symlink"
  (let* ((build-dir-name (cdr (assoc config project-root-build-directories)))
         (build-dir (concat root-dir build-dir-name)))
    (if (and (file-exists-p build-dir) (file-directory-p build-dir))
        (if (file-symlink-p build-dir)
            (file-truename build-dir)
          (if build-dir-name
              build-dir
            nil))
      nil)
    ))

(defun ede-cmake-load (dir)
  "Load a cmake-cpp-project from DIR."
  (ede-cmake-cpp-project
   "NAME"
   :file (expand-file-name "CMakeLists.txt" dir)
   :locate-build-directory 'project-root-build-locator
   :cmake-build-arguments "-j4"
   ))

(defclass ede-cmake-cpp-project (ede-cpp-root-project)
  (
   (file :type string
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
Individual target/project types can form associations between a configuration,
and target specific elements such as build variables.")
   (configuration-default
    :initarg :configuration-default
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
	  [ "Run CMake In Build Directory" cmake-setup-build-directory ede-object ]
          )
         :documentation "Menu items for this project")
   )
  "EDE CMake C/C++ project.")

(defmethod initialize-instance ((this ede-cmake-cpp-project)
				&rest fields)
  ""
  ;; Add ourselves to the master list
  (call-next-method)

  ;; Call the locate build directory function to populate the build-directories slot
  (when (and (not (slot-boundp this 'build-directories))
             (slot-boundp this 'locate-build-directory))
    (let ((locatefn (oref this locate-build-directory))
          (dir-root (oref this directory)))
      (oset this build-directories
            (mapcar (lambda (c)
                      (let ((build-dir (funcall locatefn c dir-root)))
                        (cons c build-dir)))
                    (oref this configurations)))
      ))

  ;; Set the configuration-default
  (unless (slot-boundp this 'configuration-default)
    (oset this configuration-default
          ;; Set the first configuration that has a build directory
          (car (delq (lambda (c) (if (cdr c) (car c) nil))))
          ))
  )

(defmethod cmake-build-directory ((this ede-cmake-cpp-project))
  "Returns the current build directory. Raises an error if the build directory is not valid"
  (let* ((config (oref this configuration-default))
         (build-dir (cdr (assoc config (oref this build-directories)))))
    (unless build-dir
      (error "Build directory not set"))
    (unless (and (file-exists-p build-dir) (file-directory-p build-dir))
      (error "Build directory doesn't exist (or is not a directory): %S" build-dir))
    build-dir
    ))

(defmethod cmake-setup-build-directory ((this ede-cmake-cpp-project))
  "Set up build directory for configuration type CONFIG, or configuration-default if not set"
    (let* ((default-directory (cmake-build-directory this))
           (cmake-define-build-type (if (string= config "None") ""
                                      (concat "-DCMAKE_BUILD_TYPE=" config)))
           (cmake-command (concat "cmake " cmake-define-build-type " "
                                  (oref this directory) )))
      (compile cmake-command)
    ))

(defmethod project-compile-project ((proj ede-cmake-cpp-project))
  "Compile the project with cmake"
  (let ((build-dir (file-relative-name (cmake-build-directory proj)))
        (additional-args (if (slot-boundp this 'cmake-build-arguments)
                             (concat " -- " (oref this cmake-build-arguments) ""))))
    (compile (concat "cmake --build " build-dir " --config " (oref proj configuration-default) additional-args))
    ))

(defmethod project-compile-target ((this ede-cpp-root-target))
  "Compile the project with cmake"
  (project-compile-project (ede-current-project) command))



(add-to-list 'ede-project-class-files
             (ede-project-autoload "cmake" :name "CMAKE ROOT" 
                                   :file 'ede-cmake
                                   :proj-file 'ede-cpp-root-project-file-for-dir
                                   :proj-root 'ede-cpp-root-project-root
                                   :class-sym 'ede-cmake-cpp-project))

(provide 'ede-cmake)
