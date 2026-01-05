{ pkgs, config, ... }:
with config.xdg;
let
  ROSWELL_HOME = "${dataHome}/roswell";
in
{
  # placeholder
  # ROSWELL_HOME=${XDG_DATA_HOME}/roswell
  # PATH=${PATH}:${ROSWELL_HOME}/bin
  home.packages = with pkgs; [
    roswell
    (sbcl.withPackages (sp: [
      sp.cffi-libffi
      sp.cffi
    ]))
  ];

  home.sessionVariables =  {
    inherit ROSWELL_HOME;
  };

  home.sessionPath = [
    "${ROSWELL_HOME}/bin"
  ];

  home.file.".sbclrc".text = ''
    ;;; My user configs.
    (setf *print-case* :downcase)

    ;;; Quicklisp installation.
    (require :asdf)
    #-quicklisp
    (let* ((ql-dir (uiop:ensure-pathname
                     (merge-pathnames
                       "devel/quicklisp/"
                       (user-homedir-pathname))))
           (ql-install (merge-pathnames "install.lisp" ql-dir))
           (ql-setup (merge-pathnames "setup.lisp" ql-dir)))
      (if (probe-file ql-setup)
        (load ql-setup)
        (when (probe-file ql-install)
          (load ql-install)
          (funcall (with-input-from-string (s "quicklisp-quickstart:install") (read s))
            :path ql-dir))))
  '';
}
