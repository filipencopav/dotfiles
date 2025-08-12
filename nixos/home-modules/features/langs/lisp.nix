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
    sbcl
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

    ;;; The following lines added by ql:add-to-init-file:
    #-quicklisp
    (let ((quicklisp-init (merge-pathnames "devel/quicklisp/setup.lisp"
                            (user-homedir-pathname))))
      (when (probe-file quicklisp-init)
        (load quicklisp-init)))
  '';
}
