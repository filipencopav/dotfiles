{
  fuzzel,
  passage,
  pkgs,
}: pkgs.writeShellScriptBin "passfuzzel" ''
  shopt -s nullglob globstar

  prefix=''${PASSAGE_DIR}
  [[ -n $prefix ]] || exit 1
  password_files=( "$prefix"/**/*.age )
  password_files=( "''${password_files[@]#"$prefix"/}" )
  password_files=( "''${password_files[@]%.age}" )

  password=$(printf '%s\n' "''${password_files[@]}" | ${fuzzel}/bin/fuzzel --dmenu "$@")

  [[ -n $password ]] || exit
  ${passage}/bin/passage show -c "$password" 2>/dev/null
''
