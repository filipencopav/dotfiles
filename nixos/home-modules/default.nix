{ util, config, ... }:
let
  features = util.gen-modules-in-dir config [ "my" "features" ] ./features;
in {
  imports = []
  ++ features;
}
