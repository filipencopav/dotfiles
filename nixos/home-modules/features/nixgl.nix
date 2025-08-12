{
  lib, inputs, ...
}: let
  nixgl = inputs.nixgl;
in {
  nixGL = {
    packages = nixgl.packages;
    defaultWrapper = "mesa";
  };
}
