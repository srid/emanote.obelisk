# Builds a docker image containing the neuron executable
#
# Run as:
#   docker load -i $(
#     nix-build docker.nix \
#       --argstr name <image-name> \
#       --argstr tag <image-tag>
#   )
let
  pkgs = import ./dep/nixpkgs {};
  emanote = import ./. {};
in {
  name ? "ghcr.io/srid/emanote"
, tag ? "latest"
}: pkgs.dockerTools.buildImage {
  inherit name tag;
  contents = [ 
    emanote
    # These are required for the GitLab CI runner
    pkgs.coreutils 
    pkgs.bash_5 
  ];

  config = {
    WorkingDir = "/notes";
    Volumes = {
      "/notes" = {};
    };
  };
}
