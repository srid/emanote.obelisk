# Builds a docker image containing the neuron executable
#
# Run as:
#   docker load -i $(nix-build docker.nix)
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
  ];

  config = {
    WorkingDir = "/app";
    Volumes = {
      "/app" = {};
    };
  };
}
