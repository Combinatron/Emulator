let source = ''
      {
        "owner": "NixOS",
        "repo": "nixpkgs-channels",
        "rev": "7ee897a3b3aa75dc53bd408ceea5f9e4e98822b2",
        "sha256": "1bz6kdrvhm5dpv1b415qh1sl3xxard372axqavfd0xdqkqj5lywk"
      }
      '';
in
import ((import <nixpkgs> {}).fetchFromGitHub (builtins.fromJSON (source)))
