let
   
   rev = "d7c0d9a7f83b7f80e08888c040ea8a2ab7ca5f71";
   np = builtins.fetchTarball {
     url = "https://github.com/nixos/nixpkgs/archive/${rev}.tar.gz";
     sha256 = "1i5a0si7m80w1a987crm9xmbin35jpmdcsk3113xxp9xcw0v23rn";
   };
   config = {
      packageOverrides = pkgs: rec {
         haskellPackages = pkgs.haskellPackages.override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {
               rf =
                  haskellPackagesNew.callPackage ./default.nix { };
            };
         };
      };
   };
   pkgs = import np { inherit config; };
in
   rec {
      inherit pkgs;
      rf = pkgs.haskellPackages.rf;
   }
