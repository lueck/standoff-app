(import ./reflex-platform {}).projekt ({ pkgs, ...}: {
  packages = {
    StandOff = ./standoff-tools;
    StandOffApp = ./app;
  };

  shells = {
    ghc = ["StandOff" "StandOffApp"];
    ghcjs = ["StandOff" "StandOffApp"];
  };
})
