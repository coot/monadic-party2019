{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "2.4";
      identifier = { name = "monadic-party2019"; version = "0.1.0.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "profunctor@pm.me";
      author = "Marcin Szamotulski";
      homepage = "https://coot.me";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.conduit)
          (hsPkgs.cborg)
          (hsPkgs.pipes)
          (hsPkgs.serialise)
          (hsPkgs.typed-protocols)
          (hsPkgs.ouroboros-network)
          ];
        };
      exes = {
        "demo-stream" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.contra-tracer)
            (hsPkgs.monadic-party2019)
            (hsPkgs.network)
            (hsPkgs.ouroboros-network)
            (hsPkgs.pipes-bytestring)
            (hsPkgs.typed-protocols)
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.; }