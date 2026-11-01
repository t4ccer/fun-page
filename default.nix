{
  pkgs ? import <nixpkgs> { },

  microhsSrc ? builtins.path {
    name = "microhs-src";
    path = pkgs.fetchFromGitHub {
      owner = "t4ccer";
      repo = "MicroHS";
      rev = "841aa2e6c04486224c4cc0ea34437548b72f828b";
      hash = "sha256-CqIvRetNVE6gSURd8O6O6XXiKfEa5JLeJ0emCotuLy0=";
    };
    filter =
      path: type:
      let
        base = baseNameOf path;
      in
      !(builtins.elem base [
        ".git"
        "bin"
        "dist-mcabal"
        "ghc-out"
        ".mhscache"
        "mhs.conf"
      ])
      && !(pkgs.lib.hasSuffix ".pkg" base && type == "regular" && baseNameOf (dirOf path) == "lib");
  },
}:

let
  inherit (pkgs) lib;

  mhsVersion = "0.16.12.0";

  hackagePackages = [
    {
      name = "ghc-compat";
      version = "0.5.11.0";
      hash = "sha256-723euxCzwowIBTrtYxPg3V9ErKAhrpC6lFuy/+4acqw=";
    }
    {
      name = "array-mhs";
      version = "0.5.8.0";
      hash = "sha256-pvnDTKFHdjhSRLZ7pIHDXM8zrTlK24pQ1aNzOO3FglM=";
    }
    {
      name = "transformers";
      version = "0.6.2.0";
      hash = "sha256-KJ5LSbe+ifJjujOR1hxuHGpo5FZmo9a62ZlOWXeYdi4=";
    }
    {
      name = "containers";
      version = "0.8.1";
      hash = "sha256-VPjeZqgrVppkqUWjUtwSptx7BTOwjRT8lpIGhklZK0s=";
    }
  ];

  fetchHackage =
    p:
    pkgs.fetchzip {
      url = "https://hackage.haskell.org/package/${p.name}-${p.version}/${p.name}-${p.version}.tar.gz";
      inherit (p) hash;
    };

  funPageSrc = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.unions [
      ./fun-page.cabal
      ./README.md
      ./LICENSE
      ./src
      ./examples
      ./fun-page-tasty
      ./test
      ./fun-page-playground
    ];
  };

  funPageLibSrc = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.unions [
      ./fun-page.cabal
      ./README.md
      ./LICENSE
      ./src
    ];
  };

  mhsEnv = db: ''
    export HOME=$TMPDIR/home
    mkdir -p $HOME
    export CABALDIR=$TMPDIR/cabal
    export MHSPKG=$CABALDIR/mhs-${mhsVersion}
    mkdir -p $CABALDIR
    ${lib.optionalString (db != null) "cp -r --no-preserve=mode ${db}/mhs-${mhsVersion} $CABALDIR/"}
    mkdir -p $MHSPKG/packages
    export MHSDIR=${microhs}/lib/mhs
    export PATH=${microhs}/bin:$PATH
  '';

  microhs = pkgs.stdenv.mkDerivation {
    pname = "microhs";
    version = mhsVersion;
    src = microhsSrc;

    buildPhase = ''
      runHook preBuild
      make bin/mhs bin/mcabal bin/cpphs
      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall
      mkdir -p $out/bin $out/lib/mhs
      cp bin/mhs bin/mcabal bin/cpphs $out/bin/
      cp -r lib mhs src $out/lib/mhs/
      cp mhs.conf $out/lib/mhs/
      runHook postInstall
    '';
    enableParallelBuilding = true;
  };

  mhsPackages = pkgs.stdenv.mkDerivation {
    name = "mhs-packages-${mhsVersion}";
    dontUnpack = true;
    buildPhase = ''
      runHook preBuild
      ${mhsEnv null}
      cp -r --no-preserve=mode ${microhs}/lib/mhs/lib $TMPDIR/base
      (cd $TMPDIR/base && mcabal -r install)
      ${lib.concatMapStrings (p: ''
        cp -r --no-preserve=mode ${fetchHackage p} $TMPDIR/${p.name}
        (cd $TMPDIR/${p.name} && mcabal -r install)
      '') hackagePackages}
      runHook postBuild
    '';
    installPhase = ''
      mkdir -p $out
      cp -r $MHSPKG $out/
    '';
  };

  mhsFunPage = pkgs.stdenv.mkDerivation {
    name = "mhs-packages-fun-page";
    src = funPageLibSrc;
    buildPhase = ''
      runHook preBuild
      ${mhsEnv mhsPackages}
      mcabal -r -fbrowser install
      runHook postBuild
    '';
    installPhase = ''
      mkdir -p $out
      cp -r $MHSPKG $out/
    '';
  };

  compiler = pkgs.stdenv.mkDerivation {
    name = "fun-page-playground-compiler";
    src = lib.fileset.toSource {
      root = ./fun-page-playground/compiler;
      fileset = ./fun-page-playground/compiler/Compiler.hs;
    };
    nativeBuildInputs = [ pkgs.emscripten ];
    buildPhase =
      let
        packages = "base:transformers:containers:ghc-compat:array-mhs:fun-page";
        rts = "${microhs}/lib/mhs/src/runtime";
      in
      ''
        runHook preBuild
        ${mhsEnv mhsFunPage}
        mhs -z -i -i${microhs}/lib/mhs/mhs -i${microhs}/lib/mhs/src -i. Compiler -oplayground.c \
          --embed-packages ${packages} --embed-ffis ${packages}
        export EM_CACHE=$TMPDIR/emscripten-cache
        # ghc-compat imports waitpid without its header.
        emcc -O3 -I${rts} -I${rts}/unix -DUSE_SYSTEM_RAW -include sys/wait.h \
          playground.c ${rts}/eval.c ${rts}/main.c -lm \
          -sMODULARIZE=1 -sEXPORT_NAME=Playground -sENVIRONMENT=web \
          -sEXPORTED_RUNTIME_METHODS=FS,callMain,stringToNewUTF8,UTF8ToString -sFORCE_FILESYSTEM=1 \
          -sALLOW_MEMORY_GROWTH -sTOTAL_STACK=5MB -Wno-address-of-packed-member \
          -o playground.js
        runHook postBuild
      '';
    installPhase = ''
      mkdir -p $out
      cp playground.js playground.wasm $out/
    '';
  };

  funPage =
    (pkgs.pkgsCross.ghcjs.haskellPackages.callCabal2nix "fun-page" funPageSrc { }).overrideAttrs
      (_: {
        # The tests need jsdom (npm).
        doCheck = false;
      });

  site =
    pkgs.runCommand "fun-page-playground-site"
      {
        nativeBuildInputs = [ pkgs.nodejs ];
      }
      ''
        cp -r ${funPageSrc}/fun-page-playground ${funPageSrc}/examples .
        chmod -R u+w fun-page-playground
        # runSite copies the running program (its JavaScript) as app.js.
        node ${funPage}/bin/fun-page-playground --site-only
        cp -r .fun-page-out $out
      '';

  playground = pkgs.runCommand "fun-page-playground" { } ''
    mkdir -p $out
    cp -r ${site}/. ${compiler}/. $out/
  '';
in
{
  inherit
    microhs
    mhsPackages
    mhsFunPage
    compiler
    funPage
    site
    playground
    ;
}
