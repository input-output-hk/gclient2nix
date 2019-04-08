with import <nixpkgs> {};

lib.fix (self: {
  inherit pkgs;
  gclient2nix' = haskellPackages.callCabal2nix "gclient2nix" ./gclient2nix {};
  gn = callPackage ./gn.nix {};
  raw_depot_tools = fetchgit {
    url = "https://chromium.googlesource.com/chromium/tools/depot_tools.git";
    rev = "8d3ba46327207c40e66b4ec818cec077f0ac08cf";
    sha256 = "1mpbybgcqpfal956d37qcb3fakidkrvsswxf19l8kls3k73ljmxv";
  };
  updated_depot_tools = runCommand "updated-depot-tools" {
    # TODO: disable the auto-updater
    buildInputs = [ curl python ];
    outputHashMode = "recursive";
    outputHashAlgo = "sha256";
    outputHash = "1bm0nnfgbk9z034ybwzgcv2i64b2vgl7s5ss9qkznxzbxnhz9fvi";
    SSL_CERT_FILE = "${cacert}/etc/ssl/certs/ca-bundle.crt";
  } ''
    mkdir $out
    cp -r ${self.raw_depot_tools} $out/bin
    chmod +w -R $out
    patchShebangs $out/bin/
    $out/bin/gclient --help
  '';
  depot_tools = runCommand "depot-tools" {
    buildInputs = [ makeWrapper ];
  } ''
    cp -r ${self.updated_depot_tools} $out
    chmod +w -R $out
    wrapProgram $out/bin/gclient --prefix PATH : ${python}/bin
  '';
  electron-src = runCommand "electron-src" {
    buildInputs = [ git nodejs python curl ];
    outputHashMode = "recursive";
    outputHashAlgo = "sha256";
    outputHash = "1bm0nnfgbk9z034ybwzgcv2i64b2vgl7s5ss9qkznxzbxn00000i";
    SSL_CERT_FILE = "${cacert}/etc/ssl/certs/ca-bundle.crt";
  } ''
    cp -r ${self.raw_depot_tools} depot_tools
    chmod +w -R depot_tools
    patchShebangs depot_tools
    export PATH=$PATH:$(realpath depot_tools)
    export HOME=/build
    mkdir $out
    cd $out
    gclient config --name "src/electron" --unmanaged https://github.com/electron/electron
    gclient sync --with_branch_heads --with_tags
  '';
  electron = stdenv.mkDerivation {
    name = "electron";
    src = ./.;
    buildInputs = [ self.gn ninja self.depot_tools ];
    buildPhase = ''
      gn gen out
      ls out -l
    '';
  };
  adapter = writeScriptBin "gclient2nix" ''
    #!${stdenv.shell}
    export PYTHONPATH=${self.raw_depot_tools}
    exec ${python}/bin/python ${./gclient2nix.py} "$@"
  '';
  hashes = let
    tofu = "0000000000000000000000000000000000000000000000000000";
  in {
    "https://chromium.googlesource.com/chromium/src.git@63.0.3239.150" = "0pi42pzlgnfdcmwd2csmswg5xspcgf6x8bsvkxqbl7brgwdgm6wi";
    "https://chromium.googlesource.com/chromium/src.git@75.0.3740.3" = "1gjj692mg519zggl8pq29wd8qwz0im9cm07v4i2y9svv656w0ir0";
    "https://github.com/electron/libchromiumcontent.git@1b74a92a80c2077fb0848d81b58ee6f0e4db752d" = "1irg74yfi5c88h7lmshgk7rg7h7s5s09mwaqn5xz86lpzf78r5c5";
    "https://github.com/electron/node.git@v9.7.0-33-g538a5023af" = "0nsmayw1hljx6grji08f0wldmxv0yj6x7gm2dhwwr2wgciwqa4pb";
    "https://github.com/electron/node.git@2dc0f8811b2b295c08d797b8a11b030234c98502" = "1h2xs46q2nr53p4azjw78883x6j6hawgvhpqp5nsaflywn19bz14";
    "https://github.com/electron/native-mate.git@4cd7d113915de0cc08e9a218be35bff9c7361906" = "1kvw3y2gwa7ab66k5816529d1057510kk249lwwvqsa244yn7gv8";
    "https://github.com/boto/boto.git@f7574aa6cc2c819430c1f05e9a1a1a666ef8169b" = "0rvsamyjp8jc9qn4wpd4iz3xlh0nnr2pb715l8ag3brx3d1rh0g3";
    "https://github.com/yaml/pyyaml.git@3.12" = "0pg4ni2j35rcy0yingmm8m3b98v281wsxicb44c2bd5v7k7abhpz";
    "https://github.com/kennethreitz/requests.git@e4d59bedfd3c7f4f254f4f5d036587bcd8152458" = "1bnwam9c8qca2fvw8kqlqw2avfs38jhyla72pm3nk27swnxvcv0y";
    "https://chrome-internal.googlesource.com/chrome/src-internal.git@93420be4148edd84ad5d12c8a4bc28b947c6fdb5" = tofu;
    "https://chromium.googlesource.com/chromium/llvm-project/cfe/tools/clang-format.git@96636aa0e9f047f17447f2d45a094d0b59ed7917" = "0ynvnp3vrcpngmwakb23xv4xn7jbkg43s196q7pg9nkl13x4n2nq";
    "https://chromium.googlesource.com/chromium/llvm-project/libcxx.git@9009625c821e9580bfece732c25bac1cc9c5a7c2" = tofu;
    "https://chromium.googlesource.com/chromium/llvm-project/libcxxabi.git@0d529660e32d77d9111912d73f2c74fc5fa2a858" = tofu;
    "https://chromium.googlesource.com/external/llvm.org/libunwind.git@69d9b84cca8354117b9fe9705a4430d789ee599b" = tofu;
    "https://chromium.googlesource.com/media_router.git@29324b698ccd8920bc81c71d42dadc6310f0ad0f" = tofu;
    "https://chromium.googlesource.com/chromium/canvas_bench.git@a7b40ea5ae0239517d78845a5fc9b12976bfc732" = tofu;
    "https://chromium.googlesource.com/chromium/frame_rate/content.git@c10272c88463efeef6bb19c9ec07c42bc8fe22b9" = tofu;
    "https://chromium.googlesource.com/external/github.com/toji/webvr.info.git@c58ae99b9ff9e2aa4c524633519570bf33536248" = tofu;
    "https://chromium.googlesource.com/external/github.com/immersive-web/webxr-samples.git@cf02f19c4ff6894705a9407722ab52551e010c60" = tofu;
    "https://chromium.googlesource.com/chromium/cdm.git@817c8005a57ea3ca417f767b3b3679601329afd8" = tofu;
    "https://chromium.googlesource.com/native_client/src/native_client.git@7d24fb15d81f95ed30dde0c04b68584749709657" = tofu;
    "https://quiche.googlesource.com/quiche.git@d5d13c2a81b5b504c1bf3308b12c5479a1eb8d58" = tofu;
    "https://chromium.googlesource.com/external/github.com/KhronosGroup/SPIRV-Tools.git@e1a76269b649b7263266c1197622468dae9950b3" = tofu;
    "https://chromium.googlesource.com/angle/angle.git@12a1fe4a5b7a3d4229b971ae9a5ede4c3c08237c" = "0dz731c61dsn1530i5fr2slwmi6fyzx8rqal65zand5d5v22cb84";
    "https://chromium.googlesource.com/external/deqp@66a49e0a43f7af654ee1de8a3b1bcaf6c0d14aa4" = tofu;
    "https://chromium.googlesource.com/external/github.com/glmark2/glmark2@c4b3ff5a481348e8bdc2b71ee275864db91e40b1" = tofu;
    "https://chromium.googlesource.com/external/github.com/Tencent/rapidjson@7484e06c589873e1ed80382d262087e4fa80fb63" = tofu;
    "https://chromium.googlesource.com/external/github.com/KhronosGroup/Vulkan-Headers@c200cb25db0f47364d3318d92c1d8e9dfff2fef1" = tofu;
    "https://chromium.googlesource.com/external/github.com/KhronosGroup/Vulkan-Loader@e1eafa18e17d00374253bcd37d015befa89fcc43" = tofu;
    "https://chromium.googlesource.com/external/github.com/KhronosGroup/Vulkan-Tools@91b17fd866b2e9cfb875bf516b05536d059416b1" = tofu;
    "https://chromium.googlesource.com/external/github.com/KhronosGroup/Vulkan-ValidationLayers@4eee269ae976567ef78db9c9feaafc3364578c87" = tofu;
    "https://boringssl.googlesource.com/boringssl.git@aadcce380fe9e5e17ff38f8471e956463fc4df21" = tofu;
    "https://chromium.googlesource.com/breakpad/breakpad.git@19a8433a604e6105575c08529fc8e0b2947f5af5" = tofu;
    "https://chromium.googlesource.com/catapult.git@aa79e2674359513ec55f7a0e7369675bebac4a77" = tofu;
    "https://chromium.googlesource.com/external/github.com/google/compact_enc_det.git@ba412eaaacd3186085babcd901679a48863c7dd5" = tofu;
    "https://chromium.googlesource.com/chromiumos/chromite.git@82d1eb14f7f621b6f85be4584ccafc9b0dbc8878" = tofu;
    "https://chromium.googlesource.com/external/github.com/google/cld_3.git@c0e79be769c519bd8f92cf6079be6155a1bff579" = tofu;
    "https://chromium.googlesource.com/external/colorama.git@799604a1041e9b3bc5d2789ecbd7e8db2e18e6b8" = tofu;
    "https://chromium.googlesource.com/external/github.com/google/crc32c.git@5998f8451548244de8cde7fab387a550e7c4497d" = tofu;
    "https://chromium.googlesource.com/chromiumos/platform2/system_api.git@4b79dfea832fad7ec8f0bbc4dfd30613e5049ee7" = tofu;
    "https://chromium.googlesource.com/external/github.com/videolan/dav1d.git@ace3855a60379a76624bc01d74bae7fc40233c54" = tofu;
    "https://dawn.googlesource.com/dawn.git@54e4d47db4910ebd1ffce0247b60d4e6f984774f" = tofu;
    "https://chromium.googlesource.com/chromium/tools/depot_tools.git@5637e87bda2811565c3e4e58bd2274aeb3a4757e" = tofu;
    "https://chromium.googlesource.com/external/github.com/ChromeDevTools/devtools-node-modules@5f7cd2497d7a643125c3b6eb910d99ba28be6899" = tofu;
    "https://chromium.googlesource.com/chromium/dom-distiller/dist.git@3093c3e238768ab27ff756bd7563ccbb12129d9f" = tofu;
    "https://chromium.googlesource.com/external/github.com/googlei18n/emoji-segmenter.git@9ba6d25d0d9313569665d4a9d2b34f0f39f9a50e" = tofu;
    "https://chromium.googlesource.com/chromium/third_party/ffmpeg.git@f5b197fe669943e9e18d2181dc8aa696075620aa" = tofu;
    "https://chromium.googlesource.com/chromium/deps/flac.git@af862024c8c8fa0ae07ced05e89013d881b00596" = tofu;
    "https://chromium.googlesource.com/external/github.com/google/flatbuffers.git@9bf9b18f0a705dfd6d50b98056463a55de6a1bf9" = tofu;
    "https://chromium.googlesource.com/external/fontconfig.git@ba206df9b9a7ca300265f650842c1459ff7c634a" = tofu;
    "https://chromium.googlesource.com/chromium/src/third_party/freetype2.git@31757f969fba60d75404f31e8f1168bef5011770" = tofu;
    "https://chromium.googlesource.com/chromiumos/platform/gestures.git@74f55100df966280d305d5d5ada824605f875839" = tofu;
    "https://chromium.googlesource.com/external/github.com/glfw/glfw.git@2de2589f910b1a85905f425be4d32f33cec092df" = tofu;
    "https://chromium.googlesource.com/external/github.com/KhronosGroup/glslang.git@5efb004d59601711cdf328c8a8bfbe7f333dc7a0" = tofu;
    "https://chromium.googlesource.com/external/github.com/google/googletest.git@8b6d3f9c4a774bef3081195d422993323b6bb2e0" = tofu;
    "https://chromium.googlesource.com/external/github.com/grpc/grpc.git@0fc01a302f03c313d33492439366821cb66e0eb6" = tofu;
    "https://chromium.googlesource.com/external/github.com/harfbuzz/harfbuzz.git@bcb4e505d6ffe33e3268a06698e75d6be0e64957" = tofu;
    "https://chromium.googlesource.com/chromium/deps/hunspell_dictionaries.git@f7ce90e84f5aa9acfbc9b7ca04e567bf471e5bcd" = tofu;
    "https://chromium.googlesource.com/chromium/deps/icu.git@b10cc9f714e6da621c94de0f1e6090c176f876ae" = tofu;
    "https://chromium.googlesource.com/external/github.com/open-source-parsers/jsoncpp.git@f572e8e42e22cfcf5ab0aea26574f408943edfa4" = tofu;
    "https://chromium.googlesource.com/external/leveldb.git@9ce30510d482f5b2fa2965201453f0fc914f700c" = tofu;
    "https://chromium.googlesource.com/chromium/llvm-project/compiler-rt/lib/fuzzer.git@e847d8a9b47158695593d5693b0f69250472b229" = tofu;
    "https://chromium.googlesource.com/external/libaddressinput.git@81e7ead903f5b71a326e0584f4325f106c804df1" = tofu;
    "https://aomedia.googlesource.com/aom.git@625cded0550bb79efd10d98a9809a7ccd72a8f60" = tofu;
    "https://chromium.googlesource.com/chromiumos/third_party/libdrm.git@0061b1f244574e615c415479725046ab2951f09a" = tofu;
    "https://chromium.googlesource.com/chromiumos/platform/libevdev.git@9f7a1961eb4726211e18abd147d5a11a4ea86744" = tofu;
    "https://chromium.googlesource.com/chromium/deps/libjpeg_turbo.git@61a2bbaa9aec89cb2c882d87ace6aba9aee49bb9" = tofu;
    "https://chromium.googlesource.com/external/liblouis-github.git@97ce1c67fccbd3668291b7e63c06161c095d49f2" = tofu;
    "https://chromium.googlesource.com/external/libphonenumber.git@a4da30df63a097d67e3c429ead6790ad91d36cf4" = tofu;
    "https://chromium.googlesource.com/external/github.com/google/libprotobuf-mutator.git@439e81f8f4847ec6e2bf11b3aa634a5d8485633d" = tofu;
    "https://chromium.googlesource.com/chromium/deps/libsrtp.git@650611720ecc23e0e6b32b0e3100f8b4df91696c" = tofu;
    "https://chromium.googlesource.com/aosp/platform/system/core/libsync.git@f4f4387b6bf2387efbcfd1453af4892e8982faf6" = tofu;
  };
  gclient2nix = { name, url, sha256, rev }:
  let
    src = fetchgit {
      inherit url sha256 rev;
      fetchSubmodules = false;
    };
    mkDeps.git = { url, ... }:
    let
      parts = lib.splitString "@" (lib.traceShowVal url);
      revparts = lib.splitString "-" (lib.traceShowVal (builtins.elemAt parts 1));
      rev = if (builtins.length revparts == 1) then (builtins.elemAt parts 1) else (builtins.substring 1 20 (builtins.elemAt revparts 2));
      url' = builtins.elemAt parts 0;
      viaGit = fetchgit ({
        url = url';
        rev = rev;
        sha256 = self.hashes.${url};
        fetchSubmodules = false;
      } // (if builtins.length revparts == 1 then {} else {
        branchName = builtins.elemAt revparts 0;
      }));
      viaGitHub = fetchzip {
        url = (lib.strings.replaceStrings [ ".git" ] [ "" ] url') + "/archive/${rev}.tar.gz";
        sha256 = self.hashes.${url};
      };
    in assert (builtins.length parts == 2); if lib.strings.hasPrefix "https://github.com" url' then viaGitHub else viaGit;
    mkDeps.cipd = { ... }: runCommand "fetch-cipd" {} ''
      ${self.raw_depot_tools}/.cipd_client resolve
    '';
    mkDep = { dep_type, ... } @ args: mkDeps.${dep_type} args;
    mkMergeDeps = depsFile:
    let
      depsJson = runCommand "deps.json" { buildInputs = [ self.adapter ]; } ''
        gclient2nix ${depsFile} > $out
      '';
      deps = lib.recursiveUpdate (builtins.fromJSON (builtins.readFile depsJson)) overrides;
      overrides = {
        vars = {
          # TODO, while chasing recursedeps, also inherit parent vars
          build_with_chromium = true;
          checkout_mac = false;
          checkout_ios = false;
          checkout_win = false;
          checkout_linux = true;
        };
      };
    in builtins.trace "${depsFile} ${depsJson}" (lib.concatMapStringsSep "\n" mergeDep (lib.mapAttrsToList (name: value: { inherit name value; inherit (deps) vars recursedeps; }) deps.deps));
    mergeDep = { name, value, vars, recursedeps }: let
      dep = mkDep value;
      mergeScript = ''
        mkdir -pv $out/${name}
        echo copying ${dep} to ${name}
        cp --no-preserve=mode,ownership -r ${dep}/* $out/${name}
        ${if (builtins.elem name recursedeps) then mkMergeDeps "${dep}/DEPS" else "echo not recursing into ${name}"}
      '';
      conditionMap."checkout_ios and checkout_ios_webkit" = false;
      conditionMap.checkout_android_native_support = false;
      conditionMap."not build_with_chromium" = false;
      conditionMap."checkout_fuchsia and not build_with_chromium" = false;
      conditionMap."not build_with_chromium and (host_os == \"linux\" and checkout_fuchsia)" = false;
      conditionMap."not build_with_chromium and (host_os == \"mac\" and checkout_fuchsia)" = false;
      conditionMap."checkout_android or checkout_linux" = true;
      conditionMap."checkout_nacl and checkout_win" = false;
      conditionMap."checkout_ios or checkout_mac" = false;
      conditionParts = lib.splitString " " value.condition;
      processedCondition = if (value ? condition) then cond2 else true;
      cond2 = if (builtins.typeOf value.condition == "bool") then value.condition else cond3;
      cond3 = if (value.condition == ''host_os == "linux"'') then true else cond4;
      cond4 = if (value.condition == ''host_os == "mac"'') then false else cond5;
      cond5 = if (value.condition == ''host_os == "win"'') then false else cond6;
      cond6 = conditionMap.${value.condition} or cond7;
      cond7 = builtins.trace "cond is ${value.condition or "missing"}" (assert (builtins.length conditionParts == 1); vars.${builtins.elemAt conditionParts 0});
    in if processedCondition then mergeScript else "echo skipping ${name}";
    merged = runCommand "thing" {} ''
      set -x
      mkdir -pv $(dirname $out/${name})
      echo copying ${src} to ${name}
      cp -r --no-preserve=mode,ownership ${src} $out/${name}
      ${mkMergeDeps "${src}/DEPS"}
      cd $out
      find -type f | sort > listing.txt
    '';
  in merged;
  electron2 = self.gclient2nix {
    name = "src/electron";
    url = "https://github.com/electron/electron";
    sha256 = "12rnjvf41ix3hnkids4xqq4c60k9q999wgbrn3gmzn5ic1m3g10w";
    #rev = "v3.0.14";
    rev = "9c3cb55ef296254564d72ff9013813f2b03d03b5";
  };
})
