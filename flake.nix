{
  description = "glmnet - Racket bindings for lasso and elastic-net regularized models";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      version = "0.1.0";

      # Source filter shared by both derivations: drop local build artifacts and
      # any dev-staged shared objects so they do not perturb the store hash.
      cleanSrc = pkgs: src:
        pkgs.lib.cleanSourceWith {
          inherit src;
          filter = path: _type:
            let base = baseNameOf path; in
            base != "build"
            && base != "compiled"
            && base != "result"
            && base != "doc"
            && !(pkgs.lib.hasSuffix ".dylib" base)
            && !(pkgs.lib.hasSuffix ".so" base);
        };
    in
    {
      packages = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };

          # polyfill-glibc rewrites ELF binaries built against a newer glibc so
          # they resolve only symbols available on an older target. Used by
          # scripts/build-so.sh to make Linux candidates portable back to the
          # glibc 2.17 baseline (covers Ubuntu 22.04 / pkg-build.racket-lang.org
          # and older). Not in nixpkgs; pinned to a known-good upstream commit.
          polyfill-glibc = pkgs.stdenv.mkDerivation {
            pname = "polyfill-glibc";
            version = "unstable-2025-dd59051";
            src = pkgs.fetchFromGitHub {
              owner = "corsix";
              repo = "polyfill-glibc";
              rev = "dd59051faaa10ee63c1b96f1b47bf9fcd3770ee2";
              hash = "sha256-Qkzy33dIGnv9BOmRwql+LpYaEukZZIADSux09Fz3h7E=";
            };
            nativeBuildInputs = [ pkgs.ninja ];
            dontConfigure = true;
            buildPhase = ''
              runHook preBuild
              ninja polyfill-glibc
              runHook postBuild
            '';
            installPhase = ''
              runHook preInstall
              install -Dm755 polyfill-glibc $out/bin/polyfill-glibc
              runHook postInstall
            '';
            meta = {
              description = "Patch ELF binaries to require an older glibc version";
              homepage = "https://github.com/corsix/polyfill-glibc";
              license = pkgs.lib.licenses.mit;
              platforms = [ "x86_64-linux" "aarch64-linux" ];
            };
          };

          # The native C-ABI shim: libglmnetcompat, built from the vendored glmnet
          # Fortran (fortran/vendor/glmnet5.f90) plus our iso_c_binding wrapper.
          # ctest runs the Fortran self-checks (incl. the -fdefault-real-8 probe).
          native = pkgs.stdenv.mkDerivation {
            pname = "glmnet-compat";
            inherit version;
            src = cleanSrc pkgs ./fortran;

            nativeBuildInputs = [ pkgs.cmake pkgs.gfortran ];
            # libgfortran / libquadmath as a runtime dependency so the shared
            # object's rpath resolves them out of the store.
            buildInputs = [ pkgs.gfortran.cc.lib ];

            cmakeFlags = [ "-DBUILD_TESTING=ON" ];

            doCheck = true;
            checkPhase = ''
              runHook preCheck
              ctest --output-on-failure
              runHook postCheck
            '';
          };

          # The Racket package, with the native library injected.
          racket = pkgs.stdenv.mkDerivation {
            pname = "glmnet";
            inherit version;
            src = cleanSrc pkgs ./.;

            nativeBuildInputs = [ pkgs.racket pkgs.makeWrapper ];
            buildInputs = [ native ];

            buildPhase = ''
              runHook preBuild

              export PLTUSERHOME=$TMPDIR/racket-home
              export GLMNET_NATIVE_LIB_PATH=${native}
              mkdir -p $PLTUSERHOME

              # Pre-populate native-libs/ so define-runtime-path resolves during
              # the test phase even without the env var.
              mkdir -p ./glmnet/native-libs
              cp ${native}/lib/libglmnetcompat.* ./glmnet/native-libs/ 2>/dev/null || true

              raco pkg install --batch --deps fail --no-setup --copy --scope user \
                --name glmnet ./glmnet

              raco setup --no-docs --pkgs glmnet

              runHook postBuild
            '';

            doCheck = true;
            checkPhase = ''
              runHook preCheck
              # Recursive: covers tests/ plus the literate examples' companion
              # harnesses under glmnet/examples/test/.
              raco test ./glmnet/

              # Render the Scribble docs to catch errors.
              raco scribble --htmls --dest "$TMPDIR/glmnet-doc" glmnet/scribblings/glmnet.scrbl
              runHook postCheck
            '';

            installPhase = ''
              runHook preInstall

              mkdir -p $out/share $out/bin
              cp -r $PLTUSERHOME $out/share/racket-home

              makeWrapper ${pkgs.racket}/bin/racket $out/bin/glmnet \
                --set PLTUSERHOME $out/share/racket-home \
                --add-flags "-l glmnet"

              runHook postInstall
            '';
          };

          # Stage the native library for non-Nix workflows (dev convenience).
          copy-native-libs = pkgs.writeShellApplication {
            name = "copy-native-libs";
            text = ''
              DEST="$(pwd)/glmnet/native-libs"
              mkdir -p "$DEST"
              cp -v --no-preserve=mode ${native}/lib/libglmnetcompat.* "$DEST/"
              echo "Native library copied to $DEST"
              ls -la "$DEST"
            '';
          };
        in
        {
          default = racket;
          inherit native racket copy-native-libs;
        } // nixpkgs.lib.optionalAttrs pkgs.stdenv.isLinux {
          inherit polyfill-glibc;
        });

      apps = forAllSystems (system: {
        copy-native-libs = {
          type = "app";
          program = "${self.packages.${system}.copy-native-libs}/bin/copy-native-libs";
        };
      });

      checks = forAllSystems (system: {
        inherit (self.packages.${system}) native racket;
      });

      devShells = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };
        in
        {
          default = pkgs.mkShell {
            packages = [ pkgs.racket pkgs.gfortran pkgs.cmake pkgs.gnumake ];
            shellHook = ''
              export PLTUSERHOME="$PWD/.racket-user"
              echo "glmnet dev shell. Build the native lib with:"
              echo "  cmake -S fortran -B fortran/build -DBUILD_TESTING=ON && cmake --build fortran/build && ctest --test-dir fortran/build"
              echo "Then stage it:  cp fortran/build/libglmnetcompat.* glmnet/native-libs/"
            '';
          };
        });
    };
}
