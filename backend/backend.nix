{ mkDerivation, aeson, base, bytestring, containers, ekg, ekg-core
, fast-logger, hspec, katip, microlens, monad-control, monad-logger
, monad-metrics, mtl, parsec, persistent, persistent-postgresql
, persistent-sqlite, persistent-template, QuickCheck, safe, servant
, servant-js, servant-server, stdenv, text, transformers, wai
, wai-extra, wai-middleware-metrics, warp
}:
mkDerivation {
  pname = "servant-persistent";
  version = "0.2.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers ekg ekg-core fast-logger katip
    microlens monad-control monad-logger monad-metrics mtl parsec
    persistent persistent-postgresql persistent-template servant
    servant-js servant-server text transformers wai wai-extra
    wai-middleware-metrics warp
  ];
  executableHaskellDepends = [
    base ekg ekg-core microlens monad-logger monad-metrics mtl parsec
    persistent persistent-postgresql persistent-sqlite
    persistent-template safe text wai wai-middleware-metrics warp
  ];
  testHaskellDepends = [
    base hspec monad-metrics mtl persistent persistent-postgresql
    QuickCheck servant servant-server text transformers
  ];
  description = "Brief example on using persistent with servant";
  license = stdenv.lib.licenses.mit;
}
