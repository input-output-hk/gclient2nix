{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text)
import GitUrlParser (parseGitRef)
import State
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import Text.Megaparsec

testDepsJson :: Text
testDepsJson = "{\"vars\": {\"electron_git\": \"https://github.com/electron\", \"pyyaml_version\": \"3.12\", \"checkout_node\": true, \"checkout_android\": false, \"boto_git\": \"https://github.com/boto\", \"checkout_requests\": false, \"chromium_version\": \"75.0.3740.3\", \"download_external_binaries\": true, \"yaml_git\": \"https://github.com/yaml\", \"node_version\": \"2dc0f8811b2b295c08d797b8a11b030234c98502\", \"apply_patches\": true, \"requests_git\": \"https://github.com/kennethreitz\", \"checkout_nacl\": false, \"checkout_chromium\": true, \"checkout_oculus_sdk\": false, \"build_with_chromium\": true, \"checkout_android_native_support\": false, \"checkout_libaom\": true, \"requests_version\": \"e4d59bedfd3c7f4f254f4f5d036587bcd8152458\", \"checkout_boto\": false, \"chromium_git\": \"https://chromium.googlesource.com\", \"checkout_pyyaml\": false, \"boto_version\": \"f7574aa6cc2c819430c1f05e9a1a1a666ef8169b\"}, \"hooks\": [{\"action\": [\"python\", \"src/electron/script/apply_all_patches.py\", \"src/electron/patches/common/config.json\"], \"pattern\": \"src/electron\", \"name\": \"patch_chromium\", \"condition\": \"checkout_chromium and apply_patches\"}, {\"action\": [\"python\", \"src/electron/script/update-external-binaries.py\"], \"pattern\": \"src/electron/script/update-external-binaries.py\", \"name\": \"electron_external_binaries\", \"condition\": \"download_external_binaries\"}, {\"action\": [\"python\", \"-c\", \"import os, subprocess; os.chdir(os.path.join(\\\"src\\\", \\\"electron\\\")); subprocess.check_call([\\\"python\\\", \\\"script/lib/npm.py\\\", \\\"install\\\"]);\"], \"pattern\": \"src/electron/package.json\", \"name\": \"electron_npm_deps\"}, {\"action\": [\"python\", \"-c\", \"import os, subprocess; os.chdir(os.path.join(\\\"src\\\", \\\"electron\\\", \\\"vendor\\\", \\\"boto\\\")); subprocess.check_call([\\\"python\\\", \\\"setup.py\\\", \\\"build\\\"]);\"], \"pattern\": \"src/electron\", \"name\": \"setup_boto\", \"condition\": \"checkout_boto\"}, {\"action\": [\"python\", \"-c\", \"import os, subprocess; os.chdir(os.path.join(\\\"src\\\", \\\"electron\\\", \\\"vendor\\\", \\\"requests\\\")); subprocess.check_call([\\\"python\\\", \\\"setup.py\\\", \\\"build\\\"]);\"], \"pattern\": \"src/electron\", \"name\": \"setup_requests\", \"condition\": \"checkout_requests\"}], \"recursedeps\": [\"src\"], \"gclient_gn_args\": [\"build_with_chromium\", \"checkout_android\", \"checkout_android_native_support\", \"checkout_libaom\", \"checkout_nacl\", \"checkout_oculus_sdk\"], \"deps\": {\"src/third_party/electron_node\": {\"url\": \"https://github.com/electron/node.git@2dc0f8811b2b295c08d797b8a11b030234c98502\", \"dep_type\": \"git\", \"condition\": \"checkout_node\"}, \"src\": {\"url\": \"https://chromium.googlesource.com/chromium/src.git@75.0.3740.3\", \"dep_type\": \"git\", \"condition\": \"checkout_chromium\"}, \"src/electron/vendor/boto\": {\"url\": \"https://github.com/boto/boto.git@f7574aa6cc2c819430c1f05e9a1a1a666ef8169b\", \"dep_type\": \"git\", \"condition\": \"checkout_boto\"}, \"src/electron/vendor/requests\": {\"url\": \"https://github.com/kennethreitz/requests.git@e4d59bedfd3c7f4f254f4f5d036587bcd8152458\", \"dep_type\": \"git\", \"condition\": \"checkout_requests\"}, \"src/electron/vendor/pyyaml\": {\"url\": \"https://github.com/yaml/pyyaml.git@3.12\", \"dep_type\": \"git\", \"condition\": \"checkout_pyyaml\"}}, \"gclient_gn_args_file\": \"src/build/config/gclient_args.gni\"}\n"

main :: IO ()
main = do
  pure ()

testParse :: IO ()
testParse = do
  let
    info :: Either String DepsInfo
    info = eitherDecode' $ LBS.fromStrict $ T.encodeUtf8 testDepsJson
  putStrLn $ T.unpack testDepsJson
  print info
  parseTest GitUrlParser.parseGitRef "https://github.com/kennethreitz/requests.git@e4d59bedfd3c7f4f254f4f5d036587bcd8152458"
  parseTest GitUrlParser.parseGitRef "https://github.com/yaml/pyyaml.git@3.12"
  parseTest GitUrlParser.parseGitRef "https://chromium.googlesource.com/chromium/src.git@75.0.3740.3"
  parseTest GitUrlParser.parseGitRef "https://chromium.googlesource.com/external/github.com/google/libprotobuf-mutator.git@439e81f8f4847ec6e2bf11b3aa634a5d8485633d"

testCache :: IO ()
testCache = do
  let
    cache = HashCache (HMS.insert "key" "value" mempty)
  print $ ((eitherDecode' $ encode cache) :: Either String HashCache)
  pure ()
