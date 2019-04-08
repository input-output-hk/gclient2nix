import gclient_eval
import gclient_utils
import sys
import json

filepath = sys.argv[1];

deps_content = gclient_utils.FileRead(filepath)
result = gclient_eval.Parse(deps_content, False, filepath, {}, {})

print(json.dumps(result))
