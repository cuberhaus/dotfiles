import json
import os

home = os.path.expanduser( '~' )
path= home  +"/.config/discord/settings.json"

f = open(path)
# lines = f.readlines()
disc = {"SKIP_HOST_UPDATE": True}
data = json.load(f)
data.update(disc)
print(json.dumps(data))

