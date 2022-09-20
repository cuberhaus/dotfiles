import json
import os

home = os.path.expanduser('~')
path = home + "/.config/discord/settings.json"

with open(path, 'r+') as f:
    # lines = f.readlines()
    disc = {"SKIP_HOST_UPDATE": True}
    data = json.load(f)
    data.update(disc)
    print(json.dumps(data))
    f.seek(0) # set pointer to start of file
    json.dump(data, f, indent=4)
