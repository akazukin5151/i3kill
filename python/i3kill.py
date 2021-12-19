import os
import json
import subprocess

def main():
    tree = subprocess.check_output('i3-msg -t get_tree', shell=True)
    j = json.loads(tree)
    workspaces = j['nodes'][1]['nodes'][1]['nodes']

    for workspace in workspaces:
        windows = workspace['nodes'][0]['nodes']
        for idx, window in enumerate(windows):
            if idx == 0 and window['focused'] is True:
                return os.system('i3-msg "kill"')
            elif window['focused'] is True:
                return os.system('i3-msg "kill; focus left"')


if __name__ == '__main__':
    main()
