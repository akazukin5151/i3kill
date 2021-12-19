import os
import sys
import json
import subprocess

def main(idx):
    workspaces = subprocess.check_output('i3-msg -t get_workspaces', shell=True)
    j = json.loads(workspaces)

    res = None
    for workspace in j:
        if workspace['focused']:
            res = workspace
            break
    else:  # no break
        return
    this_workspace_id = res['id']

    tree = subprocess.check_output('i3-msg -t get_tree', shell=True)
    j = json.loads(tree)
    workspaces = j['nodes'][1]['nodes'][1]['nodes']

    for workspace in workspaces:
        if workspace['id'] == this_workspace_id:
            windows = workspace['nodes'][0]['nodes']
            ids = [window['id'] for window in windows]
            for window in windows:
                if window['focused'] is True:
                    try:
                        id_ = ids[idx]
                    except IndexError:
                        pass
                    return os.system(f'i3-msg "[con_id={id_}] focus"')


if __name__ == '__main__':
    # 1 is the leftmost key, not 0
    main(int(sys.argv[1]) - 1)
