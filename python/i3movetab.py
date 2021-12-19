import os
import sys
import json
import subprocess

def core(direction):
    return f'mark --add _last; focus {direction}; swap with mark "_last"; focus {direction}'

def repeat(direction, times):
    if direction == 'left':
        direction = 'right'
    else:
        direction = 'left'
    return f'{core(direction)};' * times

def mk_i3_cmd(s):
    return f'i3-msg "{s}"'

def main(direction):
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
            for idx, window in enumerate(windows):
                if window['focused'] is True:
                    if idx + 1 == len(windows) and direction == 'right':
                        # If this is the last window, move it to the first window
                        # don't swap with front
                        # idx is also the number of times to move left
                        return os.system(mk_i3_cmd(repeat(direction, idx)))
                    elif idx == 0 and direction == 'left':
                        # If this is the first window, move it to the last window
                        # don't swap with front
                        return os.system(mk_i3_cmd(repeat(direction, len(windows) - 1)))
                    else:
                        # Otherwise, swap the windows
                        return os.system(mk_i3_cmd(core(direction)))


if __name__ == '__main__':
    main(sys.argv[1])
