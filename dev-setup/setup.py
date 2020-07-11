#!/bin/python3
import os
import subprocess

xdg_cache_dev_setup=os.environ.get('XDG_CACHE_HOME') + '/dev-setup/'
dev_setup_packages=os.environ.get('HOME') + '/dev-setup/packages/'

def install_package(package):
    print('Running ' + dev_setup_packages + package + ' ...')
    if not os.path.exists(xdg_cache_dev_setup + package):
        process = subprocess.Popen(['sh', dev_setup_packages + package + '.sh'],
                stdout=subprocess.PIPE,
                universal_newlines=True)
        process.wait()
        if process.returncode == 0:
            os.system('touch ' + xdg_cache_dev_setup + package)
    else:
        print('Skipping ...')

def main():
    if not os.path.exists(xdg_cache_dev_setup):
        os.mkdir(xdg_cache_dev_setup)
    install_package('COMMON')
    install_package('ZSH')
    install_package('OPENVPN')
    install_package('ALACRITTY')
    install_package('NEOVIM')
    install_package('NVM')
    install_package('TMUX')
    install_package('PYTHON')

if __name__ == "__main__":
    main()
