#!/bin/bash

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" 2>/dev/null && pwd)"
EMACS_FILE_SRC="${SCRIPT_DIR}/.emacs"

setSystemInfo() {
    # BASED ON https://unix.stackexchange.com/questions/6345/how-can-i-get-distribution-name-and-version-number-in-a-simple-shell-script
    if [ -f /etc/os-release ]; then
        # freedesktop.org and systemd
        . /etc/os-release
        OS=GNU/Linux
        DISTRO="${NAME}"
        VER="${VERSION_ID}"
    elif type lsb_release >/dev/null 2>&1; then
        # linuxbase.org
        OS=GNU/Linux
        DISTRO="$(lsb_release -si)"
        VER="$(lsb_release -sr)"
    elif [ -f /etc/lsb-release ]; then
        # For some versions of Debian/Ubuntu without lsb_release command
        . /etc/lsb-release
        OS=GNU/Linux
        DISTRO=$DISTRIB_ID
        VER="${DISTRIB_RELEASE}"
    elif [ -f /etc/debian_version ]; then
        # Older Debian/Ubuntu/etc.
        OS=GNU/Linux
        DISTRO=Debian
        VER="$(cat /etc/debian_version)"
    elif [ -f /etc/SuSe-release ] || [ -f /etc/redhat-release ]; then
        # Older Red Hat, CentOS, etc.
        OS=GNU/Linux
        DISTRO="$(grep -oP '^\w+' /etc/redhat-release)"
        VER="$(grep -oP '\d+(\.\d+)*' /etc/redhat-release)"
    else
        # Fall back to uname, e.g. "Linux <version>",
        # also works for BSD, Cygwin, MINGW, etc.
        OS="$(uname -o)"
        DISTRO="$(uname -s)"
        VER="$(uname -r)"
    fi
}

# Git Bash path to Windows path
toWindowsPath() {
	local path="${1}"
	cygpath --absolute --long-name --windows "${path}"
}

setup() {
	setSystemInfo

    if [ "${OS}" == GNU/Linux ]; then
        # Linux machine
		local emacs_file_dst="${HOME}/.emacs"
		local repo_dir="${SCRIPT_DIR}"
    else
		# TODO MAC?

        # Might be in Cygwin or MinGW. Or Mac?
        local emacs_file_dst="$HOME/AppData/Roaming/.emacs"
		local repo_dir="$(toWindowsPath "${SCRIPT_DIR}" | perl -pe 's|\\|/|g')"
    fi

	perl -pe 's|\@REPO_DIR\@|'"${repo_dir}"'|g' "${EMACS_FILE_SRC}" > \
		"${emacs_file_dst}"
}

setup
