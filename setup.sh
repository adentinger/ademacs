#!/bin/bash

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" 2>/dev/null && pwd)"
SCRIPT_BASENAME="$(basename "${BASH_SOURCE[0]}")"
EMACS_FILE_SRC="${SCRIPT_DIR}/emacs.d/init.el"

usage() {
	echo 'USAGE' >&2
	echo "${SCRIPT_BASENAME} (-h | --help)" >&2
	echo "" >&2
	echo "${SCRIPT_BASENAME} --run [--repo]" >&2
	echo "" >&2
	echo "${SCRIPT_BASENAME} --rerun [--repo]" >&2
	echo "" >&2
	echo "" >&2
	echo "OPTIONS" >&2
	echo "  -h | --help Prints this help message and exits." >&2
	echo "" >&2
	echo "  --run Runs the emacs config setup." >&2
	echo "" >&2
	echo "  --rerun Deletes existing emacs config dir and runs the config setup." >&2
	echo "" >&2
	echo "  --repo Don't install this repo's init.el; run it directly from this repo." >&2
}

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
	# Replace backslashed by forward slashes to avoid summoning strange special
	# characters
	cygpath --absolute --long-name --windows "${path}" | perl -pe 's|\\|/|g'
}

setup() {
	setSystemInfo

	local repo_dir="${SCRIPT_DIR}"

    if [ "${OS}" == GNU/Linux ]; then
        # Linux machine
		local emacs_dir_default="${HOME}/emacs.d"

		local ademacs_dir="${HOME}/ademacs.d"
    else
		# TODO MAC?

        # Might be in Cygwin or MinGW. Or Mac?
		local sys_config_dir="$(toWindowsPath "$HOME/AppData/Roaming")"
		local repo_dir="$(toWindowsPath "${repo_dir}")"
		local emacs_dir_default="${sys_config_dir}/.emacs.d"

		local ademacs_dir="${sys_config_dir}/ademacs.d"
    fi

	local emacs_file_dst="${emacs_dir_default}/init.el"
	# Let's keep this config self-contained
	local emacs_dir="${ademacs_dir}"

	if [ "${RERUN}" != 0 ]; then
		echo "Deleting ${emacs_dir}"
		rm -rf "${emacs_dir}"
	fi

	mkdir -p "$(dirname "${emacs_file_dst}")"
	mkdir -p "$(dirname "${ademacs_init_file}")"
	mkdir -p "${emacs_dir}"

	local ademacs_init_file_src="${repo_dir}/ademacs.d/init.el"
	if [ "${REPO}" != 0 ]; then
		local ademacs_init_file="${ademacs_init_file_src}"
	else
		local ademacs_init_file="${emacs_dir}/init.el"
		echo "Creating ${ademacs_init_file}"
		cp "${ademacs_init_file_src}" "${ademacs_init_file}"
	fi

	echo "Creating ${emacs_file_dst}"
	perl -p                                                                   \
		-e 's|\@ADEMACS_INIT_FILE\@|'"${ademacs_init_file}"'|g;'              \
		-e 's|\@EMACS_DIR\@|'"${emacs_dir}"'|g;'                              \
		"${EMACS_FILE_SRC}" > "${emacs_file_dst}"
}

parseArgs() {
	REPO=0
	while [ $# -ge 1 ]; do
		case "${1}" in
		--help | -h)
			usage
			exit 0
			;;
		--run)
			RERUN=0
			;;
		--rerun)
			RERUN=1
			;;
		--repo)
			REPO=1
			;;
		*)
			echo "Unknown argument \"${1}\"." >&2
			echo "" >&2
			usage
			exit 1
			;;
		esac

		shift
	done
	if [ "${RERUN}" == "" ]; then
		echo "Missing --run or --rerun." >&2
		usage
		exit 1
	fi
}

run() {
	parseArgs "$@"
	setup
}

run "$@"
