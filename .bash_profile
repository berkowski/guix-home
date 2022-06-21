export XDG_DATA_DIRS="$HOME/.local/share/flatpak/exports/share:/var/lib/flatpak/exports/share:/usr/local/share:/usr/share"
export XDG_CONFIG_DIRS="/etc/xdg"
export XCURSOR_PATH="$HOME/.local/share/icons:$HOME/.icons:/usr/share/icons:/usr/share/pixmaps"

export GUIX_LOCPATH=$HOME/.guix-profile/lib/locale

# Need this to get the `guix` executable into our path since we're using `guix` on top of Fedora Silverblue
GUIX_PROFILE="${HOME}/.config/guix/current"
. "$GUIX_PROFILE/etc/profile"
