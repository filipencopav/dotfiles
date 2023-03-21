#!/bin/sh

echo "make sure the phone's connected for file transfer."
read uselessvar

MOUNT_DIR="$(mktemp -d)"
jmtpfs "$MOUNT_DIR" || exit

MUSIC_DIR="$MOUNT_DIR"/"Internal shared storage/Music/"

rsync -rvu "$(xdg-user-dir MUSIC)/" "$MUSIC_DIR"
sync
fusermount -u "$MOUNT_DIR"
