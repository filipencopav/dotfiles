#!/usr/bin/env bash

# Create a temporary directory
temp=$(mktemp -d)

# Function to cleanup temporary directory on exit
cleanup() {
  rm -rf "$temp"
}
trap cleanup EXIT

if [[ -z "${AGE_KEY_PATH:-}" ]]; then
  echo "Error: AGE_KEY_PATH environment variable is not set." >&2
  exit 1
fi

if [[ ! -f "$AGE_KEY_PATH" ]]; then
  echo "Error: Age key file not found: $AGE_KEY_PATH" >&2
  exit 1
fi

if [[ -z "${TARGET_HOST:-}" ]]; then
    echo "Error: TARGET_HOST is not set." >&2
    exit 1
fi

install -d -m755 "$temp/etc/sops"

cp "$AGE_KEY_PATH" "$temp/etc/sops/age.txt"

chmod 600 "$temp/etc/sops/age.txt"

exec nix run nixpkgs#nixos-anywhere -- \
    --extra-files "$temp" \
    --flake '.#xray' \
    --target-host "$TARGET_HOST" \
    --generate-hardware-config nixos-generate-config ./configurations/xray/hardware-configuration.nix
