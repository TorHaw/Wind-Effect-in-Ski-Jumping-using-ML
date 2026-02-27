#!/usr/bin/env bash
set -euo pipefail

# ------------------------------------------------------------
# download_links.sh
#
# Downloads FIS PDF links from:
#   data/raw/female_data_links.txt -> data/raw/female_PDFs/
#   data/raw/male_data_links.txt   -> data/raw/male_PDFs/
#
# Run from repo root:
#   bash data/download_links.sh
# ------------------------------------------------------------

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "${script_dir}/.." && pwd)"

female_links="${repo_root}/data/raw/female_data_links.txt"
male_links="${repo_root}/data/raw/male_data_links.txt"

female_out="${repo_root}/data/raw/female_PDFs"
male_out="${repo_root}/data/raw/male_PDFs"

download_from_list () {
  local links_file="$1"
  local out_dir="$2"
  local label="$3"

  if [[ ! -f "$links_file" ]]; then
    echo "Error: missing links file: $links_file"
    exit 1
  fi

  mkdir -p "$out_dir"

  echo "[$label] Links : $links_file"
  echo "[$label] Out   : $out_dir"

  while IFS= read -r url; do
    # trim whitespace
    url="$(echo "$url" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')"
    [[ -z "$url" ]] && continue
    [[ "$url" == \#* ]] && continue

    echo "[$label] Downloading: $url"
    wget -nc --content-disposition -P "$out_dir" "$url"
  done < "$links_file"

  echo "[$label] Done."
  echo
}

download_from_list "$female_links" "$female_out" "female"
download_from_list "$male_links"   "$male_out"   "male"

echo "All downloads complete."