#!/usr/bin/env bash
rsync -azP . qrpi:~/aoc_sync --exclude={".git/*",".venv/*","**/.ccls-cache/*","cpp/vendors/BigInt/bin/*","cpp/debug/*","cpp/vendors/fmt/objects/*","**/build/*","haskell/.stack-work/*"} && ssh qrpi "cd aoc_sync/arm32; ${1}"
