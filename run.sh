#!/bin/bash
msbuild /verbosity:quiet /nologo \
  && mono GenTypeLevel/bin/Debug/GenTypeLevel.exe \
  | tee Generated.fs && (
    echo ""
    echo "--------------------"
    echo ""
    fsharpi --exec --nologo --quiet Generated.fs
  )
