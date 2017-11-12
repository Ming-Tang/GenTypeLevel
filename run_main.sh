#!/bin/bash
msbuild /verbosity:quiet /nologo \
  && <input.txt mono GenTypeLevel.Main/bin/Debug/GenTypeLevel.Main.exe \
        2>/dev/null | tee Generated1.fs \
  && fsharpi --exec --nologo --quiet Generated1.fs
