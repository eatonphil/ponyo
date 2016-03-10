#!/usr/bin/env bash

set -e

find ./tests -name "build.sml" | xargs -n 1 polyc $1
