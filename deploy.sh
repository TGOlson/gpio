#!/usr/bin/env bash
set -e

rsync -avz --progress -e ssh ./out/* pi@raspberrypi.local:/home/pi/dev
