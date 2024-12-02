#!/usr/bin/env python
import argparse
import os
from pathlib import Path

import requests

if __name__ == "__main__":
    session_cookie = os.environ.get("AOC_SESSION_COOKIE", None)
    assert session_cookie, "Missing session cookie. Please set environment variable `AOC_SESSION_COOKIE`."
    data_root = os.environ.get("AOC_DATA_PATH", None)
    assert data_root, "Environment variable `AOC_DATA_PATH` has not been set."

    parser = argparse.ArgumentParser()
    parser.add_argument("-y", "--year", nargs="?", type=int, required=True, help="Year")
    parser.add_argument("-d", "--day", nargs="?", type=int, required=True, help="Day")
    args = parser.parse_args()

    data_path = Path(data_root) / str(args.year) / f"input_{args.day:02}.txt"

    if data_path.exists():
        print(f"Data already exists at {data_path}")
    else:
        response = requests.get(
            f"https://adventofcode.com/{args.year}/day/{args.day}/input", cookies={"session": session_cookie}
        )
        if response.status_code != 200:
            print(f"Failed to fetch data. Status code: {response.status_code}")
            exit(1)
        with open(data_path, "w") as writer:
            writer.write(response.text)
        print(f"Data written to {data_path}")
