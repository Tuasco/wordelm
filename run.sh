#!/usr/bin/env bash

# Help function
show_help() {
  cat <<EOF
Usage: $0 [OPTIONS]

Builds the Elm Wordle app and starts a local Python HTTP server.

Options:
  -d, --detached    Run the server in the background
  -h, --help        Show this help message

Examples:
  $0                Run in foreground (default)
  $0 -d             Run in background
  $0 --help         Show help

The app will be available at http://localhost:PORT/index.html
EOF
}

# Parse options
DETACHED=false
while [[ $# -gt 0 ]]; do
  case $1 in
  -d | --detached)
    DETACHED=true
    shift
    ;;
  -h | --help)
    show_help
    exit 0
    ;;
  *)
    echo "Unknown option: $1"
    show_help
    exit 1
    ;;
  esac
done

# Check if elm is installed
if ! command -v elm &>/dev/null; then
  echo "Error: elm is not installed. Please install Elm from https://elm-lang.org/"
  exit 1
fi

# Check if python3 or python is installed
PYTHON_CMD=""
if command -v python3 &>/dev/null; then
  PYTHON_CMD="python3"
elif command -v python &>/dev/null; then
  PYTHON_CMD="python"
else
  echo "Error: Neither python3 nor python is installed."
  exit 1
fi

echo "Building Elm to JS..."
elm make Main.elm --output=main.js

# Find an open port
PORT=8000
if command -v netstat &>/dev/null; then
  while netstat -tln 2>/dev/null | grep -q ":$PORT "; do
    PORT=$((PORT + 1))
  done
else
  PORT=$((RANDOM % 1000 + 8000)) # Random port between 8000-8999
  echo "Warning: netstat not available, using random port $PORT. It might conflict."
fi

echo "Starting server on port $PORT..."
if [ "$DETACHED" = true ]; then
  $PYTHON_CMD -m http.server $PORT >/dev/null 2>&1 &
  echo "Server running in background on http://localhost:$PORT/index.html"
  echo "PID: $!"
else
  $PYTHON_CMD -m http.server $PORT
  echo "Server stopped."
fi
