#!/usr/bin/env bash
set -euo pipefail

# Small Ollama dashboard
ollama ps || true
echo
ollama list || true
echo
echo "Commands:"
echo "  ollama run <model>"
echo "  ollama pull <model>"
echo
exec "${SHELL:-bash}"
