# Necro-Bridge Toolkit

## Overview

The Necro-Bridge Toolkit is a reusable, open-source framework for modernizing legacy systems by creating bridge patterns between vintage compiled binaries and modern web applications.

## Purpose

This toolkit will be packaged as `@necro-bridge/core` and provides:

- **Universal Bridge Pattern**: Connect any legacy binary to modern APIs
- **Multi-Language Support**: COBOL, FORTRAN, PASCAL, BASIC, and more
- **Auto-Detection**: Automatically discover legacy binaries in your project
- **CLI Tools**: Command-line interface for quick setup and testing
- **Template Generation**: Pre-built bridge templates for common languages

## Directory Structure

```
toolkit/
├── templates/          # Bridge server templates for each language
├── parsers/           # Output parsing utilities
├── compilers/         # Compilation wrapper utilities
├── server/            # Express server wrapper
└── cli/               # Command-line interface tools
```

## Planned CLI Commands

```bash
necro-bridge init <language>     # Initialize bridge for legacy language
necro-bridge detect              # Auto-detect legacy binaries
necro-bridge serve               # Start bridge server
necro-bridge test <binary>       # Test legacy binary integration
```

## Status

🚧 **Under Construction** - This toolkit is being developed as part of the COBOL Resurrection Bridge hackathon project.

## Future NPM Package

This will be published as `@necro-bridge/core` to enable developers worldwide to resurrect their own legacy systems.

---

*"Bringing the dead back to life, one binary at a time."* 👻
