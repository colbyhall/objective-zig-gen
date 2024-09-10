# Zig Objective-C Generator (zigobjcgen)

![License](https://img.shields.io/badge/license-MIT-blue.svg)

`zigobjcgen` is a work-in-progress tool for generating Objective-C bindings from Zig. This project aims to streamline the process of integrating Objective-C code with Zig, providing developers with a seamless way to interact with Objective-C APIs from Zig projects.

## Goals

- **Objective-C Bindings Generation**: Automatically generate Zig bindings for Objective-C code, enabling smooth integration between Zig and Objective-C.
- **Type Mapping**: Handles basic type conversions between Zig and Objective-C.
- **Command Line Interface**: Easy-to-use CLI for generating bindings directly from Objective-C header files.
- **Comptime Zig to Objective C ABI**: Allow users to write Objective-C compatible Zig so there is no need to write any Objective-C.

## Getting Started

### Prerequisites

- [Zig](https://ziglang.org/) (latest version recommended)
- A working Objective-C toolchain (Xcode on macOS)

### Installation

Since this project is under active development, it's recommended to clone the repository and build from source:

```bash
git clone https://github.com/colbyhall/zigobjcgen.git
cd zigobjcgen
zig build
```

### Usage

The program works off of a manifest file that list out xtool sdk frameworks, their dependencies, and other information for formatting the types.

```bash
zigobjcgen <path/to/manifest.json>
```
Here is an example manifest file

```json
[
    {
        "name": "Security",
        "output_file": "security"
    },
    {
        "name": "CoreFoundation",
        "output_file": "cf",

        "remove_prefix": "CF"
    },
    {
        "name": "CoreServices",
        "output_file": "cs",

        "dependencies": [
            "CoreFoundation"
        ]
    },
    {
        "name": "Foundation",
        "output_file": "ns",

        "remove_prefix": "NS",
        "dependencies": [
            "CoreServices",
            "CoreFoundation",
            "Security"
        ]
    }
]
```

## Roadmap
- [ ] Complete type mapping for all Objective-C types. (Protocols, Interfaces, Blocks)
- [ ] Zig interface for writing Objective-C ABI compatible Zig.
- [ ] Comprehensive test and benchmarking.

## Contributing
Contributions are welcome! Feel free to open issues or submit pull requests if you want to help improve the project.

## License
This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgements
- Thanks to the Zig community for their ongoing support and inspiration
- Inspiration for this project comes from the ened to bridge the gap between Zig and the Objective-C ecosystem.
- Mach and [mach-objc](https://github.com/hexops/mach-objc) for giving me the initial inspiration, resources, and some runtime code bindings.

---

**Note**: This project is actively being developed and is not ready for production use. Expect frequent changes and updates.
