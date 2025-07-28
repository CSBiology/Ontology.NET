# Ontology.NET

A library to work with ontologies and ontology-related file formats.

## Documentation

A large documentation page is in preperation at the moment.

Small usage instructions, examples and code snippets can be found under the README.md files for every project/subproject:

- [ControlledVocabulary]()
- [Ontology](https://github.com/CSBiology/Ontology.NET/tree/main/src/Ontology.NET#readme)
- [OBO]()
- [Ontology Extensions]()

## Contributing

Every contribution is welcome, such as:

- Bug reports
- Feature requests
- Documentation improvement requests
- Typo fixes
- Performance discussions and improvements
- New alogithm implementations
- etc.

**Please start by opening an issue**.

**Check the [Development section](#development)** for general guidance on the codebase

**Pull Requests should target the `main` branch from a forked version of the repo.**

This is an **open source project** created as the result of scientific teaching and research efforts.
Please refrain from unrealistic expectations from maintainers.

## Development

### General

Ontology.NET repositories usually folllow this structure:

```
root
│   📄<project name>.sln
│   📄build.cmd
│   📄build.sh
├───📁build
├───📁src
|   └───📁<project name>
└───tests
    └───📁<testproject name>
```

- <project name>.sln is the root solution file.
- `build` contains a [FAKE](https://fake.build/) build project with targets for building, testing and packaging the project.
- `build/sh` and `build.cmd` in the root are shorthand scripts to execute the buildproject.
- `docs` contains the documentation in form of literate scripts and notebooks. 
- `src` contains folders with the source code of the project(s).
- `tests` contains folders with test projects.

### Build

just call `build.sh` or `build.cmd` depending on your OS.

### Test

```bash
build.sh runtests
```

```bash
build.cmd runtests
```

### Create Nuget package

```bash
build.sh pack
```

```bash
build.cmd pack
```
