# QuickQuickCheck

quickquickcheck is a tool for Haskell developers that enables an annotation-like system for running property-based tests using the QuickCheck library. By placing special annotations in comments above your function declarations, quickquickcheck automatically discovers these annotations and runs the specified QuickCheck properties on the corresponding functions.

## Features

- **Annotation-based Testing**: Define which QuickCheck properties to run using simple comment annotations above your functions.
- **Automatic Discovery**: Scans your Haskell source files for annotated functions and runs the desired QuickCheck tests.
- **Seamless Integration**: No need to modify your function signatures or add boilerplate code—just annotate and test.

## Example Usage with Annotation Styles

Check the `theorems` directory for examples of how to use quickquickcheck quickly and effectively.

## Development

To get started with quickquickcheck, clone the repository and run the following commands:

```bash
git clone https://github.com/PepeuFBV/quickquickcheck.git
cd quickquickcheck
```

Download the project dependencies:

```bash
# install the necessary dependencies
cabal update

# project's global dependencies
cabal install --lib QuickCheck --package-env .
```

Run the project:

```bash
cabal run
```

This will allow you to contribute to the project, run tests, and explore the codebase.

## Running Tests

To run the tests for quickquickcheck, you can use the following command:

```bash
stack build
```

This will compile the project, now you need to install the project in the PATH:

```bash
stack install
```

Then, install `QuickCheck` globally if you haven't done so already:

```bash
stack install QuickCheck
```

Now, you can execute the quickquickcheck command on your Haskell files:

```bash
stack exec quickquickcheck /path/to/your/file.hs
```

It has to be done through `stack exec` because the project depends on the `hint` library, which allows dynamic loading of Haskell modules at runtime.

> [!WARNING]
> Make sure your `~/.local/bin` is in your `PATH` environment variable, as this is where the `quickquickcheck` executable will be installed. If not, add this line to your `~/.bashrc` or `~/.zshrc`:
> 
> export PATH="$HOME/.local/bin:$PATH"

## AI usage

AI was used to help writting this README file, as well as to help understanding the process of making this project an executable Haskell program.

## Contributing

For contributions, please fork the repository and submit a pull request. Contributions are welcome, whether it's fixing bugs, improving documentation, or adding new features.

[Contributing Guidelines](CONTRIBUTING)

## License

The project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.