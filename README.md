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

## AI usage

AI was used to help writting this README file, as well as to help understanding the process of making this project an executable Haskell program.

## Contributing

For contributions, please fork the repository and submit a pull request. Contributions are welcome, whether it's fixing bugs, improving documentation, or adding new features.

[Contributing Guidelines](CONTRIBUTING)

## License

The project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.