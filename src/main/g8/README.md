# Hello World (Laminar + Vite)

This is an implementation of the default [Hello World](https://github.com/microsoft/vscode-webview-ui-toolkit-samples/tree/main/default/hello-world) sample extension that demonstrates how to set up and use a Scala.js + Laminar + [Vite](https://vitejs.dev/) webview extension.

![A screenshot of the sample extension.](./assets/hello-world.png)

## Documentation

For a deeper dive into how this sample works, read the guides below.

- [Extension structure](./docs/extension-structure.md)
- [Extension commands](./docs/extension-commands.md)
- [Extension development cycle](./docs/extension-development-cycle.md)

## Run The Sample
```
# Navigate into sample directory
cd vscode-laminar-vite-extension

# Install dependencies for both the extension and webview UI source code
npm run install:all

# Build webview UI source code
npm run build:webview

# Open sample in VS Code
code .
```
Once the sample is open inside VS Code you can run the extension by doing the following:

1. Press `F5` to open a new Extension Development Host window
2. Inside the host window, open the command palette (`Ctrl+Shift+P` or `Cmd+Shift+P` on Mac) and type `Hello World (React + Vite): Show`

## Active development
### Scala.js side
Run the Scala.js fast link command in sbt to actively compile the code from scala to javascript on every save:
```
# Navigate into webview scala project
cd vscode-laminar-vite-extension
cd webview-ui

# Start the sbt console
sbt

# Run the Scala.js compiler on watch on save
~fastLinkJS
```
### Vite Side
In a seperate terminal, run vite to reload the frontend on every change to the re-compiled javascript:
```
# In the root directory, start the vite webview server
npm run start:webview
```
### Testing in the extension
Once you are happy with the frontend UI on the vite side, run the build script from the 'Run the Sample' section and hit F5 to view the webview in the extension.
