import { defineConfig } from "vite";
import scalaJSPlugin from "@scala-js/vite-plugin-scalajs";

export default defineConfig({
  plugins: [scalaJSPlugin({
          // path to the directory containing the sbt build
      // default: '.'
      cwd: '../.',

      // sbt project ID from within the sbt build to get fast/fullLinkJS from
      // default: the root project of the sbt build
      projectID: 'webview',
})],
  build: {
    outDir: "build",
    rollupOptions: {
      output: {
        entryFileNames: `assets/[name].js`,
        chunkFileNames: `assets/[name].js`,
        assetFileNames: `assets/[name].[ext]`,
      },
    },
  },
});