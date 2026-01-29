import { defineConfig } from 'vite';

export default defineConfig({
  root: './',
  build: {
    outDir: './dist',
    rollupOptions: {
      input: {
        main: './index.html',
      },
      output: {
        entryFileNames: `[name].js`,
        chunkFileNames: `[name].js`,
        assetFileNames: `[name].[ext]`,
      },
    },
  },
  server: {
    port: 1234, // Using the same port as mentioned in the README
    open: true,
  },
  resolve: {
    alias: {
      // Add any necessary aliases here
    },
  },
  plugins: [],
});
