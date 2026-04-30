import { defineConfig } from 'vite';
import tailwindcss from '@tailwindcss/vite';


export default defineConfig({
  root: './',
  build: {
    outDir: './dist',
    rollupOptions: {
      input: { main: './index.html' },
      output: {
        entryFileNames: `[name].js`,
        chunkFileNames: `[name].js`,
        assetFileNames: `[name].[ext]`,
      },
    },
  },
  server: { port: 1234, open: true, },
  plugins: [tailwindcss()],
});
