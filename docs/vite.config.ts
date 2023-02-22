import { defineConfig } from 'vite';
import mdx from '@mdx-js/rollup';
import react from '@vitejs/plugin-react';
import { viteStaticCopy } from 'vite-plugin-static-copy';


export default defineConfig({
  build: { target: 'esnext' },
  plugins: [
    { enforce: 'pre', ...mdx() },
    react(),
    viteStaticCopy({
      targets: [
        // Languages
        { src: '../vscode-extension/syntaxes/*', dest: './languages' },
        { src: './node_modules/shiki/languages/*', dest: './languages' },
        // Themes
        { src: './node_modules/shiki/themes/*', dest: './themes' },
        { src: './node_modules/overnight/themes/*', dest: './themes' },
        // WASM binaries
        { src: './node_modules/shiki/dist/*.wasm', dest: './dist' },
      ],
    }),
  ],
});
