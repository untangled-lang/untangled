import { defineConfig } from 'vite';
import mdx from '@mdx-js/rollup';
import rehypeSlug from 'rehype-slug';
import react from '@vitejs/plugin-react';
import { viteStaticCopy } from 'vite-plugin-static-copy';


export default defineConfig({
  build: { target: 'esnext' },
  base: process.env.DOCS_BASE_PATH ?? '/',
  plugins: [
    { enforce: 'pre', ...mdx({ rehypePlugins: [rehypeSlug] }) },
    react(),
    viteStaticCopy({
      targets: [
        // Languages
        { src: '../vscode-extension/syntaxes/*', dest: './languages' },
        { src: './node_modules/shiki/languages/*', dest: './languages' },
        // Themes
        { src: './node_modules/shiki/themes/*', dest: './themes' },
        // WASM binaries
        { src: './node_modules/shiki/dist/*.wasm', dest: './dist' },
      ],
    }),
  ],
});
