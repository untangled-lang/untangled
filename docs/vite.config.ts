import { defineConfig } from 'vite';
import mdx from '@mdx-js/rollup';
import rehypeSlug from 'rehype-slug';
import react from '@vitejs/plugin-react';
import { viteStaticCopy } from 'vite-plugin-static-copy';
import { fileURLToPath } from 'url';


export default defineConfig({
  build: {
    target: 'esnext',
    rollupOptions: {
      input: {
        index: fileURLToPath(new URL('./index.html', import.meta.url)),
        lrm: fileURLToPath(new URL('./lrm.html', import.meta.url)),
        tutorial: fileURLToPath(new URL('./tutorial.html', import.meta.url)),
      },
    }
  },
  base: process.env.DOCS_BASE_PATH ?? '/',
  plugins: [
    { enforce: 'pre', ...mdx({ rehypePlugins: [rehypeSlug] }) },
    react({ include: /\.(mdx|js|jsx|ts|tsx)$/ }),
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
