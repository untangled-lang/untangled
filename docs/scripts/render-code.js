import fs from 'fs/promises';

const templatePath = new URL('../dist/highlighted-code-template.html', import.meta.url);
const template = (await fs.readFile(templatePath, 'utf-8'));

/**
 * Write a HTML file that will display highlighted source code, given the path to a source code file
 *
 * @param {string} srcPath - the path (from the repository root) to the source file
 * @param {string | URL} outFile - the path to which the (HTML) output should be written
 */
export default async function renderHighlighted(srcPath, outFile) {
  const fileUrl = new URL(srcPath, new URL('../../', import.meta.url));
  const sourceCode = await fs.readFile(fileUrl, 'utf-8');
  const sourceCodeEscaped = sourceCode
    .replaceAll('&', '&amp;')
    .replaceAll('<', '&lt;')
    .replaceAll('>', '&gt;')
    .replaceAll('"', '&quot;');

  const filename = /** @type {string} */ (srcPath.split('/').at(-1));
  const ext = /** @type {string} */ (filename.split('.').at(-1));
  const language = {
    'ml': 'ocaml',
    'mll': 'ocamllex',
    'mly': 'menhir',
    'c': 'c',
    'Makefile': 'make',
    'md': 'markdown',
    'dune': 'dune',
    'dune-project': 'dune-project',
    'll': 'llvm',
  }[ext];
  if (!language) throw new Error(`Could not identify language for file ${srcPath}`);

  const html = template
    .replace('{{SOURCE_PATH}}', srcPath)
    .replace('{{LANGUAGE}}', language)
    .replace('{{SOURCE}}', sourceCodeEscaped);
  await fs.writeFile(outFile, html);
}
