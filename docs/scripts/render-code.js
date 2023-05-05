import fs from 'fs/promises';

const templatePath = new URL('../dist/highlighted-code-template.html', import.meta.url);
const template = (await fs.readFile(templatePath, 'utf-8'));

/**
 * Write a HTML file that will display highlighted source code, given the path to a source code file
 *
 * @param {string} srcPath - the path (from the repository root) to the source file
 * @param {string | URL} outFile - the path to which the (HTML) output should be written
 * @param {string} language - the language of the source code, e.g. `javascript`
 */
export default async function renderHighlighted(srcPath, outFile, language) {
  const fileUrl = new URL(srcPath, new URL('../../', import.meta.url));
  const sourceCode = await fs.readFile(fileUrl, 'utf-8');
  const html = template
    .replace('{{SOURCE_PATH}}', srcPath)
    .replace('{{LANGUAGE}}', language)
    .replace('{{SOURCE}}', sourceCode);
  await fs.writeFile(outFile, html);
}
