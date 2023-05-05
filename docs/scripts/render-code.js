import fs from 'fs/promises';

const templatePath = new URL('../dist/highlighted-code-template.html', import.meta.url);
const template = (await fs.readFile(templatePath, 'utf-8'));

/**
 * Write a HTML file that will display highlighted source code, given the path to a source code file
 *
 * @param {string | URL} srcFile - the path to the source code file
 * @param {string | URL} outFile - the path to which the (HTML) output should be written
 * @param {string} language - the language of the source code, e.g. `javascript`
 */
export default async function renderHighlighted(srcFile, outFile, language) {
  const sourceCode = await fs.readFile(srcFile, 'utf-8');
  const html = template
    .replace('{{LANGUAGE}}', language)
    .replace('{{SOURCE}}', sourceCode);
  await fs.writeFile(outFile, html);
}
