import fs from 'fs/promises';
import { globby } from 'globby';
import { dirname } from 'path';
import { fileURLToPath } from 'url';
import { spawn } from 'child_process';
import { stdout } from 'process';

import puppeteer from 'puppeteer';
import { preview } from 'vite';
import renderHighlighted from './render-code.js';


const cropScriptPath = fileURLToPath(new URL('./crop-pdf.py', import.meta.url));
const mergeScriptPath = fileURLToPath(new URL('./merge-pdfs.py', import.meta.url));

/** Get an absolute path in the output directory
 *  @param {string} path */
const getOutputPath = (path) => fileURLToPath(new URL(path, new URL('../export/', import.meta.url)));
const finalPdfPath = getOutputPath('final-report.pdf');



// Check the app has been built

const hasDist = await fs.access('dist').then(() => true).catch(() => false);
if (!hasDist) {
  console.error('Error: run `vite build` before capturing a PDF');
  process.exit(1);
}


// Serve the built app

const previewServer = await preview({
  base: process.env.VITE_DOCS_BASE_PATH ?? '/',
  preview: { port: 5174 },
});
const localServerBase = previewServer.resolvedUrls.local[0];
console.log('Started server');


const browser = await puppeteer.launch({ headless: 'new' });
console.log('Launched browser');


const inch = 96;
/**
 * Capture a webpage to a piece of the PDF
 * @param {string} pagePath The path on the website to capture
 */
async function pageToPdf(pagePath) {
  console.log(`Capturing PDF for ${pagePath}`);
  if (pagePath.startsWith('/')) pagePath = pagePath.slice(1);
  // Load the web page
  const page = await browser.newPage();
  page.on('console', (msg) => console.log('[PAGE LOG]', msg.text()));
  page.on('pageerror', (error) => console.log('[PAGE ERROR]', error.message));
  const url = new URL(pagePath, localServerBase);
  await page.goto(url.href);
  await page.waitForSelector('#root-container *');

  // Set up the page
  await page.emulateMediaType('print');
  await new Promise((resolve) => setTimeout(resolve, 500));
  await page.waitForNetworkIdle({ idleTime: 200 });
  const zoom = 0.66;
  // @ts-ignore
  await page.evaluate((z) => document.documentElement.style.zoom = z, zoom);
  await new Promise((resolve) => setTimeout(resolve, 500));

  // Measure the height of each page of the PDF
  const pageHeights = await page.evaluate((z) => {
    // Collapse all pages besides the current one
    const pageBreaks = /** @type {HTMLElement[]} */ ([...document.getElementsByClassName('page-break')]);
    return [
      // height of all pages up to the last break
      ...pageBreaks.map((p, i, arr) => p.offsetTop - (arr[i - 1]?.offsetTop ?? 0)),
      // height of final page
      document.body.scrollHeight - (pageBreaks[pageBreaks.length - 1]?.offsetTop ?? 0),
    ].map((ph) => ph * z);
  }, zoom);

  // Capture the PDF
  const outFileName = pagePath.endsWith('.html') ? pagePath.slice(0, -5) : pagePath;
  const uncroppedPdfPath = getOutputPath(`${outFileName}-uncropped.pdf`);
  const croppedPdfPath = getOutputPath(`${outFileName}.pdf`);
  await fs.mkdir(dirname(uncroppedPdfPath), { recursive: true });
  await page.pdf({
    width: '8.5in',
    // add extra padding to make sure we’re not wrapping. note we’re going to crop it away later
    height: Math.max(...pageHeights) * 1.1,
    printBackground: true,
    path: uncroppedPdfPath,
  });

  // We don’t need the browser page anymore
  await page.close();

  // Call out to our Python script to crop the PDF pages
  const cropScript = spawn('python', [cropScriptPath, uncroppedPdfPath, croppedPdfPath]);
  cropScript.stdin.write(JSON.stringify(pageHeights.map((ph) => ph / inch)));
  cropScript.stdin.end();
  cropScript.stdout.on('data', (data) => { stdout.write(data); }); // make Python script’s output visible for debugging
  await new Promise((resolve, reject) => {
    cropScript.on('exit', resolve);
    cropScript.on('error', reject);
  });

  // Clean up
  await fs.rm(uncroppedPdfPath);
  console.log(`Captured ${pagePath}`);

  return croppedPdfPath
}


/**
 * Capture a source code file to a PDF
 * @param {string} sourcePath - The path (from the repository root) to the source code file to capture
 */
async function codeToPdf(sourcePath) {
  const pagePath = fileURLToPath(new URL(`${sourcePath}.html`, new URL('../dist/source/', import.meta.url)));
  console.log(pagePath);
  await fs.mkdir(dirname(pagePath), { recursive: true });

  // Create the page file
  await renderHighlighted(
    sourcePath,
    pagePath,
  );

  return pageToPdf(new URL(`${sourcePath}.html`, 'http://example.com/source/').pathname.slice(1));
}


// Generate all the “component” PDFs we need

/** @type {Promise<string>[]} */
const pieces = [];
pieces.push(pageToPdf('index.html'));
pieces.push(pageToPdf('tutorial.html'));
pieces.push(pageToPdf('lrm.html'));
pieces.push(pageToPdf('project.html'));
pieces.push(pageToPdf('architecture.html'));
pieces.push(pageToPdf('testing.html'));

// TODO: test inputs/outputs

pieces.push(pageToPdf('lessons.html'));

// appendix: source code

const compilerSourcePaths = [
  'README.md',
  // Build system
  'Makefile',
  'dune-project',
  'src/dune',
  // OCaml sources (in “order” of compiler steps)
  'src/untangled.ml',
  'src/scanner.mll',
  'src/parser.mly',
  'src/ast.ml',
  'src/sast.ml',
  'src/semant.ml',
  'src/codegen.ml',
  // everything we missed
  ...await globby('src/**/*', { cwd: new URL('../../', import.meta.url), gitignore: true }),
].filter((p, i, arr) => arr.indexOf(p) === i); // remove duplicates
console.log('Rendering source code files: ', compilerSourcePaths);
pieces.push(...compilerSourcePaths.map((path) => codeToPdf(path)));


// Merge the “component” PDFs into the big final report

console.log(`Merging generated PDFs...`);
const mergeScript = spawn('python', [mergeScriptPath, ...await Promise.all(pieces), finalPdfPath]);
mergeScript.stdout.on('data', (data) => { stdout.write(data); }); // make Python script’s output visible for debugging
await new Promise((resolve, reject) => {
  mergeScript.on('exit', resolve);
  mergeScript.on('error', reject);
});



console.log('done!');

// Clean up server and browser

await browser.close();
previewServer.httpServer.close();
