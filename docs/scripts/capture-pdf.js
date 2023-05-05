import fs from 'fs/promises';
import { dirname } from 'path';
import { spawn } from 'child_process';
import { fileURLToPath } from 'url';
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
  base: process.env.DOCS_BASE_PATH ?? '/',
  preview: { port: 5174 },
});
const localServerBase = previewServer.resolvedUrls.local[0];
console.log('Started server');


const browser = await puppeteer.launch({ headless: 'new' });
console.log('Launched browser');


const inch = 96;
/**
 * Capture a webpage to a piece of the PDF
 * @param {string | URL} path The path on the website to capture
 */
async function pageToPdf(path) {
  console.log(`Capturing page ${path}...`);
  // Load the web page
  const page = await browser.newPage();
  const url = new URL(path, localServerBase);
  await page.goto(url.href);
  await page.waitForSelector('#root-container *');
  console.log(`Loaded ${path}`);

  // Set up the page
  await page.emulateMediaType('print');
  await new Promise((resolve) => setTimeout(resolve, 1000));
  const zoom = 0.66;
  // @ts-ignore
  await page.evaluate((z) => document.documentElement.style.zoom = z, zoom);

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
  console.log(`Capturing PDF for ${path}...`);
  const uncroppedPdfPath = getOutputPath(`${path}-uncropped.pdf`);
  const croppedPdfPath = getOutputPath(`${path}.pdf`);
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
  console.log(`Cropping PDF for ${path}...`);
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
  console.log(`Captured ${path}`);

  return croppedPdfPath
}


// Generate all the “component” PDFs we need

const items = [
  'tutorial.html',
  'lrm.html',
];
const componentPdfPaths = await Promise.all(items.map((path) => pageToPdf(path)));



/**
 * Capture a source code file to a PDF
 * @param {string | URL} path The path (from the repository root) to the source code file to capture
 */
async function codeToPdf(path) {
  const pagePath = fileURLToPath(new URL(`${path}.html`, new URL('../dist/source/', import.meta.url)));
  const sourcePath = new URL(path, new URL('../../', import.meta.url));
  await fs.mkdir(dirname(pagePath), { recursive: true });

  // Create the page file
  await renderHighlighted(
    sourcePath,
    pagePath,
    'ocaml', // TODO: be flexible
  );

  return pageToPdf(new URL(path, 'http://example.com/source/').pathname.slice(1));
}

// Generate PDFs of code


const a = await codeToPdf('src/ast.ml');


// Merge the “component” PDFs into the big final report

console.log(`Merging generated PDFs...`);
const mergeScript = spawn('python', [mergeScriptPath, ...componentPdfPaths, finalPdfPath]);
mergeScript.stdout.on('data', (data) => { stdout.write(data); }); // make Python script’s output visible for debugging
await new Promise((resolve, reject) => {
  mergeScript.on('exit', resolve);
  mergeScript.on('error', reject);
});



console.log('done!');

// Clean up server and browser

await browser.close();
previewServer.httpServer.close();
