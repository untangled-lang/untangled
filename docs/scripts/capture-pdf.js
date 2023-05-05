import fs from 'fs/promises';
import { spawn } from 'child_process';
import { fileURLToPath } from 'url';
import { stdout } from 'process';

import puppeteer from 'puppeteer';
import { preview } from 'vite';


const cropScriptPath = fileURLToPath(new URL('./crop-pdf.py', import.meta.url));
const uncroppedPdfPath = fileURLToPath(new URL('../export/uncropped.pdf', import.meta.url));
const finalPdfPath = fileURLToPath(new URL('../export/manual.pdf', import.meta.url));


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
console.log('Started server');


// Load the page

const browser = await puppeteer.launch({ headless: 'new' });
const page = await browser.newPage();
const url = new URL('lrm.html', previewServer.resolvedUrls.local[0]);
await page.goto(url.href);
console.log('Launched browser');
await page.waitForSelector('h1');
console.log('Loaded page');


// Capture PDF

await page.emulateMediaType('print');
await new Promise((resolve) => setTimeout(resolve, 1000));
const zoom = 0.66;
// @ts-ignore
await page.evaluate((z) => document.documentElement.style.zoom = z, zoom);

const pageHeights = await page.evaluate((z) => {
  // Collapse all pages besides the current one
  const pages = [...document.getElementsByClassName('page')];
  // Return the height of the current page
  return pages.map((page) => Math.ceil(page.clientHeight * z) + 1)
}, zoom);


// Capture the page

stdout.write('Capturing pdf...');
const inch = 96;

await page.pdf({
  width: '8.5in',
  height: Math.max(...pageHeights) + (0.5 * inch), // add extra padding to prevent wrapping
  printBackground: true,
  path: 'export/uncropped.pdf',
});

stdout.write(' done\n');


// Clean up server and browser

await browser.close();
previewServer.httpServer.close();


// Call out to our Python script to crop the PDF

const cropScript = spawn('python', [cropScriptPath, uncroppedPdfPath, finalPdfPath]);
cropScript.stdin.write(JSON.stringify(pageHeights.map((ph) => ph / inch)));
cropScript.stdin.end();
cropScript.stdout.on('data', (data) => {
  stdout.write(data);
});
await new Promise((resolve, reject) => {
  cropScript.on('exit', resolve);
  cropScript.on('error', reject);
});


// Clean up

await fs.rm(uncroppedPdfPath);
