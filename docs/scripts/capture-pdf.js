import puppeteer from 'puppeteer';
import { preview } from 'vite';
import fs from 'fs/promises';
import { stdout } from 'process';

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
await page.goto(previewServer.resolvedUrls.local[0]);
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


// Clean up

await browser.close();
previewServer.httpServer.close();
