import puppeteer from 'puppeteer';
import { preview } from 'vite';
import fs from 'fs/promises';

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
const browser = await puppeteer.launch({ headless: true });
const page = await browser.newPage();
await page.goto(previewServer.resolvedUrls.local[0]);
console.log('Launched browser');
await page.waitForSelector('h1');
console.log('Loaded page');


// Capture PDF

await page.emulateMediaType('print');
await new Promise((resolve) => setTimeout(resolve, 1000));
// @ts-ignore
await page.evaluate(() => document.documentElement.style.zoom = 0.66);
const contentHeight = await page.evaluate(() => document.documentElement.scrollHeight);
await page.pdf({
  width: '8.5in',
  height: contentHeight + 96 * 1, // add extra padding to prevent wrapping
  printBackground: true,
  path: 'untangled.pdf',
});

console.log('Captured PDF');

// Clean up
await browser.close();
previewServer.httpServer.close();
