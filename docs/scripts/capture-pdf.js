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
const previewServer = await preview({ preview: { port: 5174 } });

// Load the page
const browser = await puppeteer.launch({ headless: true });
const page = await browser.newPage();
await page.goto('http://localhost:5174');
await page.waitForSelector('h1');

// Capture PDF
// @ts-ignore
await page.evaluate(() => document.documentElement.style.zoom = 0.66);
const contentHeight = await page.evaluate(() => document.documentElement.scrollHeight);
await page.pdf({
  width: '8.5in',
  height: contentHeight + 96 * 1.5, // add extra padding to prevent wrapping
  printBackground: true,
  path: 'untangled.pdf',
});

// Clean up
await browser.close();
previewServer.httpServer.close();
