import React from 'react';
import ReactDOM from 'react-dom/client';
import Code, { CodeConfig } from './src/components/Code';

const root = document.getElementById('root-container');
if (!root) throw new Error('React root element not found');

const codeToHighlight = document.getElementById('source')!.innerHTML;
const highlightLanguage = document.getElementById('language')!.innerHTML;
const codePath = document.getElementById('source-path')!.innerHTML;

ReactDOM.createRoot(root).render(
  <React.StrictMode>
    <CodeConfig theme="poimandres">
      <p>
        File: {codePath} (<a href={`https://github.com/untangled-lang/untangled/blob/main/${codePath}`}>view on GitHub</a>)
      </p>
      <pre>
        <Code highlight className={`language-${highlightLanguage}`}>
          {codeToHighlight}
        </Code>
      </pre>
    </CodeConfig>
  </React.StrictMode>
);
