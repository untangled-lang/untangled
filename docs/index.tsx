import React from 'react';
import ReactDOM from 'react-dom/client';
import Main from './src/main';
import Code, { CodeConfig, CodeWrapper } from './src/components/Code';
import Link from './src/components/Link';

const root = document.getElementById('root-container');
if (!root) throw new Error('React root element not found');

// Mount the MDX page
ReactDOM.createRoot(root).render(
  <React.StrictMode>
    <CodeConfig theme="poimandres">
      <Main components={{ code: Code, pre: CodeWrapper, a: Link }} />
    </CodeConfig>
  </React.StrictMode>
);
