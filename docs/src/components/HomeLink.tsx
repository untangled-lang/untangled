import React from 'react';

const BASE = import.meta.env.DOCS_BASE_PATH ?? '/';

export default function HomeLink() {
  return (
    <a href={BASE} className="home-link no-print">
      Back
    </a>
  );
}
