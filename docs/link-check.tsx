// Highlights links to non-existent elements in the document

const observer = new MutationObserver((mutations) => {
  const links = document.querySelectorAll('a');
  for (const link of links) {
    const href = link.getAttribute('href');
    if (href && href.startsWith('#')) {
      const referencedElement = document.getElementById(href.slice(1));
      if (!referencedElement) {
        console.warn(`Link to non-existent element: ${href}`);
        link.classList.add('invalid');
      }
    }
  }
});

observer.observe(document, {
  childList: true,
  subtree: true,
});

export {};
