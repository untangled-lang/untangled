import React from 'react';

export default function Link({
  href,
  children,
  ...props
}: {
  href?: string,
  children?: React.ReactNode
} & React.HTMLAttributes<HTMLAnchorElement>) {
  if (href && (href.startsWith('http://') || href.startsWith('https://'))) {
    return (
      <a href={href} target="_blank" rel="noopener noreferrer" {...props}>
        {children}
        <svg xmlns="http://www.w3.org/2000/svg" fill="currentColor" viewBox="0 0 256 256">
            <rect width="256" height="256" fill="none" />
            <line x1="64" y1="192" x2="192" y2="64" fill="none" stroke="currentColor" stroke-width="20" />
            <polyline points="88 64 192 64 192 168" fill="none" stroke="currentColor" stroke-width="20" />
          </svg>
      </a>
    );
  }
  if (href) return <a href={href} {...props}>{children}</a>;
  return <a {...props}>{children}</a>;
}
