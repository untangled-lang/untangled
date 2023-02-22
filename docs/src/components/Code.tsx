import React, { useContext, createContext } from 'react';
import { FontStyle, getHighlighter, HighlighterOptions } from 'shiki';


const UNTANGLED_LANGUAGE_DEFINITION = {
  id: 'untangled',
  scopeName: 'source.untangled',
  aliases: ['untangled'],
  path: '/languages/untangled.tmLanguage.json',
} as const;

const LOAD_LANGUAGES = ['ocaml', 'python', UNTANGLED_LANGUAGE_DEFINITION] as const;
const LOAD_THEMES = ['poimandres', 'Overnight-Slumber-italic', 'Overnight-Slumber', 'nord', 'github-dark', 'github-dark-dimmed'] as const;

// Union types for languages/themes
type LanguageName<T extends typeof LOAD_LANGUAGES[number]> = T extends object ? T['id'] : T;
export type Language = LanguageName<typeof LOAD_LANGUAGES[number]>;
export type Theme = typeof LOAD_THEMES[number];

const DEFAULT_THEME = 'github-dark' satisfies Theme;



// Create shiki highlighter instance (requires some messy type gymnastics to remove `readonly`)
type DeepReadonly<T> = T extends Function ? T : T extends object ? { readonly [K in keyof T]: DeepReadonly<T[K]> } : T;
const options: HighlighterOptions = { themes: LOAD_THEMES, langs: LOAD_LANGUAGES } satisfies DeepReadonly<HighlighterOptions> as any;
const highlighter = await getHighlighter(options);


// Configuration context for the Code component, which allows us to set the theme
export const CodeConfigContext = createContext<{ theme: Theme }>({ theme: DEFAULT_THEME });
type ContextValue<T> = T extends React.Context<infer V> ? V : never;
export function CodeConfig({ children, ...props }: { children?: React.ReactNode } & ContextValue<typeof CodeConfigContext>) {
  return <CodeConfigContext.Provider value={props}>{children}</CodeConfigContext.Provider>;
}


/** Renders code with syntax highlighting */
export default function Code({
  children,
  className = 'language-untangled',
  highlight = true,
}: {
  children?: React.ReactNode;
  className?: string;
  highlight?: boolean;
}) {
  const language = className.replace(/^language-/, '') as Language;
  const { theme } = useContext(CodeConfigContext);
  // If highlight is disabled, or if children is *not* a single string, play dead.
  if (typeof children !== 'string' || !highlight) return <code>{children}</code>;
  // Highlight string child with shiki
  const lines = highlighter.codeToThemedTokens(children, language, theme);

  return (
    <code>
      {lines.map((tokens, lineIdx) => (
        <span key={lineIdx}>
          {/* Tokens in this line */}
          {
            tokens.map((token, tokenIdx) => (
              <span
                key={tokenIdx}
                style={{
                  color: token.color,
                  ...(token.fontStyle ?? 0) & FontStyle.Italic && { fontStyle: 'italic' },
                  ...(token.fontStyle ?? 0) & FontStyle.Bold && { fontWeight: 'bold' },
                  ...(token.fontStyle ?? 0) & FontStyle.Underline && { textDecoration: 'underline' },
                }}
                data-token={JSON.stringify(token)}
              >
                {token.content}
              </span>
            ))
          }
          {/* Newline between lines */}
          {lineIdx < lines.length - 1 ? '\n' : null}
        </span>
      ))}
    </code>
  );
}
