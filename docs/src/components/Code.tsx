import React, { useMemo, useContext, createContext } from 'react';
import { getHighlighter, HighlighterOptions } from 'shiki';
import parse from 'html-react-parser';



const UNTANGLED_LANGUAGE_DEFINITION = {
  id: 'untangled',
  scopeName: 'source.untangled',
  aliases: ['untangled'],
  path: '/languages/untangled.tmLanguage.json',
} as const;

const LOAD_LANGUAGES = ['ocaml', 'python', UNTANGLED_LANGUAGE_DEFINITION] as const;
const LOAD_THEMES = ['poimandres'] as const;


// Union types for the above
type LanguageName<T extends typeof LOAD_LANGUAGES[number]> = T extends object ? T['id'] : T;
export type Language = LanguageName<typeof LOAD_LANGUAGES[number]>;
export type Theme = typeof LOAD_THEMES[number];

// Create shiki highlighter instance (requires some messy type gymnastics to remove `readonly`)
type DeepReadonly<T> = T extends Function ? T : T extends object ? { readonly [K in keyof T]: DeepReadonly<T[K]> } : T;
const options: HighlighterOptions = { themes: LOAD_THEMES, langs: LOAD_LANGUAGES } satisfies DeepReadonly<HighlighterOptions> as any;
const highlighter = await getHighlighter(options);


/** Context for the Code component, which allows us to set the theme */
export const CodeContext = createContext<{ theme: Theme }>({ theme: 'poimandres' });
export const CodeConfig = CodeContext.Provider;


export type CodeProps = {
  children: string;
  className?: `language-${Language}`;
};
/** Renders code with syntax highlighting */
export default function Code({
  children: code,
  className = 'language-untangled',
}: CodeProps) {
  const language = className.replace(/^language-/, '') as Language;
  const { theme } = useContext(CodeContext);
  const html = useMemo(() => highlighter.codeToHtml(code, { lang: language, theme }), [code, language]);
  return <>{parse(html)}</>;
}


/** Renders a `pre` tag unless its only child is a Code component, in which case it renders the
 *  child directly. This is a utility to prevent double-rendering `pre` tags. */
export function CodeWrapper({ children, ...props }: { children: React.ReactNode } & React.HTMLAttributes<HTMLPreElement>) {
  if (React.Children.count(children) === 1) {
    const child = React.Children.only(children);
    if (child && typeof child === 'object' && 'type' in child && child.type === Code) {
      return children;
    }
  }
  return <pre {...props}>{children}</pre>;
}
