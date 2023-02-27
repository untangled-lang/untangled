// Components for grammar rules
import React from 'react';


export function RuleBlock({ children }: { children: React.ReactNode }) {
  return (
    <div className="rule-block">
      {children}
    </div>
  );
}


export function Rule({
  name,
  children: casesNode,
  inlineCases = false,
}: {
  name: string;
  children: React.ReactNode,
  inlineCases?: boolean,
}) {
  const cases = React.Children.toArray(casesNode);
  return (
    <div className="rule-block">
      <div className="rule-name">
        <NonTerminal definition>{name}</NonTerminal>
      </div>
      <div className="rule-separator">::=</div>

      {
        !inlineCases ? (
          <>
            <div className="rule-body">{cases[0] ?? null}</div>

            {cases.slice(1).map((child, idx) => (
              <React.Fragment key={idx}>
                <div className="rule-separator">&#x01c0;</div>
                <div className="rule-body">{child}</div>
              </React.Fragment>
            ))}
          </>
        ) : (
          <div className="rule-body inline-cases">
            {cases.map((child, idx) => (
              <div key={idx}>
                {child}
                {idx < cases.length - 1 ? <span className="rule-separator">&#x01c0;</span> : null}
              </div>
            ))}
          </div>
        )
      }
    </div>
  );
}


function NonTerminal({
  definition = false,
  children,
  ...props
}: {
  definition?: boolean,
  children: string,
} & React.HTMLAttributes<HTMLAnchorElement>) {
  const nonterminalId = `nonterminal-${children.replace(/\s+/g, '-').toLowerCase()}`
  const linkingProps = definition ? { id: nonterminalId } : { href: `#${nonterminalId}` };
  return <a {...props} className="nonterminal" {...linkingProps}>{children}</a>;
}


function Terminal({
  children,
  ...props
}: {
  children: string,
} & React.HTMLAttributes<HTMLSpanElement>) {
  return <span {...props} className="terminal">{children}</span>;
}


// Enable “magic” syntax for terminals/nonterminals (e.g. <NT.expr />)

const reactInternalsBlacklist = [
  ...Object.getOwnPropertyNames(React.Component),
  ...Object.getOwnPropertyNames(NonTerminal),
  'childContextTypes', 'constructor', 'getDerivedStateFromError', 'getDerivedStateFromProps',
  'isReactComponent', 'propTypes', 'PropTypes', 'defaultProps', 'getDefaultProps', 'contextTypes',
];
const magicHandler = {
  get(Target: typeof NonTerminal | typeof Terminal, name: string) {
    name = ({
      semi: ';', comma: ',', dot: '.', lparen: '(', rparen: ')', lbrack: '[', rbrack: ']',
      lbrace: '{', rbrace: '}', arrow: '->',
    })[name] ?? name;
    if (typeof name === 'string' && reactInternalsBlacklist.includes(name)) return (Target as any)[name];
    return (props: any) => <Target {...props}>{name}</Target>;
  }
};
type ProxyType<T extends typeof Terminal | typeof NonTerminal> = T & {
  [key: string]: (props: Omit<Parameters<T>[0], 'children' | 'definition'>) => ReturnType<T>
};

const NonTerminalProxy = new Proxy(NonTerminal, magicHandler) as ProxyType<typeof NonTerminal>;
export { NonTerminalProxy as NonTerminal };
const TerminalProxy = new Proxy(Terminal, magicHandler) as ProxyType<typeof Terminal>;
export { TerminalProxy as Terminal };
