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


export function NonTerminal({
  definition = false,
  children,
  ...props
}: {
  definition?: boolean,
  children: string,
} & React.HTMLAttributes<HTMLAnchorElement>) {
  const nonterminalId = `nonterminal-${children.replace(/\s+/g, '-').toLowerCase()}`
  const linkingProps = definition ? { id: nonterminalId } : { href: `#${nonterminalId}` };
  return <a className="nonterminal" {...linkingProps} {...props}>{children}</a>;
}


export function Terminal({
  children,
}: {
  children: string,
}) {
  return <span className="terminal">{children}</span>;
}
